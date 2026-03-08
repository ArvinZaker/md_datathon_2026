library(pROC)
library(rpart)

set.seed(123)

if (!dir.exists("rpart")) dir.create("rpart", recursive = TRUE)

input_path <- "ml_inputs/train_all_cleaned2.csv"
validation_path <- "ml_inputs/external_all_cleaned2.csv"
model_output <- "rpart/rpart_caret_model.rds"
results_output <- "rpart/rpart_caret_cv_results.csv"
preds_output <- "rpart/rpart_caret_cv_predictions.csv"
train_preds_output <- "rpart/rpart_training_predictions.csv"
validation_preds_output <- "rpart/rpart_validation_predictions.csv"


# Keep this list simple and editable
categorical_vars <- c(
  "sex",
  "chest pain",
  "resting ECG",
  "exercise angina",
  "diagnosis"
)



df <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)
validation_df <- read.csv(validation_path, stringsAsFactors = FALSE, check.names = FALSE)

df <- df[, names(df) != "", drop = FALSE]
validation_df <- validation_df[, names(validation_df) != "", drop = FALSE]

if ("dataset" %in% names(df)) df$dataset <- NULL
if ("dataset" %in% names(validation_df)) validation_df$dataset <- NULL

for (nm in intersect(categorical_vars, names(df))) df[[nm]] <- as.factor(df[[nm]])
for (nm in intersect(categorical_vars, names(validation_df))) validation_df[[nm]] <- as.factor(validation_df[[nm]])

# ---- START: chest pain indicator columns (remove this block if not needed) ----
use_chest_pain_dummies <- TRUE
if (use_chest_pain_dummies && "chest pain" %in% names(df) && "chest pain" %in% names(validation_df)) {
  chest_map <- c(
    "typical angina" = "Typical_Angina_Chest_Pain",
    "atypical angina" = "Atypical_Angina_Chest_Pain",
    "non-anginal pain" = "Non_Anginal_Chest_Pain",
    "asymptomatic" = "Asymptomatic_Chest_Pain"
  )

  train_cp <- as.character(df$`chest pain`)
  valid_cp <- as.character(validation_df$`chest pain`)

  for (lv in names(chest_map)) {
    new_col <- chest_map[[lv]]
    if (new_col == "Asymptomatic_Chest_Pain") {
      df[[new_col]] <- factor(ifelse(train_cp == lv, "Yes", "No"), levels = c("No", "Yes"))
      validation_df[[new_col]] <- factor(ifelse(valid_cp == lv, "Yes", "No"), levels = c("No", "Yes"))
    } else {
      df[[new_col]] <- as.integer(train_cp == lv)
      validation_df[[new_col]] <- as.integer(valid_cp == lv)
    }
  }

  df$`chest pain` <- NULL
  validation_df$`chest pain` <- NULL
}
# ---- END: chest pain indicator switch ----

# ---- START: resting ECG indicator columns (remove this block if not needed) ----
use_ecg_dummies <- TRUE
if (use_ecg_dummies && "resting ECG" %in% names(df) && "resting ECG" %in% names(validation_df)) {
  ecg_map <- c(
    "normal" = "isECGNormal",
    "ST-T wave abnormality" = "isECGSTTWaveAbnormality",
    "left ventricular hypertrophy" = "isECGLVHypertrophy"
  )

  train_ecg <- as.character(df$`resting ECG`)
  valid_ecg <- as.character(validation_df$`resting ECG`)

  for (lv in names(ecg_map)) {
    new_col <- ecg_map[[lv]]
    df[[new_col]] <- as.integer(train_ecg == lv)
    validation_df[[new_col]] <- as.integer(valid_ecg == lv)
  }

  df$`resting ECG` <- NULL
  validation_df$`resting ECG` <- NULL
}
# ---- END: resting ECG indicator switch ----

if (all(c("<50% narrowing", ">50% narrowing") %in% unique(df$diagnosis))) {
  df$diagnosis <- factor(
    df$diagnosis,
    levels = c("<50% narrowing", ">50% narrowing"),
    labels = c("No_Narrowing", "Has_Narrowing")
  )
  validation_df$diagnosis <- factor(
    validation_df$diagnosis,
    levels = c("<50% narrowing", ">50% narrowing"),
    labels = c("No_Narrowing", "Has_Narrowing")
  )
} else {
  lv <- sort(unique(as.character(df$diagnosis)))
  safe_lv <- make.names(lv)
  df$diagnosis <- factor(df$diagnosis, levels = lv, labels = safe_lv)
  validation_df$diagnosis <- factor(validation_df$diagnosis, levels = lv, labels = safe_lv)
}

for (nm in setdiff(names(df), "diagnosis")) {
  if (is.factor(df[[nm]])) {
    validation_df[[nm]] <- factor(validation_df[[nm]], levels = levels(df[[nm]]))
  }
}
validation_df$diagnosis <- factor(validation_df$diagnosis, levels = levels(df$diagnosis))


pos_class <- levels(df$diagnosis)[2]
neg_class <- levels(df$diagnosis)[1]

make_stratified_folds <- function(y, k = 5) {
  fold_id <- integer(length(y))
  y_chr <- as.character(y)
  for (cls in unique(y_chr)) {
    idx <- which(y_chr == cls)
    idx <- sample(idx, length(idx))
    fold_id[idx] <- rep(seq_len(k), length.out = length(idx))
  }
  fold_id
}

fold_id <- make_stratified_folds(df$diagnosis, k = 5)
cv_pred_list <- vector("list", 5)
fold_acc <- numeric(5)
fold_auc <- numeric(5)

for (fold in seq_len(5)) {
  train_idx <- which(fold_id != fold)
  test_idx <- which(fold_id == fold)

  fold_fit <- rpart(
    diagnosis ~ .,
    data = df[train_idx, , drop = FALSE],
    method = "class",
    control = rpart.control(maxdepth = 4)
  )

  fold_prob <- predict(fold_fit, newdata = df[test_idx, , drop = FALSE], type = "prob")
  fold_prob_pos <- fold_prob[, pos_class]
  fold_pred <- factor(
    colnames(fold_prob)[max.col(fold_prob, ties.method = "first")],
    levels = levels(df$diagnosis)
  )

  fold_obs <- df$diagnosis[test_idx]
  fold_acc[fold] <- mean(fold_pred == fold_obs)
  fold_auc[fold] <- as.numeric(auc(roc(fold_obs, fold_prob_pos, levels = c(neg_class, pos_class), quiet = TRUE)))

  cv_pred_list[[fold]] <- data.frame(
    obs = fold_obs,
    pred = fold_pred,
    rowIndex = test_idx,
    Resample = paste0("Fold", fold),
    stringsAsFactors = FALSE
  )
  cv_pred_list[[fold]][[neg_class]] <- fold_prob[, neg_class]
  cv_pred_list[[fold]][[pos_class]] <- fold_prob[, pos_class]
}

cv_preds <- do.call(rbind, cv_pred_list)
cv_preds$obs <- factor(cv_preds$obs, levels = levels(df$diagnosis))
cv_preds$pred <- factor(cv_preds$pred, levels = levels(df$diagnosis))

cv_results <- data.frame(
  model = "rpart_manual_5fold",
  maxdepth = 4,
  Accuracy = mean(fold_acc),
  AccuracySD = stats::sd(fold_acc),
  ROC = mean(fold_auc),
  ROCSD = stats::sd(fold_auc)
)

rpart_fit <- rpart(
  diagnosis ~ .,
  data = df,
  method = "class",
  control = rpart.control(maxdepth = 4)
)

write.csv(cv_results, results_output, row.names = FALSE)
write.csv(cv_preds, preds_output, row.names = FALSE)
saveRDS(rpart_fit, model_output)

prob_train <- predict(rpart_fit, newdata = df, type = "prob")
train_out <- data.frame(
  obs = df$diagnosis,
  prob_positive = prob_train[, pos_class],
  rowIndex = seq_len(nrow(df))
)
write.csv(train_out, train_preds_output, row.names = FALSE)

prob_valid <- predict(rpart_fit, newdata = validation_df, type = "prob")
validation_pred <- factor(
  colnames(prob_valid)[max.col(prob_valid, ties.method = "first")],
  levels = levels(df$diagnosis)
)
validation_prob <- prob_valid[, pos_class]

validation_out <- data.frame(
  obs = validation_df$diagnosis,
  pred = validation_pred,
  prob_positive = validation_prob
)
write.csv(validation_out, validation_preds_output, row.names = FALSE)

roc_obj <- roc(
  response = validation_df$diagnosis,
  predictor = validation_prob,
  levels = c(neg_class, pos_class),
  quiet = TRUE
)
auc_val <- as.numeric(auc(roc_obj))

cat("Training complete\n")
cat("Saved:", results_output, "\n")
cat("Saved:", preds_output, "\n")
cat("Saved:", train_preds_output, "\n")
cat("Saved:", model_output, "\n")
cat("Saved:", validation_preds_output, "\n")
cat("Validation ROC AUC:", sprintf("%.4f", auc_val), "\n")
