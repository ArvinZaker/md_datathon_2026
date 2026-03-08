library(caret)
library(pROC)
library(ranger)
library(rpart)

set.seed(123)

if (!dir.exists("rf")) dir.create("rf", recursive = TRUE)

input_path <- "ml_inputs/train_all_cleaned2.csv"
validation_path <- "ml_inputs/external_all_cleaned2.csv"
model_output <- "rf/rf_caret_model.rds"
results_output <- "rf/rf_caret_cv_results.csv"
preds_output <- "rf/rf_caret_cv_predictions.csv"
validation_preds_output <- "rf/rf_validation_predictions.csv"
tree_model_output <- "rf/rf_decision_tree_model.rds"

# Keep this list simple and editable
categorical_vars <- c(
  "sex",
  "chest pain",
  "resting ECG",
  "exercise angina",
  "diagnosis"
)

# ---- START: chest pain indicator columns (remove this block if not needed) ----
use_chest_pain_dummies <- TRUE
# ---- END: chest pain indicator switch ----

# ---- START: resting ECG indicator columns (remove this block if not needed) ----
use_ecg_dummies <- TRUE
# ---- END: resting ECG indicator switch ----

df <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)
validation_df <- read.csv(validation_path, stringsAsFactors = FALSE, check.names = FALSE)

df <- df[, names(df) != "", drop = FALSE]
validation_df <- validation_df[, names(validation_df) != "", drop = FALSE]

if ("dataset" %in% names(df)) df$dataset <- NULL
if ("dataset" %in% names(validation_df)) validation_df$dataset <- NULL

for (nm in intersect(categorical_vars, names(df))) df[[nm]] <- as.factor(df[[nm]])
for (nm in intersect(categorical_vars, names(validation_df))) validation_df[[nm]] <- as.factor(validation_df[[nm]])

if (use_chest_pain_dummies && "chest pain" %in% names(df) && "chest pain" %in% names(validation_df)) {
  chest_map <- c(
    "typical angina" = "Typical Angina Chest Pain",
    "atypical angina" = "Atypical Angina Chest Pain",
    "non-anginal pain" = "Non Anginal Chest Pain",
    "asymptomatic" = "Asymptomatic Chest Pain"
  )

  train_cp <- as.character(df$`chest pain`)
  valid_cp <- as.character(validation_df$`chest pain`)

  for (lv in names(chest_map)) {
    new_col <- chest_map[[lv]]
    df[[new_col]] <- as.integer(train_cp == lv)
    validation_df[[new_col]] <- as.integer(valid_cp == lv)
  }

  df$`chest pain` <- NULL
  validation_df$`chest pain` <- NULL
}

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

x_train <- as.data.frame(model.matrix(diagnosis ~ . - 1, data = df))
x_valid <- as.data.frame(model.matrix(diagnosis ~ . - 1, data = validation_df))

missing_in_valid <- setdiff(names(x_train), names(x_valid))
if (length(missing_in_valid) > 0) for (nm in missing_in_valid) x_valid[[nm]] <- 0

extra_in_valid <- setdiff(names(x_valid), names(x_train))
if (length(extra_in_valid) > 0) x_valid <- x_valid[, setdiff(names(x_valid), extra_in_valid), drop = FALSE]

x_valid <- x_valid[, names(x_train), drop = FALSE]

rf_fit <- train(
  x = x_train,
  y = df$diagnosis,
  method = "ranger",
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE,
    savePredictions = "final",
    classProbs = TRUE
  ),
  tuneLength = 5,
  max.depth = 4,
  importance = "impurity",
  metric = "Accuracy"
)

write.csv(rf_fit$results, results_output, row.names = FALSE)
write.csv(rf_fit$pred, preds_output, row.names = FALSE)
saveRDS(rf_fit, model_output)

pos_class <- levels(df$diagnosis)[2]
neg_class <- levels(df$diagnosis)[1]

prob_valid <- predict(rf_fit, newdata = x_valid, type = "prob")
validation_pred <- predict(rf_fit, newdata = x_valid, type = "raw")
validation_prob <- prob_valid[[pos_class]]

validation_out <- data.frame(
  obs = validation_df$diagnosis,
  pred = validation_pred,
  prob_positive = validation_prob
)
write.csv(validation_out, validation_preds_output, row.names = FALSE)

dt_fit <- rpart(
  diagnosis ~ .,
  data = df,
  method = "class",
  control = rpart.control(cp = 0.005, maxdepth = 4, minsplit = 20)
)
saveRDS(dt_fit, tree_model_output)

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
cat("Saved:", model_output, "\n")
cat("Saved:", validation_preds_output, "\n")
cat("Saved:", tree_model_output, "\n")
cat("Validation ROC AUC:", sprintf("%.4f", auc_val), "\n")
