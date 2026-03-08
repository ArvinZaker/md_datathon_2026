library(caret)
library(pROC)
set.seed(456)

# caret_method <- "xgbTree"
caret_method <- "glmnet"
output_dir <- "glmnet"

metric_name <- "Accuracy"
tune_length <- 10
tune_grid <- NULL

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

input_path <- "ml_inputs/train_all_cleaned2.csv"
validation_path <- "ml_inputs/external_all_cleaned2.csv"

model_output <- file.path(output_dir, "glmnet_caret_model.rds")
final_model_output <- file.path(output_dir, "glmnet_final_model.rds")
results_output <- file.path(output_dir, "glmnet_caret_cv_results.csv")
preds_output <- file.path(output_dir, "glmnet_caret_cv_predictions.csv")
train_preds_output <- file.path(output_dir, "glmnet_training_predictions.csv")
validation_preds_output <- file.path(output_dir, "glmnet_validation_predictions.csv")
feature_schema_output <- file.path(output_dir, "glmnet_feature_schema.csv")
artifact_output <- file.path(output_dir, "glmnet_artifacts.rds")
roc_summary_output <- file.path(output_dir, "glmnet_roc_summary.csv")
roc_fold_metrics_output <- file.path(output_dir, "glmnet_roc_fold_metrics.csv")
cv_train_roc_plot_path <- file.path(output_dir, "glmnet_cv_training_roc.png")
cv_test_roc_plot_path <- file.path(output_dir, "glmnet_cv_testing_roc.png")
validation_roc_plot_path <- file.path(output_dir, "glmnet_validation_roc.png")
cv_train_roc_curve_path <- file.path(output_dir, "glmnet_cv_training_roc_curve.csv")
cv_test_roc_curve_path <- file.path(output_dir, "glmnet_cv_testing_roc_curve.csv")
validation_roc_curve_path <- file.path(output_dir, "glmnet_validation_roc_curve.csv")

normalize_text <- function(x) {
  tolower(trimws(as.character(x)))
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

roc_to_df <- function(roc_obj, split_name, fold_name = "overall") {
  data.frame(
    split = split_name,
    fold = fold_name,
    threshold = as.numeric(roc_obj$thresholds),
    specificity = as.numeric(roc_obj$specificities),
    sensitivity = as.numeric(roc_obj$sensitivities),
    fpr = 1 - as.numeric(roc_obj$specificities),
    tpr = as.numeric(roc_obj$sensitivities),
    stringsAsFactors = FALSE
  )
}

build_feature_spec <- function(train_df) {
  sex_levels_display <- sort(unique(trimws(as.character(train_df$sex))))
  sex_levels_norm <- normalize_text(sex_levels_display)
  male_label <- if ("male" %in% sex_levels_norm) "male" else tail(sex_levels_norm, 1)

  fasting_levels_display <- sort(unique(trimws(as.character(train_df$`fasting sugar`))))
  fasting_levels_norm <- normalize_text(fasting_levels_display)
  high_candidates <- fasting_levels_norm[grepl(">", fasting_levels_norm, fixed = TRUE)]
  high_label <- if (length(high_candidates) > 0) high_candidates[1] else tail(fasting_levels_norm, 1)

  angina_levels_display <- sort(unique(trimws(as.character(train_df$`exercise angina`))))
  angina_levels_norm <- normalize_text(angina_levels_display)
  angina_yes_label <- if ("yes" %in% angina_levels_norm) "yes" else tail(angina_levels_norm, 1)

  chest_levels_display <- c("typical angina", "atypical angina", "non-anginal pain", "asymptomatic")
  chest_feature_map <- c(
    "typical angina" = "chest_pain_typical_angina",
    "atypical angina" = "chest_pain_atypical_angina",
    "non-anginal pain" = "chest_pain_non_anginal_pain",
    "asymptomatic" = "chest_pain_asymptomatic"
  )

  ecg_levels_display <- c("normal", "ST-T wave abnormality", "left ventricular hypertrophy")
  ecg_feature_map <- c(
    "normal" = "resting_ecg_normal",
    "st-t wave abnormality" = "resting_ecg_st_t_wave_abnormality",
    "left ventricular hypertrophy" = "resting_ecg_left_ventricular_hypertrophy"
  )

  feature_map <- list(
    age = "age",
    sex_male = "sex_male",
    resting_bp = "resting_bp",
    cholestoral = "cholestoral",
    fasting_sugar_high = "fasting_sugar_high",
    max_hr = "max_hr",
    exercise_angina_yes = "exercise_angina_yes",
    exercise_st_depression = "exercise_st_depression",
    chest_pain = chest_feature_map,
    resting_ecg = ecg_feature_map
  )

  ordered_features <- c(
    "age",
    "sex_male",
    "resting_bp",
    "cholestoral",
    "fasting_sugar_high",
    "max_hr",
    "exercise_angina_yes",
    "exercise_st_depression",
    unname(chest_feature_map),
    unname(ecg_feature_map)
  )

  list(
    input_spec = list(
      sex = list(levels = sex_levels_display, male_label = male_label),
      fasting_sugar = list(levels = fasting_levels_display, high_label = high_label),
      chest_pain = list(levels = chest_levels_display),
      resting_ecg = list(levels = ecg_levels_display),
      exercise_angina = list(levels = angina_levels_display, yes_label = angina_yes_label)
    ),
    feature_map = feature_map,
    ordered_features = ordered_features
  )
}

prepare_feature_frame <- function(df, feature_spec) {
  out <- data.frame(
    age = safe_numeric(df$age),
    resting_bp = safe_numeric(df$`resting bp`),
    cholestoral = safe_numeric(df$cholestoral),
    max_hr = safe_numeric(df$`max HR`),
    stringsAsFactors = FALSE
  )

  sex_chr <- normalize_text(df$sex)
  fasting_chr <- normalize_text(df$`fasting sugar`)
  angina_chr <- normalize_text(df$`exercise angina`)
  st_depression_num <- safe_numeric(df$`exercise ST depression`)
  chest_chr <- normalize_text(df$`chest pain`)
  ecg_chr <- normalize_text(df$`resting ECG`)

  out$sex_male <- as.integer(sex_chr == feature_spec$input_spec$sex$male_label)
  out$fasting_sugar_high <- as.integer(fasting_chr == feature_spec$input_spec$fasting_sugar$high_label)
  out$exercise_angina_yes <- as.integer(angina_chr == feature_spec$input_spec$exercise_angina$yes_label)
  out$exercise_st_depression <- st_depression_num

  chest_map <- feature_spec$feature_map$chest_pain
  for (lv in names(chest_map)) {
    out[[chest_map[[lv]]]] <- as.integer(chest_chr == lv)
  }

  ecg_map <- feature_spec$feature_map$resting_ecg
  for (lv in names(ecg_map)) {
    out[[ecg_map[[lv]]]] <- as.integer(ecg_chr == lv)
  }

  for (nm in feature_spec$ordered_features) {
    if (!nm %in% names(out)) out[[nm]] <- 0
  }
  out <- out[, feature_spec$ordered_features, drop = FALSE]

  for (nm in names(out)) {
    out[[nm]] <- safe_numeric(out[[nm]])
    bad <- is.na(out[[nm]]) | !is.finite(out[[nm]])
    if (any(bad)) out[[nm]][bad] <- NA_real_
  }

  out
}

df <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)
validation_df <- read.csv(validation_path, stringsAsFactors = FALSE, check.names = FALSE)

df <- df[, names(df) != "", drop = FALSE]
validation_df <- validation_df[, names(validation_df) != "", drop = FALSE]

if ("dataset" %in% names(df)) df$dataset <- NULL
if ("dataset" %in% names(validation_df)) validation_df$dataset <- NULL

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
validation_df$diagnosis <- factor(validation_df$diagnosis, levels = levels(df$diagnosis))

feature_spec <- build_feature_spec(df)
x_train <- prepare_feature_frame(df, feature_spec)
x_valid <- prepare_feature_frame(validation_df, feature_spec)

invalid_train <- !is.finite(as.matrix(x_train)) | is.na(as.matrix(x_train))
if (any(invalid_train)) {
  bad_cols <- names(x_train)[apply(invalid_train, 2, any)]
  stop(
    sprintf(
      "Found NA/non-finite values in training features: %s",
      paste(bad_cols, collapse = ", ")
    )
  )
}

invalid_valid <- !is.finite(as.matrix(x_valid)) | is.na(as.matrix(x_valid))
if (any(invalid_valid)) {
  bad_cols <- names(x_valid)[apply(invalid_valid, 2, any)]
  stop(
    sprintf(
      "Found NA/non-finite values in validation features: %s",
      paste(bad_cols, collapse = ", ")
    )
  )
}

feature_medians <- vapply(x_train, stats::median, numeric(1), na.rm = TRUE)

feature_schema <- data.frame(
  feature = names(x_train),
  mean = as.numeric(colMeans(x_train)),
  median = as.numeric(feature_medians[names(x_train)]),
  min = as.numeric(vapply(x_train, min, numeric(1))),
  max = as.numeric(vapply(x_train, max, numeric(1))),
  stringsAsFactors = FALSE
)
write.csv(feature_schema, feature_schema_output, row.names = FALSE)

pos_class <- levels(df$diagnosis)[2]
neg_class <- levels(df$diagnosis)[1]

glm_model_p_value <- NA_real_
try({
  analysis_df <- data.frame(
    diagnosis = df$diagnosis,
    x_train,
    check.names = FALSE
  )
  glm_null <- stats::glm(diagnosis ~ 1, data = analysis_df, family = stats::binomial())
  glm_full <- suppressWarnings(stats::glm(diagnosis ~ ., data = analysis_df, family = stats::binomial()))
  lrt <- stats::anova(glm_null, glm_full, test = "Chisq")
  candidate_p <- suppressWarnings(as.numeric(lrt$`Pr(>Chi)`[2]))

  if (!is.finite(candidate_p)) {
    dev_diff <- glm_null$deviance - glm_full$deviance
    df_diff <- glm_null$df.residual - glm_full$df.residual
    if (is.finite(dev_diff) && is.finite(df_diff) && df_diff > 0) {
      candidate_p <- stats::pchisq(dev_diff, df = df_diff, lower.tail = FALSE)
    }
  }

  if (is.finite(candidate_p)) glm_model_p_value <- candidate_p
}, silent = TRUE)

ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  savePredictions = "final",
  classProbs = TRUE
)

train_args <- list(
  x = x_train,
  y = df$diagnosis,
  method = caret_method,
  trControl = ctrl,
  metric = metric_name,
  tuneLength = tune_length
)
if (!is.null(tune_grid)) train_args$tuneGrid <- tune_grid

model_fit <- do.call(train, train_args)

cv_preds <- model_fit$pred
for (param in names(model_fit$bestTune)) {
  if (param %in% names(cv_preds)) {
    cv_preds <- cv_preds[cv_preds[[param]] == model_fit$bestTune[[param]], , drop = FALSE]
  }
}
cv_preds$obs <- factor(cv_preds$obs, levels = levels(df$diagnosis))
cv_preds$pred <- factor(cv_preds$pred, levels = levels(df$diagnosis))

write.csv(model_fit$results, results_output, row.names = FALSE)
write.csv(cv_preds, preds_output, row.names = FALSE)
saveRDS(model_fit, model_output)
saveRDS(model_fit$finalModel, final_model_output)

prob_train <- predict(model_fit, newdata = x_train, type = "prob")
train_pred <- predict(model_fit, newdata = x_train, type = "raw")
train_out <- data.frame(
  obs = df$diagnosis,
  pred = train_pred,
  prob_positive = prob_train[[pos_class]],
  rowIndex = seq_len(nrow(df))
)
write.csv(train_out, train_preds_output, row.names = FALSE)

validation_pred <- predict(model_fit, newdata = x_valid, type = "raw")
prob_valid <- predict(model_fit, newdata = x_valid, type = "prob")
validation_prob <- prob_valid[[pos_class]]

validation_out <- data.frame(
  obs = validation_df$diagnosis,
  pred = validation_pred,
  prob_positive = validation_prob
)
write.csv(validation_out, validation_preds_output, row.names = FALSE)

roc_cv_train <- roc(
  response = train_out$obs,
  predictor = train_out$prob_positive,
  levels = c(neg_class, pos_class),
  quiet = TRUE
)
roc_cv_test <- roc(
  response = cv_preds$obs,
  predictor = cv_preds[[pos_class]],
  levels = c(neg_class, pos_class),
  quiet = TRUE
)
roc_validation <- roc(
  response = validation_out$obs,
  predictor = validation_out$prob_positive,
  levels = c(neg_class, pos_class),
  quiet = TRUE
)

auc_cv_train <- as.numeric(auc(roc_cv_train))
auc_cv_test <- as.numeric(auc(roc_cv_test))
auc_validation <- as.numeric(auc(roc_validation))

fold_ids <- sort(unique(cv_preds$Resample))
cv_train_fold_rocs <- list()
cv_test_fold_rocs <- list()
cv_train_fold_aucs <- numeric(length(fold_ids))
cv_test_fold_aucs <- numeric(length(fold_ids))
names(cv_train_fold_aucs) <- fold_ids
names(cv_test_fold_aucs) <- fold_ids

for (fold_id in fold_ids) {
  fold_test_ids <- unique(cv_preds$rowIndex[cv_preds$Resample == fold_id])
  fold_test_pos <- which(train_out$rowIndex %in% fold_test_ids)
  fold_train_pos <- setdiff(seq_len(nrow(train_out)), fold_test_pos)

  roc_train_fold <- roc(
    response = train_out$obs[fold_train_pos],
    predictor = train_out$prob_positive[fold_train_pos],
    levels = c(neg_class, pos_class),
    quiet = TRUE
  )
  cv_train_fold_rocs[[fold_id]] <- roc_train_fold
  cv_train_fold_aucs[fold_id] <- as.numeric(auc(roc_train_fold))

  fold_cv_test <- cv_preds[cv_preds$Resample == fold_id, , drop = FALSE]
  roc_test_fold <- roc(
    response = fold_cv_test$obs,
    predictor = fold_cv_test[[pos_class]],
    levels = c(neg_class, pos_class),
    quiet = TRUE
  )
  cv_test_fold_rocs[[fold_id]] <- roc_test_fold
  cv_test_fold_aucs[fold_id] <- as.numeric(auc(roc_test_fold))
}

fold_cols <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51")

png(cv_train_roc_plot_path, width = 7, height = 7, units = "in", res = 300)
par(xaxs = "i", yaxs = "i")
plot(roc_cv_train, main = "5-Fold CV Training ROC (glmnet)", col = "#2a9d8f", lwd = 3, xlim = c(1.02, -0.02), ylim = c(-0.02, 1.02))
for (i in seq_along(fold_ids)) {
  plot(cv_train_fold_rocs[[fold_ids[i]]], add = TRUE, col = fold_cols[i], lwd = 2.4)
}
abline(a = 0, b = 1, lty = 2, col = "gray50")
legend(
  "bottomright",
  legend = sprintf("%s AUC=%.3f", fold_ids, cv_train_fold_aucs[fold_ids]),
  col = fold_cols[seq_along(fold_ids)],
  lwd = 2.4,
  cex = 0.75,
  bty = "n"
)
dev.off()

png(cv_test_roc_plot_path, width = 7, height = 7, units = "in", res = 300)
par(xaxs = "i", yaxs = "i")
plot(roc_cv_test, main = "5-Fold CV Testing ROC (glmnet)", col = "#457b9d", lwd = 3, xlim = c(1.02, -0.02), ylim = c(-0.02, 1.02))
for (i in seq_along(fold_ids)) {
  plot(cv_test_fold_rocs[[fold_ids[i]]], add = TRUE, col = fold_cols[i], lwd = 2.4)
}
abline(a = 0, b = 1, lty = 2, col = "gray50")
legend(
  "bottomright",
  legend = sprintf("%s AUC=%.3f", fold_ids, cv_test_fold_aucs[fold_ids]),
  col = fold_cols[seq_along(fold_ids)],
  lwd = 2.4,
  cex = 0.75,
  bty = "n"
)
dev.off()

png(validation_roc_plot_path, width = 7, height = 7, units = "in", res = 300)
par(xaxs = "i", yaxs = "i")
plot(roc_validation, main = "External Validation ROC (glmnet)", col = "#d94b5f", lwd = 3, xlim = c(1.02, -0.02), ylim = c(-0.02, 1.02))
abline(a = 0, b = 1, lty = 2, col = "gray50")
text(0.22, 0.08, labels = sprintf("AUC = %.3f", auc_validation), cex = 1.0, font = 2)
dev.off()

cv_train_roc_df <- roc_to_df(roc_cv_train, split_name = "training", fold_name = "overall")
cv_test_roc_df <- roc_to_df(roc_cv_test, split_name = "testing", fold_name = "overall")
validation_roc_df <- roc_to_df(roc_validation, split_name = "validation", fold_name = "overall")

for (fold_id in fold_ids) {
  cv_train_roc_df <- rbind(cv_train_roc_df, roc_to_df(cv_train_fold_rocs[[fold_id]], split_name = "training", fold_name = fold_id))
  cv_test_roc_df <- rbind(cv_test_roc_df, roc_to_df(cv_test_fold_rocs[[fold_id]], split_name = "testing", fold_name = fold_id))
}

write.csv(cv_train_roc_df, cv_train_roc_curve_path, row.names = FALSE)
write.csv(cv_test_roc_df, cv_test_roc_curve_path, row.names = FALSE)
write.csv(validation_roc_df, validation_roc_curve_path, row.names = FALSE)

roc_summary <- data.frame(
  split = c("training", "testing", "validation"),
  auc = c(auc_cv_train, auc_cv_test, auc_validation),
  stringsAsFactors = FALSE
)
write.csv(roc_summary, roc_summary_output, row.names = FALSE)

roc_fold_metrics <- data.frame(
  fold = fold_ids,
  training_auc = as.numeric(cv_train_fold_aucs[fold_ids]),
  testing_auc = as.numeric(cv_test_fold_aucs[fold_ids]),
  stringsAsFactors = FALSE
)
write.csv(roc_fold_metrics, roc_fold_metrics_output, row.names = FALSE)

artifacts <- list(
  feature_names = names(x_train),
  feature_means = setNames(as.numeric(colMeans(x_train)), names(x_train)),
  feature_medians = setNames(as.numeric(feature_medians[names(x_train)]), names(x_train)),
  class_levels = levels(df$diagnosis),
  pos_class = pos_class,
  neg_class = neg_class,
  model_p_value = glm_model_p_value,
  best_tune = model_fit$bestTune,
  tune_metric = metric_name,
  input_spec = feature_spec$input_spec,
  feature_map = feature_spec$feature_map
)
saveRDS(artifacts, artifact_output)

cat("Training complete\n")
cat("Method:", caret_method, "\n")
cat("Saved:", results_output, "\n")
cat("Saved:", preds_output, "\n")
cat("Saved:", train_preds_output, "\n")
cat("Saved:", validation_preds_output, "\n")
cat("Saved:", model_output, "\n")
cat("Saved:", final_model_output, "\n")
cat("Saved:", feature_schema_output, "\n")
cat("Saved:", artifact_output, "\n")
cat("Saved:", cv_train_roc_plot_path, "\n")
cat("Saved:", cv_test_roc_plot_path, "\n")
cat("Saved:", validation_roc_plot_path, "\n")
cat("Saved:", cv_train_roc_curve_path, "\n")
cat("Saved:", cv_test_roc_curve_path, "\n")
cat("Saved:", validation_roc_curve_path, "\n")
cat("Saved:", roc_summary_output, "\n")
cat("Saved:", roc_fold_metrics_output, "\n")
if (is.finite(glm_model_p_value)) {
  cat("Model p-value:", sprintf("%.6g", glm_model_p_value), "\n")
}
cat("Training ROC AUC:", sprintf("%.4f", auc_cv_train), "\n")
cat("Testing ROC AUC:", sprintf("%.4f", auc_cv_test), "\n")
cat("Validation ROC AUC:", sprintf("%.4f", auc_validation), "\n")
