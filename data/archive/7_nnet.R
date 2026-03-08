library(neuralnet)
library(foreach)
library(doParallel)
library(pROC)

set.seed(123)

# ---- Input / output ----
input_path <- "ml_inputs/train_all_cleaned2.csv"
validation_path <- "ml_inputs/external_all_cleaned2.csv"
# input_path <- "ml_inputs/train_cleavland_cleaned.csv"
# validation_path <- "ml_inputs/external_cleavland_cleaned.csv"
output_dir <- "nnet"

model_output <- file.path(output_dir, "neuralnet_model.rds")
validation_preds_output <- file.path(output_dir, "neuralnet_validation_predictions.csv")
metrics_output <- file.path(output_dir, "neuralnet_validation_metrics.csv")
architecture_study_output <- file.path(output_dir, "neuralnet_architecture_study.csv")
structure_plot_output <- file.path(output_dir, "neuralnet_structure.png")

# ---- Neuralnet main run parameters (single run) ----
hidden_layers <- c(8)
algorithm <- "rprop+"
act_fct_name <- "relu"
act_fct <- function(x) ifelse(x > 0, x, 0)
environment(act_fct) <- baseenv()
err_fct <- "sse"
linear_output <- FALSE
stepmax <- 1e6
threshold <- 0.01
replications <- 5
lifesign <- "minimal"
lifesign_step <- 1000
classification_threshold <- 0.5
num_cores <- 23

# ---- Optional architecture study ----
run_architecture_study <- FALSE
architecture_grid <- list(
  c(2),
  c(4),
  c(8),
  c(4, 2),
  c(8, 4),
  c(8, 4, 2)
)

# ---- Feature engineering toggles ----
categorical_vars <- c(
  "sex",
  "chest pain",
  "resting ECG",
  "exercise angina",
  "diagnosis"
)
use_chest_pain_dummies <- FALSE
use_ecg_dummies <- FALSE

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
doParallel::registerDoParallel(cores = num_cores)

df <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)
validation_df <- read.csv(validation_path, stringsAsFactors = FALSE, check.names = FALSE)

df <- df[, names(df) != "", drop = FALSE]
validation_df <- validation_df[, names(validation_df) != "", drop = FALSE]
df$dataset <- NULL
validation_df$dataset <- NULL

for (nm in categorical_vars) df[[nm]] <- as.factor(df[[nm]])
for (nm in categorical_vars) validation_df[[nm]] <- as.factor(validation_df[[nm]])


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

x_train <- as.data.frame(model.matrix(diagnosis ~ . - 1, data = df))
x_valid <- as.data.frame(model.matrix(diagnosis ~ . - 1, data = validation_df))
x_valid <- x_valid[, names(x_train), drop = FALSE]

safe_feature_names <- make.names(names(x_train), unique = TRUE)
names(x_train) <- safe_feature_names
names(x_valid) <- safe_feature_names

x_center <- colMeans(x_train)
x_scale <- apply(x_train, 2, sd)
x_scale[is.na(x_scale) | x_scale == 0] <- 1

x_train_scaled <- as.data.frame(scale(x_train, center = x_center, scale = x_scale))
x_valid_scaled <- as.data.frame(scale(x_valid, center = x_center, scale = x_scale))

class_levels <- levels(df$diagnosis)
pos_class <- class_levels[2]
neg_class <- class_levels[1]

train_nn_df <- x_train_scaled
train_nn_df$diagnosis_num <- as.integer(df$diagnosis == pos_class)
predictor_names <- setdiff(names(train_nn_df), "diagnosis_num")
nn_formula <- as.formula(sprintf("diagnosis_num ~ %s", paste(predictor_names, collapse = " + ")))
nn_formula_text <- paste(deparse(nn_formula), collapse = "")

fit_and_eval_neuralnet <- function(hidden_value) {
  rep_seeds <- 123 + seq_len(replications)
  fit_list <- foreach(
    rep_id = seq_len(replications),
    .packages = "neuralnet",
    .export = c(
      "nn_formula_text",
      "train_nn_df",
      "algorithm",
      "act_fct",
      "err_fct",
      "linear_output",
      "stepmax",
      "threshold",
      "lifesign",
      "lifesign_step"
    ),
    .errorhandling = "pass"
  ) %dopar% {
    set.seed(rep_seeds[rep_id])
    neuralnet(
      formula = as.formula(nn_formula_text, env = baseenv()),
      data = train_nn_df,
      hidden = hidden_value,
      algorithm = algorithm,
      act.fct = act_fct,
      err.fct = err_fct,
      linear.output = linear_output,
      stepmax = stepmax,
      threshold = threshold,
      lifesign = lifesign,
      lifesign.step = lifesign_step,
      rep = 1
    )
  }

  extract_fit_error <- function(fit_item) {
    if (inherits(fit_item, "try-error") || inherits(fit_item, "error")) return(Inf)
    if (!inherits(fit_item, "nn")) return(Inf)
    if (is.null(fit_item$result.matrix)) return(Inf)

    err <- suppressWarnings(as.numeric(fit_item$result.matrix["error", 1]))
    if (length(err) == 0 || is.na(err) || !is.finite(err)) return(Inf)
    err
  }

  fit_errors <- vapply(fit_list, extract_fit_error, numeric(1))
  valid_idx <- which(is.finite(fit_errors))
  if (length(valid_idx) == 0) {
    stop("All neuralnet repetitions failed. Try lowering hidden_layers or changing threshold/stepmax.")
  }
  fit <- fit_list[[valid_idx[which.min(fit_errors[valid_idx])]]]

  prob_valid <- as.numeric(compute(fit, x_valid_scaled)$net.result)
  validation_pred <- factor(
    ifelse(prob_valid >= classification_threshold, pos_class, neg_class),
    levels = class_levels
  )

  validation_acc <- mean(validation_pred == validation_df$diagnosis)
  roc_obj <- roc(
    response = validation_df$diagnosis,
    predictor = prob_valid,
    levels = c(neg_class, pos_class),
    quiet = TRUE
  )
  validation_auc <- as.numeric(auc(roc_obj))

  list(
    fit = fit,
    pred = validation_pred,
    prob = prob_valid,
    acc = validation_acc,
    auc = validation_auc
  )
}

main_result <- fit_and_eval_neuralnet(hidden_layers)

saveRDS(main_result$fit, model_output)

png(structure_plot_output, width = 14, height = 8, units = "in", res = 300)
plot(main_result$fit, rep = "best", show.weights = FALSE, information = TRUE)
dev.off()

validation_out <- data.frame(
  obs = validation_df$diagnosis,
  pred = main_result$pred,
  prob_positive = main_result$prob
)
write.csv(validation_out, validation_preds_output, row.names = FALSE)

metrics_out <- data.frame(
  model = "neuralnet",
  hidden_layers = paste(hidden_layers, collapse = "-"),
  algorithm = algorithm,
  act_fct = act_fct_name,
  err_fct = err_fct,
  threshold = threshold,
  stepmax = stepmax,
  replications = replications,
  cores = num_cores,
  validation_accuracy = main_result$acc,
  validation_auc = main_result$auc
)
write.csv(metrics_out, metrics_output, row.names = FALSE)

if (run_architecture_study) {
  study_rows <- lapply(architecture_grid, function(arch) {
    result <- fit_and_eval_neuralnet(arch)
    data.frame(
      hidden_layers = paste(arch, collapse = "-"),
      algorithm = algorithm,
      act_fct = act_fct_name,
      err_fct = err_fct,
      threshold = threshold,
      stepmax = stepmax,
      validation_accuracy = result$acc,
      validation_auc = result$auc
    )
  })
  study_df <- do.call(rbind, study_rows)
  write.csv(study_df, architecture_study_output, row.names = FALSE)
  cat("Saved:", architecture_study_output, "\n")
}

cat("Training complete\n")
cat("Method: neuralnet\n")
cat("Saved:", model_output, "\n")
cat("Saved:", validation_preds_output, "\n")
cat("Saved:", metrics_output, "\n")
cat("Saved:", structure_plot_output, "\n")
cat("Cores:", num_cores, "\n")
cat("Validation Accuracy:", sprintf("%.4f", main_result$acc), "\n")
cat("Validation ROC AUC:", sprintf("%.4f", main_result$auc), "\n")
