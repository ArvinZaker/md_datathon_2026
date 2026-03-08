library(pROC)
library(rpart)

set.seed(123)

train_preds_path <- "rpart/rpart_training_predictions.csv"
cv_preds_path <- "rpart/rpart_caret_cv_predictions.csv"
model_path <- "rpart/rpart_caret_model.rds"
validation_preds_path <- "rpart/rpart_validation_predictions.csv"
cv_train_roc_plot_path <- "rpart/rpart_cv_training_roc.png"
cv_test_roc_plot_path <- "rpart/rpart_cv_testing_roc.png"
validation_roc_plot_path <- "rpart/rpart_validation_roc.png"

train_out <- read.csv(train_preds_path, stringsAsFactors = FALSE, check.names = FALSE)
validation_out <- read.csv(validation_preds_path, stringsAsFactors = FALSE, check.names = FALSE)
cv_preds <- read.csv(cv_preds_path, stringsAsFactors = FALSE, check.names = FALSE)
rpart_fit <- readRDS(model_path)

if (all(c("No_Narrowing", "Has_Narrowing") %in% unique(validation_out$obs))) {
  validation_out$obs <- factor(validation_out$obs, levels = c("No_Narrowing", "Has_Narrowing"))
  train_out$obs <- factor(train_out$obs, levels = c("No_Narrowing", "Has_Narrowing"))
  cv_preds$obs <- factor(cv_preds$obs, levels = c("No_Narrowing", "Has_Narrowing"))
  pos_class <- "Has_Narrowing"
} else {
  validation_out$obs <- factor(validation_out$obs)
  train_out$obs <- factor(train_out$obs, levels = levels(validation_out$obs))
  cv_preds$obs <- factor(cv_preds$obs, levels = levels(validation_out$obs))
  pos_class <- levels(validation_out$obs)[2]
}
neg_class <- levels(validation_out$obs)[1]
train_outcome <- train_out$obs
train_prob_positive <- train_out$prob_positive

cv_test_prob_positive <- cv_preds[[pos_class]]
fold_ids <- sort(unique(cv_preds$Resample))

roc_cv_train <- roc(
  response = train_outcome,
  predictor = train_prob_positive,
  levels = c(neg_class, pos_class),
  quiet = TRUE
)
auc_cv_train <- as.numeric(auc(roc_cv_train))

roc_cv_test <- roc(
  response = cv_preds$obs,
  predictor = cv_test_prob_positive,
  levels = c(neg_class, pos_class),
  quiet = TRUE
)
auc_cv_test <- as.numeric(auc(roc_cv_test))

train_row_ids <- train_out$rowIndex

cv_train_fold_rocs <- list()
cv_train_fold_aucs <- numeric(length(fold_ids))
cv_test_fold_rocs <- list()
cv_test_fold_aucs <- numeric(length(fold_ids))
names(cv_train_fold_aucs) <- fold_ids
names(cv_test_fold_aucs) <- fold_ids

for (fold_id in fold_ids) {
  fold_test_ids <- unique(cv_preds$rowIndex[cv_preds$Resample == fold_id])
  fold_test_pos <- which(train_row_ids %in% fold_test_ids)
  fold_train_pos <- setdiff(seq_len(length(train_outcome)), fold_test_pos)

  roc_train_fold <- roc(
    response = train_outcome[fold_train_pos],
    predictor = train_prob_positive[fold_train_pos],
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

roc_validation <- roc(
  response = validation_out$obs,
  predictor = validation_out$prob_positive,
  levels = c(neg_class, pos_class),
  quiet = TRUE
)
auc_validation <- as.numeric(auc(roc_validation))

png(cv_train_roc_plot_path, width = 7, height = 7, units = "in", res = 300)
par(xaxs = "i", yaxs = "i")
plot(roc_cv_train, main = "5-Fold CV Training ROC", col = "#2a9d8f", lwd = 3, xlim = c(1.02, -0.02), ylim = c(-0.02, 1.02))
fold_cols <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51")
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
plot(roc_cv_test, main = "5-Fold CV Testing ROC", col = "#457b9d", lwd = 3, xlim = c(1.02, -0.02), ylim = c(-0.02, 1.02))
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
plot(roc_validation, main = "External Validation ROC", col = "#d94b5f", lwd = 3, xlim = c(1.02, -0.02), ylim = c(-0.02, 1.02))
abline(a = 0, b = 1, lty = 2, col = "gray50")
text(0.22, 0.08, labels = sprintf("AUC = %.3f", auc_validation), cex = 1.0, font = 2)
dev.off()

dt_fit <- if (inherits(rpart_fit, "train")) rpart_fit$finalModel else rpart_fit

node_prob_stenosis <- dt_fit$frame$yval2[, ncol(dt_fit$frame$yval2) - 1]
risk_palette <- grDevices::colorRampPalette(c("#4f79a8", "#ffffff", "#b56b80"))
risk_colors <- risk_palette(101)
risk_idx <- pmin(100, pmax(0, round(node_prob_stenosis * 100))) + 1
is_leaf_node <- as.character(dt_fit$frame$var) == "<leaf>"
node_box_col <- ifelse(is_leaf_node, risk_colors[risk_idx], "#ffffff")

# Placeholder: customize INTERNAL (middle) node labels here.
# Edit this function only to fine-tune wording in the middle of the graph.
internal_node_label_custom <- function(label_text) {
  out <- label_text
  # Examples:
  out <- gsub("Asymptomatic_Chest_Pain", "Chest pain", out, fixed = TRUE)
  out <- gsub("Asymptomatic Chest Pain", "Chest pain", out, fixed = TRUE)
  out <- gsub("exercise ST depression", "ST depression\nduring exercise (mV)", out, fixed = TRUE)
  out <- gsub("exercise angina", "Angina during exercise", out, fixed = TRUE)
  out <- gsub("age", "Age\n(years)", out, fixed = TRUE)
  out <- gsub("max HR", "Max Heart Rate\n(BPM)", out, fixed = TRUE)
  out <- gsub("sex", "Sex", out, fixed = TRUE)
  # out <- gsub("", "", out, fixed = TRUE)
  out
}

node_label_custom <- function(x, labs, digits, varlen) {
  out <- labs
  node_vars <- as.character(x$frame$var)
  ylevels <- attr(x, "ylevels")
  leaf_status <- ylevels[x$frame$yval]
  yval2 <- x$frame$yval2
  prob_stenosis <- yval2[, ncol(yval2) - 1]

  for (i in seq_along(out)) {
    risk_label <- sprintf("(%.1f%% prob.)", 100 * prob_stenosis[i])
    if (node_vars[i] != "<leaf>") {
      var_label <- gsub("_", " ", node_vars[i], fixed = TRUE)
      out[i] <- internal_node_label_custom(var_label)
    } else {
      status_label <- gsub("_", " ", leaf_status[i], fixed = TRUE)
      out[i] <- paste(status_label, risk_label, sep = "\n")
    }
  }
  out
}
split_label_custom <- function(x, labs, digits, varlen, faclen) {
  out <- labs
  out <- gsub("Asymptomatic_Chest_Pain = No", "Symptomatic", out, fixed = TRUE)
  out <- gsub("Yes", "Asymptomatic", out, fixed = TRUE)
  out <- gsub("Asymptomatic_Chest_Pain = 0", "Symptomatic", out, fixed = TRUE)
  out <- gsub("sex = female", "Female", out)
  out <- gsub("^male$", "Male", out)
  out <- gsub("exercise angina = no", "No", out)
  out <- gsub("yes", "Yes", out)
  out <- gsub("exercise ST depression < 1.5", "< 1.5", out)
  out <- gsub("max HR >= 143", ">= 143", out)
  out <- gsub("age < 57", "< 57", out)
  out <- gsub("^ = 1", "Asymptomatic", out)
  # out <- gsub("", "", out)
  out
}


png("rpart/decision_tree_presentation.png", width = 4.5, height = 3, units = "in", res = 500)
rpart.plot::rpart.plot(
  dt_fit,
  type = 4,
  extra = 1,
  under = TRUE,
  fallen.leaves = TRUE,
  tweak = 1,
  font = 2,
  split.font = 2,
  node.fun = node_label_custom,
  split.fun = split_label_custom,
  box.col = node_box_col,
  round = 0,
  leaf.round = 0,
  border.col = "#4a4a4a",
  shadow.col = 0
  # main = "CLEAR\n(Care Logic for Exercise Assessment and Referral)"
)
dev.off()
png("rpart/decision_tree_presentation_raw.png", width = 11, height = 8, units = "in", res = 500)
rpart.plot::rpart.plot(
  dt_fit,
  type = 4,
  extra = 106,
  box.col = node_box_col,
  round = 0,
  leaf.round = 0,
  border.col = "#4a4a4a",
  shadow.col = 0
)
dev.off()
