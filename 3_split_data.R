library(caret)
set.seed(123)

input_candidates <- c("all_cleaned2.csv", "all_cleaned.csv", "cleavland_cleaned.csv")
for (i in seq_along(input_candidates)) {
  input_path <- input_candidates[file.exists(input_candidates)][i]

  df <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)

  if ("diagnosis" %in% names(df)) {
    idx <- createDataPartition(df$diagnosis, p = 0.80, list = FALSE)
  } else {
    idx <- sample(seq_len(nrow(df)), size = floor(0.80 * nrow(df)))
  }

  internal_validation <- df[idx, , drop = FALSE]
  external_validation <- df[-idx, , drop = FALSE]
  cat("Input:", input_path, "\n")
  cat("Split complete\n")
  cat("Internal (80%):", nrow(internal_validation), "rows\n")
  cat("External (20%):", nrow(external_validation), "rows\n")
  write.csv(internal_validation, sprintf("./ml_inputs/train_%s", input_path))
  write.csv(external_validation, sprintf("./ml_inputs/external_%s", input_path))
}
