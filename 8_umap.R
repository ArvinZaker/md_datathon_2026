#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggsci)
})

if (!requireNamespace("umap", quietly = TRUE)) {
  stop("Package 'umap' is required. Install it before running `Rscript 8_umap.R`.")
}

input_path <- "all_cleaned2.csv"
out_root <- file.path("results", "8_umap")

dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_path)) {
  stop(sprintf("Input file not found: %s", input_path))
}

set.seed(123)

sanitize_name <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

pretty_name <- function(x) {
  tools::toTitleCase(gsub("_", " ", x))
}

add_white_background <- function(p) {
  p + theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA)
  )
}

df <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)
df <- df[, names(df) != "", drop = FALSE]

numeric_vars <- names(df)[sapply(df, is.numeric)]
categorical_vars <- names(df)[!sapply(df, is.numeric)]

if (length(categorical_vars) == 0) {
  stop("No categorical variables found to color the UMAP plots.")
}

plot_source_df <- df

if ("diagnosis" %in% names(plot_source_df)) {
  plot_source_df$diagnosis <- ifelse(
    plot_source_df$diagnosis == ">50% narrowing",
    "Narrowing",
    "No narrowing"
  )
}

if ("age" %in% names(plot_source_df)) {
  plot_source_df[["age group"]] <- ifelse(plot_source_df$age >= 57, ">=57", "<57")
}

if ("max HR" %in% names(plot_source_df)) {
  plot_source_df[["max HR group"]] <- ifelse(plot_source_df$`max HR` >= 143, ">=143", "<143")
}

if ("exercise ST depression" %in% names(plot_source_df)) {
  plot_source_df[["exercise ST depression group"]] <- ifelse(
    plot_source_df$`exercise ST depression` >= 1.5,
    ">=1.5",
    "<1.5"
  )
}

plot_categorical_vars <- names(plot_source_df)[!sapply(plot_source_df, is.numeric)]

feature_df <- df

for (var in numeric_vars) {
  values <- feature_df[[var]]
  if (anyNA(values)) {
    values[is.na(values)] <- stats::median(values, na.rm = TRUE)
  }
  feature_df[[var]] <- values
}

for (var in categorical_vars) {
  values <- as.character(feature_df[[var]])
  values[is.na(values) | trimws(values) == ""] <- "Missing"
  feature_df[[var]] <- factor(values)
}

feature_matrix <- stats::model.matrix(~ . - 1, data = feature_df)

if (ncol(feature_matrix) < 2) {
  stop("UMAP needs at least two encoded feature columns.")
}

zero_var <- apply(feature_matrix, 2, function(x) {
  identical(stats::sd(x), 0) || is.na(stats::sd(x))
})
if (any(zero_var)) {
  feature_matrix <- feature_matrix[, !zero_var, drop = FALSE]
}

scaled_matrix <- scale(feature_matrix)
scaled_matrix[is.na(scaled_matrix)] <- 0

umap_config <- umap::umap.defaults
umap_config$n_neighbors <- min(15, max(2, nrow(scaled_matrix) - 1))
umap_config$random_state <- 123
umap_config$verbose <- FALSE

umap_fit <- umap::umap(
  scaled_matrix,
  config = umap_config,
  method = "naive",
  preserve.seed = TRUE
)

embedding <- as.data.frame(umap_fit$layout)
names(embedding) <- c("UMAP1", "UMAP2")

plot_df <- cbind(plot_source_df, embedding)

coords_path <- file.path(out_root, "8_umap_coordinates.csv")
pdf_path <- file.path(out_root, "8_umap_categorical_panels.pdf")
write.csv(plot_df, coords_path, row.names = FALSE)

build_umap_plot <- function(var_name) {
  var_df <- data.frame(
    UMAP1 = plot_df$UMAP1,
    UMAP2 = plot_df$UMAP2,
    group = as.character(plot_df[[var_name]]),
    stringsAsFactors = FALSE
  )
  var_df$group[is.na(var_df$group) | trimws(var_df$group) == ""] <- "Missing"
  group_levels <- names(sort(table(var_df$group), decreasing = TRUE))
  var_df$group <- factor(var_df$group, levels = group_levels)

  p <- ggplot(var_df, aes(x = UMAP1, y = UMAP2, color = group)) +
    geom_point(size = 2, alpha = 0.85) +
    scale_color_lancet() +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(
      title = pretty_name(var_name),
      x = "UMAP 1",
      y = "UMAP 2",
      color = pretty_name(var_name)
    ) +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold"),
      legend.box = "vertical",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 20, r = 12, b = 10, l = 12)
    )

  add_white_background(p)
}

manifest <- data.frame(
  variable = character(0),
  type = character(0),
  page = integer(0),
  output_file = character(0),
  stringsAsFactors = FALSE
)

pdf(
  file = pdf_path,
  width = 5,
  height = 5,
  onefile = TRUE,
  bg = "white"
)

for (i in seq_along(plot_categorical_vars)) {
  var <- plot_categorical_vars[i]
  plot_obj <- build_umap_plot(var)
  print(plot_obj)

  manifest <- rbind(
    manifest,
    data.frame(
      variable = var,
      type = "categorical_umap",
      page = i,
      output_file = pdf_path,
      stringsAsFactors = FALSE
    )
  )
}
dev.off()

manifest <- rbind(
  manifest,
  data.frame(
    variable = "all_categorical_variables",
    type = "categorical_umap_pdf",
    page = NA_integer_,
    output_file = pdf_path,
    stringsAsFactors = FALSE
  ),
  data.frame(
    variable = "umap_coordinates",
    type = "data",
    page = NA_integer_,
    output_file = coords_path,
    stringsAsFactors = FALSE
  )
)

manifest_path <- file.path(out_root, "8_umap_manifest.csv")
write.csv(manifest, manifest_path, row.names = FALSE)

cat("Saved", nrow(manifest), "outputs to", out_root, "\n")
cat("Manifest:", manifest_path, "\n")
