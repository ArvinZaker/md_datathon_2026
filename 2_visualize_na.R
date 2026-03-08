#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(ComplexHeatmap)
  library(circlize)
  library(corrplot)
  library(ggplot2)
  library(ggpubr)
  library(png)
  library(grid)
})

input_path <- "all.csv"
out_dir <- "results"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

df <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)
vars <- setdiff(names(df), "dataset")

if (length(vars) == 0) {
  stop("No variables found to visualize.")
}

if (!("dataset" %in% names(df))) {
  stop("Column 'dataset' is required.")
}

pretty_name_map <- c(
  "age" = "Age",
  "sex" = "Sex",
  "chest pain" = "Chest Pain",
  "resting bp" = "Resting BP",
  "cholestoral" = "Cholesterol",
  "fasting sugar" = "Fasting Sugar",
  "resting ECG" = "Resting ECG",
  "max HR" = "Max HR",
  "exercise angina" = "Exercise Angina",
  "exercise ST depression" = "Exercise ST Depression",
  "slope ST segment" = "ST Segment Slope",
  "number of vessels flourosopy" = "Vessels (Fluoroscopy)",
  "thal" = "Thal",
  "diagnosis" = "Diagnosis"
)
pretty_vars <- ifelse(vars %in% names(pretty_name_map), pretty_name_map[vars], tools::toTitleCase(vars))

na_mat <- is.na(df[, vars, drop = FALSE])
storage.mode(na_mat) <- "numeric"

dataset_levels <- unique(df$dataset)
preferred_order <- c("cleveland", "hungarian", "switzerland", "va")
dataset_levels <- c(
  intersect(preferred_order, dataset_levels),
  setdiff(dataset_levels, preferred_order)
)
dataset_levels <- unique(dataset_levels)
ord <- order(match(df$dataset, dataset_levels))

mat <- na_mat[ord, , drop = FALSE]
colnames(mat) <- pretty_vars
rownames(mat) <- NULL

split_raw <- df$dataset[ord]
counts <- table(factor(split_raw, levels = dataset_levels))
split_labels <- sprintf("%s\n(n=%d)", dataset_levels, as.integer(counts))
split_factor <- factor(split_raw, levels = dataset_levels, labels = split_labels)

dataset_counts <- as.data.frame(
  table(factor(df$dataset, levels = dataset_levels)),
  stringsAsFactors = FALSE
)
names(dataset_counts) <- c("dataset", "n")
dataset_counts$pct <- dataset_counts$n / sum(dataset_counts$n)
dataset_counts$label <- sprintf("%.1f%%\n(n=%d)", 100 * dataset_counts$pct, dataset_counts$n)

dataset_bar_plot <- ggplot(dataset_counts, aes(x = dataset, y = n)) +
  geom_col(fill = "#d8b4f8", width = 0.8) +
  geom_text(
    aes(y = n / 2, label = label),
    color = "white",
    fontface = "bold",
    lineheight = 0.9
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Dataset Sample Size",
    x = "Dataset",
    y = "Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(out_dir, "all_dataset_bar_chart.png"),
  plot = dataset_bar_plot,
  width = 7,
  height = 5,
  dpi = 300
)

ht <- Heatmap(
  mat,
  name = "Status",
  col = colorRamp2(c(0, 1), c("#f7f7f7", "#f4a8ad")),
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  row_split = split_factor,
  show_row_names = FALSE,
  row_title = "%s",
  row_title_gp = gpar(fontsize = 16, fontface = "bold"),
  row_title_rot = 0,
  column_title = "Row-Level Missingness by Dataset",
  column_title_gp = gpar(fontsize = 16, fontface = "bold"),
  column_names_gp = gpar(fontsize = 12, fontface = "bold"),
  column_names_rot = 45,
  heatmap_legend_param = list(
    at = c(0, 1),
    labels = c("Present", "Missing"),
    title = "Cell Status"
  )
)

png(file.path(out_dir, "all_na_complex_heatmap.png"), width = 14, height = 10, units = "in", res = 300)
draw(ht, heatmap_legend_side = "right")
dev.off()

# Dataset x variable missing-rate heatmap
na_rate <- vapply(dataset_levels, function(ds) {
  idx <- which(df$dataset == ds)
  colMeans(na_mat[idx, , drop = FALSE], na.rm = TRUE)
}, numeric(length(vars)))
na_rate <- t(na_rate)
colnames(na_rate) <- pretty_vars
rownames(na_rate) <- sprintf("%s\n(n=%d)", dataset_levels, as.integer(table(factor(df$dataset, levels = dataset_levels))))

ht_rate <- Heatmap(
  na_rate,
  name = "Missing %",
  col = colorRamp2(c(0, 0.5, 1), c("#f7f7f7", "#f4a8ad", "#d94b5f")),
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  row_names_gp = gpar(fontsize = 12, fontface = "bold"),
  column_names_gp = gpar(fontsize = 11, fontface = "bold"),
  column_names_rot = 45,
  column_title = "Missingness Percentage by Dataset and Variable",
  column_title_gp = gpar(fontsize = 16, fontface = "bold"),
  cell_fun = function(j, i, x, y, w, h, fill) {
    grid.text(sprintf("%.0f%%", 100 * na_rate[i, j]), x, y, gp = gpar(fontsize = 8))
  },
  heatmap_legend_param = list(
    at = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0%", "25%", "50%", "75%", "100%")
  )
)

png(file.path(out_dir, "all_na_missing_rate_heatmap.png"), width = 10, height = 3.5, units = "in", res = 500)
draw(ht_rate, heatmap_legend_side = "right")
dev.off()

# Co-occurring NA proportion heatmaps
make_co_na_plot <- function(ds, idx) {
  miss_ds <- na_mat[idx, , drop = FALSE]
  storage.mode(miss_ds) <- "numeric"
  co_na <- (t(miss_ds) %*% miss_ds) / nrow(miss_ds)
  co_na[is.na(co_na)] <- 0
  co_na <- 100 * co_na
  colnames(co_na) <- pretty_vars
  rownames(co_na) <- pretty_vars

  tf <- tempfile(fileext = ".png")
  png(tf, width = 11, height = 9, units = "in", res = 300)
  par(mar = c(2, 2, 8, 6), xpd = NA)
  cp <- suppressWarnings(corrplot(
    co_na,
    method = "circle",
    type = "lower",
    is.corr = FALSE,
    order = "hclust",
    hclust.method = "average",
    col.lim = c(0, 100),
    col = colorRampPalette(c("#f7f7f7", "#f4a8ad", "#d94b5f"))(200),
    tl.col = "black",
    tl.pos = "ld",
    tl.offset = 0.25,
    tl.srt = 35,
    tl.cex = 0.72,
    addCoef.col = NA,
    cl.pos = "r",
    cl.cex = 0.8,
    diag = TRUE
  ))
  coef_pos <- cp$corrPos
  coef_pos <- coef_pos[coef_pos$corr > 0, , drop = FALSE]
  if (nrow(coef_pos) > 0) {
    text(coef_pos$x, coef_pos$y, labels = sprintf("%.1f", coef_pos$corr), cex = 0.55, col = "black")
  }
  mtext("Percentage (%)", side = 4, line = 0.25, cex = 2)
  title(main = sprintf("Co-Occurring NA Rate: %s (n=%d)", ds, length(idx)), cex.main = 1.2, line = 3.2)
  dev.off()

  ggpubr::as_ggplot(grid::rasterGrob(png::readPNG(tf), interpolate = TRUE))
}

# 4 dataset panels in one PNG
plot_list_4 <- lapply(dataset_levels, function(ds) {
  make_co_na_plot(ds, which(df$dataset == ds))
})

panel_4 <- ggpubr::ggarrange(
  plotlist = plot_list_4,
  labels = dataset_levels,
  ncol = 2,
  nrow = 2
)

ggplot2::ggsave(
  filename = file.path(out_dir, "co_na_rate_4datasets.png"),
  plot = panel_4,
  width = 16,
  height = 16,
  dpi = 300
)

# All-data panel in separate PNG
plot_all <- make_co_na_plot("all", seq_len(nrow(df)))
ggplot2::ggsave(
  filename = file.path(out_dir, "co_na_rate_all.png"),
  plot = plot_all,
  width = 11,
  height = 9,
  dpi = 300
)
