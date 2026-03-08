#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggsci)
  library(scales)
})

input_path <- "all_cleaned2.csv"
out_root <- file.path("results", "7_variable_panels")

dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_path)) {
  stop(sprintf("Input file not found: %s", input_path))
}

df <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)

if (!("dataset" %in% names(df))) {
  stop("Column 'dataset' is required to make dataset-specific figures.")
}

df$dataset <- as.character(df$dataset)
preferred_order <- c("cleveland", "hungarian", "switzerland", "va")
dataset_levels <- unique(df$dataset)
dataset_levels <- c(
  intersect(preferred_order, dataset_levels),
  setdiff(dataset_levels, preferred_order)
)
dataset_levels <- unique(dataset_levels)
dataset_order <- c("Overall", dataset_levels)

numeric_vars <- setdiff(names(df)[sapply(df, is.numeric)], "dataset")
categorical_vars <- setdiff(names(df)[!sapply(df, is.numeric)], "dataset")

pretty_name <- function(x) {
  tools::toTitleCase(gsub("_", " ", x))
}

plot_df <- rbind(
  transform(df, dataset_group = "Overall"),
  transform(df, dataset_group = df$dataset)
)
plot_df$dataset_group <- factor(plot_df$dataset_group, levels = dataset_order)

bar_pdf_path <- file.path(out_root, "bar_charts.pdf")
hist_pdf_path <- file.path(out_root, "histograms.pdf")
density_pdf_path <- file.path(out_root, "kernel_density_plots.pdf")

manifest <- data.frame(
  variable = character(0),
  type = character(0),
  output_file = character(0),
  page = character(0),
  stringsAsFactors = FALSE
)

save_plots_pdf <- function(plot_list, file_path, width, height) {
  if (length(plot_list) == 0) {
    return(invisible(NULL))
  }

  pdf(file = file_path, width = width, height = height, onefile = TRUE)
  on.exit(dev.off(), add = TRUE)
  for (p in plot_list) {
    print(p)
  }
}

make_categorical_bar_plot <- function(var) {
  cat_values <- as.character(plot_df[[var]])
  cat_values[is.na(cat_values) | trimws(cat_values) == ""] <- "Missing"

  var_df <- data.frame(
    dataset_group = plot_df$dataset_group,
    category = cat_values,
    stringsAsFactors = FALSE
  )

  ordered_levels <- names(sort(table(var_df$category), decreasing = TRUE))
  var_df$category <- factor(var_df$category, levels = ordered_levels)

  count_df <- as.data.frame(table(var_df$dataset_group, var_df$category), stringsAsFactors = FALSE)
  names(count_df) <- c("dataset_group", "category", "n")
  count_df <- count_df[count_df$n > 0, , drop = FALSE]
  count_df$dataset_group <- factor(count_df$dataset_group, levels = dataset_order)
  count_df$category <- factor(count_df$category, levels = ordered_levels)
  totals <- ave(count_df$n, count_df$dataset_group, FUN = sum)
  count_df$prop <- count_df$n / totals
  count_df$label <- sprintf("%s\n(n=%d)", label_percent(accuracy = 0.1)(count_df$prop), count_df$n)

  ggplot(count_df, aes(x = dataset_group, y = prop, fill = category)) +
    geom_col(color = "white", linewidth = 0.2, width = 0.80) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      size = 2.6,
      color = "white",
      fontface = "bold",
      lineheight = 0.9
    ) +
    scale_fill_lancet() +
    scale_y_continuous(labels = label_percent(accuracy = 1)) +
    labs(
      title = sprintf("%s Distribution by Dataset", pretty_name(var)),
      x = "Dataset",
      y = "Percent",
      fill = pretty_name(var)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(face = "bold"),
      legend.position = "top",
      legend.title = element_text(face = "bold")
    )
}

make_dataset_size_plot <- function() {
  dataset_counts <- as.data.frame(table(factor(df$dataset, levels = dataset_levels)), stringsAsFactors = FALSE)
  names(dataset_counts) <- c("dataset", "n")
  dataset_counts$pct <- dataset_counts$n / sum(dataset_counts$n)
  dataset_counts$label <- sprintf("%.1f%%\n(n=%d)", 100 * dataset_counts$pct, dataset_counts$n)

  ggplot(dataset_counts, aes(x = dataset, y = n)) +
    geom_col(fill = "black", width = 0.80) +
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
}

make_histogram_grid_plot <- function(var) {
  overall_df <- data.frame(
    panel = "Overall",
    panel_type = "Overall",
    value = df[[var]],
    stringsAsFactors = FALSE
  )
  dataset_df <- df[, c("dataset", var), drop = FALSE]
  names(dataset_df) <- c("panel", "value")
  dataset_df$panel <- as.character(dataset_df$panel)
  dataset_df$panel_type <- "Dataset"
  hist_df <- rbind(overall_df, dataset_df)
  hist_df <- hist_df[!is.na(hist_df$value), , drop = FALSE]
  hist_df$panel <- factor(hist_df$panel, levels = dataset_order)
  hist_df$panel_type <- factor(hist_df$panel_type, levels = c("Overall", "Dataset"))

  if (nrow(hist_df) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No non-missing values") +
        xlim(-1, 1) +
        ylim(-1, 1) +
        labs(title = pretty_name(var), x = pretty_name(var), y = "Count") +
        theme_void(base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  }

  ggplot(hist_df, aes(x = value, fill = panel_type)) +
    geom_histogram(
      bins = 30,
      na.rm = TRUE,
      color = "white",
      linewidth = 0.2
    ) +
    facet_wrap(~panel, ncol = 2, scales = "free_y") +
    scale_fill_manual(values = c("Overall" = "grey80", "Dataset" = "#F6C89F")) +
    labs(
      title = sprintf("%s: Histograms", pretty_name(var)),
      x = pretty_name(var),
      y = "Count"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      legend.position = "none",
      panel.spacing = grid::unit(0.35, "lines")
    )
}

make_dataset_density_plot <- function(var) {
  density_df <- df[, c("dataset", var), drop = FALSE]
  names(density_df) <- c("dataset", "value")
  density_df <- density_df[!is.na(density_df$value), , drop = FALSE]
  density_df$dataset <- factor(density_df$dataset, levels = dataset_levels)

  if (nrow(density_df) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No non-missing values") +
        xlim(-1, 1) +
        ylim(-1, 1) +
        labs(title = pretty_name(var), x = pretty_name(var), y = "Density") +
        theme_void(base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  }

  ggplot(density_df, aes(x = value, color = dataset)) +
    geom_density(linewidth = 1, na.rm = TRUE) +
    scale_color_lancet() +
    labs(
      title = sprintf("%s: Kernel Density by Dataset", pretty_name(var)),
      x = pretty_name(var),
      y = "Density",
      color = "Dataset"
    ) +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "top",
      legend.title = element_text(face = "bold")
    )
}

bar_plots <- list()
hist_plots <- list()
density_plots <- list()

if (length(categorical_vars) > 0) {
  for (var in categorical_vars) {
    bar_plots[[length(bar_plots) + 1]] <- make_categorical_bar_plot(var)
    manifest <- rbind(
      manifest,
      data.frame(
        variable = var,
        type = "categorical",
        output_file = bar_pdf_path,
        page = pretty_name(var),
        stringsAsFactors = FALSE
      )
    )
  }
}

bar_plots[[length(bar_plots) + 1]] <- make_dataset_size_plot()
manifest <- rbind(
  manifest,
  data.frame(
    variable = "dataset",
    type = "dataset_size",
    output_file = bar_pdf_path,
    page = "Dataset Sample Size",
    stringsAsFactors = FALSE
  )
)

if (length(numeric_vars) > 0) {
  for (var in numeric_vars) {
    hist_plots[[length(hist_plots) + 1]] <- make_histogram_grid_plot(var)
    manifest <- rbind(
      manifest,
      data.frame(
        variable = var,
        type = "histogram",
        output_file = hist_pdf_path,
        page = pretty_name(var),
        stringsAsFactors = FALSE
      )
    )

    density_plots[[length(density_plots) + 1]] <- make_dataset_density_plot(var)
    manifest <- rbind(
      manifest,
      data.frame(
        variable = var,
        type = "kernel_density",
        output_file = density_pdf_path,
        page = pretty_name(var),
        stringsAsFactors = FALSE
      )
    )
  }
}

save_plots_pdf(bar_plots, bar_pdf_path, width = 4, height = 4)
save_plots_pdf(hist_plots, hist_pdf_path, width = 10, height = 6.5)
save_plots_pdf(density_plots, density_pdf_path, width = 10, height = 6.5)

manifest_path <- file.path(out_root, "7_data_exploration_manifest.csv")
write.csv(manifest, manifest_path, row.names = FALSE)

cat("Saved", nrow(manifest), "pages across PDF outputs in", out_root, "\n")
cat("Manifest:", manifest_path, "\n")
