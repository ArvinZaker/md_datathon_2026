library(shiny)
library(caret)
library(ggplot2)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

normalize_text <- function(x) {
  tolower(trimws(as.character(x)))
}

.addBackGround <- function(p) {
  p + ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    legend.background = ggplot2::element_rect(fill = "white", color = NA),
    plot.background = ggplot2::element_rect(fill = "white", color = NA)
  )
}

normalize_named_vector <- function(x) {
  if (is.null(x)) {
    return(setNames(character(0), character(0)))
  }
  x_vec <- unlist(x, use.names = TRUE)
  setNames(as.character(x_vec), normalize_text(names(x_vec)))
}

pretty_choice_label <- function(x) {
  x_norm <- normalize_text(x)
  if (x_norm == "normal") {
    return("Normal")
  }
  if (x_norm == "st-t wave abnormality") {
    return("ST-T Wave Abnormality")
  }
  if (x_norm == "left ventricular hypertrophy") {
    return("Left Ventricular Hypertrophy")
  }
  if (x_norm == "typical angina") {
    return("Typical Angina")
  }
  if (x_norm == "atypical angina") {
    return("Atypical Angina")
  }
  if (x_norm == "non-anginal pain") {
    return("Non-Anginal Pain")
  }
  if (x_norm == "asymptomatic") {
    return("Asymptomatic")
  }
  if (x_norm == "male") {
    return("Male")
  }
  if (x_norm == "female") {
    return("Female")
  }
  if (x_norm == "yes") {
    return("Yes")
  }
  if (x_norm == "no") {
    return("No")
  }
  x
}

format_choice_vector <- function(values) {
  stats::setNames(values, vapply(values, pretty_choice_label, character(1)))
}

pretty_feature_name <- function(feature_name) {
  if (feature_name == "chest_pain_typical_angina") {
    return("Chest pain: Typical angina")
  }
  if (feature_name == "chest_pain_atypical_angina") {
    return("Chest pain: Atypical angina")
  }
  if (feature_name == "chest_pain_non_anginal_pain") {
    return("Chest pain: Non-anginal pain")
  }
  if (feature_name == "chest_pain_asymptomatic") {
    return("Chest pain: Asymptomatic")
  }
  if (feature_name == "resting_ecg_normal") {
    return("Resting ECG: Normal")
  }
  if (feature_name == "resting_ecg_st_t_wave_abnormality") {
    return("Resting ECG: ST-T wave abnormality")
  }
  if (feature_name == "resting_ecg_left_ventricular_hypertrophy") {
    return("Resting ECG: Left ventricular hypertrophy")
  }
  if (feature_name == "sex_male") {
    return("Sex")
  }
  if (feature_name == "fasting_sugar_high") {
    return("Fasting sugar")
  }
  if (feature_name == "exercise_angina_yes") {
    return("Has exercise angina")
  }
  if (feature_name == "exercise_st_depression") {
    return("Exercise ST depression (mV)")
  }
  if (feature_name == "resting_bp") {
    return("Resting BP (mmHg)")
  }
  if (feature_name == "max_hr") {
    return("Max HR (bpm)")
  }
  if (feature_name == "age") {
    return("Age")
  }
  if (feature_name == "cholestoral") {
    return("Cholesterol (mg/dL)")
  }

  label <- gsub("_", " ", feature_name, fixed = TRUE)
  tools::toTitleCase(label)
}

clean_shap_waterfall_labels <- function(labels) {
  out <- labels
  # Edit SHAP waterfall row labels here with gsub().
  out <- gsub("^Sex = 1$", "Male sex", out)
  out <- gsub("^Sex = 0$", "Female sex", out)
  out <- gsub("^Has exercise angina = 1$", "Present exercise angina", out)
  out <- gsub("^Has exercise angina = 0$", "Absent exercise angina", out)
  out <- gsub("^Fasting sugar = 1$", "High fasting sugar", out)
  out <- gsub("^Fasting sugar = 0$", "Normal fasting sugar", out)
  out <- gsub("^Chest pain: Asymptomatic = [01]$", "Asymptomatic chest pain", out)
  out <- gsub("^Chest pain: Typical angina = [01]$", "Typical angina chest pain", out)
  out <- gsub("^Chest pain: Atypical angina = [01]$", "Atypical angina chest pain", out)
  out <- gsub("^Chest pain: Non-anginal pain = [01]$", "Non-anginal chest pain", out)
  out <- gsub("^Resting ECG: Normal = [01]$", "Normal resting ECG", out)
  out <- gsub("^Resting ECG: ST-T wave abnormality = [01]$", "Resting ECG with ST-T wave abnormality", out)
  out <- gsub("^Resting ECG: Left ventricular hypertrophy = [01]$", "Resting ECG with left ventricular hypertrophy", out)
  out <- gsub("^Age = ", "Age (years) = ", out)
  out
}

disambiguate_labels <- function(labels) {
  label_count <- ave(seq_along(labels), labels, FUN = length)
  label_index <- ave(seq_along(labels), labels, FUN = seq_along)
  ifelse(label_count > 1, paste0(labels, " (", label_index, ")"), labels)
}

format_odds_multiplier <- function(v) {
  odds_x <- exp(v)
  if (odds_x >= 1) {
    pct <- (odds_x - 1) * 100
    sprintf("%.2f (%.1f%% higher odds of narrowing)", odds_x, pct)
  } else {
    pct <- (1 - odds_x) * 100
    sprintf("%.2f (%.1f%% lower odds of narrowing)", odds_x, pct)
  }
}

format_model_p_value <- function(p_value) {
  if (!is.finite(p_value)) {
    return("N/A")
  }
  if (p_value < 1e-4) {
    return("p < 0.0001")
  }
  sprintf("%.3g", p_value)
}

interpret_model_p_value <- function(p_value) {
  if (!is.finite(p_value)) {
    return("CLEAR model p-value is unavailable; further clinical correlation is needed when considering imaging.")
  }
  if (p_value < 0.05) {
    return(NULL)
  }
  "CLEAR model cannot provide an accurate prediction at this significance level; further clinical correlation is needed when considering imaging."
}

format_field_value <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    return("N/A")
  }
  if (is.numeric(value)) {
    return(as.character(value))
  }
  pretty_choice_label(as.character(value))
}

format_age_years <- function(value) {
  value_num <- suppressWarnings(as.numeric(value))
  if (!is.finite(value_num)) {
    return("N/A")
  }
  as.character(as.integer(round(value_num)))
}

format_chest_pain_presentation <- function(value) {
  value_norm <- normalize_text(value)
  if (value_norm == "asymptomatic") {
    return("no chest pain")
  }
  tolower(format_field_value(value))
}

format_ecg_finding <- function(value) {
  value_norm <- normalize_text(value)
  if (value_norm == "normal") {
    return("ECG normal")
  }
  sprintf("ECG showed %s", format_field_value(value))
}

format_exercise_angina_status <- function(value) {
  value_norm <- normalize_text(value)
  if (value_norm == "yes") {
    return("present")
  }
  if (value_norm == "no") {
    return("absent")
  }
  "unknown"
}

format_st_depression_finding <- function(value) {
  value_num <- suppressWarnings(as.numeric(value))
  if (!is.finite(value_num)) {
    return("exercise ST depression noted at: N/A")
  }
  if (abs(value_num) < 1e-12) {
    return("no ST depression noted")
  }
  sprintf("exercise ST depression noted at: %s mV", format_field_value(value_num))
}

format_report_factor_name <- function(feature_name, raw_input) {
  if (feature_name == "sex_male") {
    return(sprintf("%s sex", format_field_value(raw_input$sex)))
  }
  if (feature_name == "chest_pain_typical_angina") {
    return("Typical angina chest pain")
  }
  if (feature_name == "chest_pain_atypical_angina") {
    return("Atypical angina chest pain")
  }
  if (feature_name == "chest_pain_non_anginal_pain") {
    return("Non-anginal chest pain")
  }
  if (feature_name == "chest_pain_asymptomatic") {
    return("Asymptomatic chest pain")
  }
  if (feature_name == "exercise_angina_yes") {
    if (normalize_text(raw_input$exercise_angina) == "yes") {
      return("Present exercise angina")
    }
    return("Absent exercise angina")
  }
  if (feature_name == "fasting_sugar_high") {
    if (normalize_text(raw_input$fasting_sugar) == normalize_text(input_spec$fasting_sugar$high_label)) {
      return("High fasting sugar")
    }
    return("Normal fasting sugar")
  }
  if (feature_name == "resting_ecg_normal") {
    return("Normal resting ECG")
  }
  if (feature_name == "resting_ecg_st_t_wave_abnormality") {
    return("Resting ECG with ST-T wave abnormality")
  }
  if (feature_name == "resting_ecg_left_ventricular_hypertrophy") {
    return("Resting ECG with left ventricular hypertrophy")
  }
  if (feature_name == "exercise_st_depression") {
    return(sprintf("Exercise ST depression of %s mV", format_field_value(raw_input$exercise_st_depression)))
  }
  if (feature_name == "max_hr") {
    return(sprintf("Max HR of %s bpm", format_field_value(raw_input$max_hr)))
  }
  if (feature_name == "resting_bp") {
    return(sprintf("Resting BP of %s mmHg", format_field_value(raw_input$resting_bp)))
  }
  if (feature_name == "age") {
    return(sprintf("Age of %s years", format_age_years(raw_input$age)))
  }
  if (feature_name == "cholestoral") {
    return(sprintf("Cholesterol of %s mg/dL", format_field_value(raw_input$cholestoral)))
  }
  pretty_feature_name(feature_name)
}

format_report_factor_entry <- function(feature_name, contribution, raw_input) {
  sprintf(
    "%s (SHAP = %+.3f)",
    format_report_factor_name(feature_name, raw_input),
    contribution
  )
}

collapse_report_factor_text <- function(df, idx, raw_input, direction_text) {
  if (length(idx) == 0) {
    return(sprintf("%s: none.", direction_text))
  }
  entries <- vapply(
    idx,
    function(i) format_report_factor_entry(df$feature[i], df$contribution[i], raw_input),
    character(1)
  )
  sprintf("%s: %s.", direction_text, paste(entries, collapse = ", "))
}

recommend_imaging_text <- function(probability) {
  prob_pct <- 100 * suppressWarnings(as.numeric(probability))
  if (!is.finite(prob_pct)) {
    return("CLEAR model cannot confidently determine any imaging recommendation. Please use alternative tools to estimate narrowing probability.")
  }
  if (prob_pct <= 50) {
    return(sprintf("CLEAR model does not indicate further imaging as the predicted probability of narrowing is %.2f%%.", prob_pct))
  }
  sprintf("CLEAR model predicts narrowing probability at %.2f%%; further imaging should be considered.", prob_pct)
}

build_medical_report_text <- function(rd, model_p_value) {
  result <- rd$result
  top_df <- rd$top_df
  raw_input <- result$raw_input

  toward_idx <- which(top_df$contribution > 0)
  away_idx <- which(top_df$contribution < 0)

  toward_lines <- if (length(toward_idx) > 0) {
    vapply(toward_idx, function(i) {
      sprintf(
        "- SHAP value of %s indicates an odds ratio of %s.",
        sprintf("%s (%+.3f)", top_df$feature_label[i], top_df$contribution[i]),
        format_odds_multiplier(top_df$contribution[i])
      )
    }, character(1))
  } else {
    "none"
  }

  away_lines <- if (length(away_idx) > 0) {
    vapply(away_idx, function(i) {
      sprintf(
        "- SHAP value of %s indicates an odds ratio of %s.",
        sprintf("%s (%+.3f)", top_df$feature_label[i], top_df$contribution[i]),
        format_odds_multiplier(top_df$contribution[i])
      )
    }, character(1))
  } else {
    "none"
  }

  clinical_information <- sprintf(
    "%s year old %s patient presenting today with %s. Cholesterol %s mg/dL and fasting sugar %s.",
    format_age_years(raw_input$age),
    format_field_value(raw_input$sex),
    format_chest_pain_presentation(raw_input$chest_pain),
    format_field_value(raw_input$cholestoral),
    format_field_value(raw_input$fasting_sugar)
  )

  testing_findings <- sprintf(
    "At rest: BP %s mmHg, %s. During exercise, max HR achieved was %s bpm, exercise angina was %s, and %s.",
    format_field_value(raw_input$resting_bp),
    format_ecg_finding(raw_input$resting_ecg),
    format_field_value(raw_input$max_hr),
    format_exercise_angina_status(raw_input$exercise_angina),
    format_st_depression_finding(raw_input$exercise_st_depression)
  )

  paste(
    "MODEL-ASSISTED CORONARY NARROWING RISK REPORT",
    "",
    "**CLINICAL INFORMATION**",
    clinical_information,
    "",
    "**FINDINGS**",
    testing_findings,
    sprintf("Based on the CLEAR glmnet model, Estimated narrowing probability: %.2f%%. Model p-value: %s.", 100 * result$positive_prob, format_model_p_value(model_p_value)),
    "",
    "Factors pushing toward suspicion of narrowing:",
    paste(toward_lines, collapse = "\n"),
    "",
    "Factors pushing away from suspicion of narrowing:",
    paste(away_lines, collapse = "\n"),
    "",
    "**IMPRESSION**",
    sprintf(
      "Estimated narrowing probability is %.2f%% with model p-value %s.",
      100 * result$positive_prob, format_model_p_value(model_p_value)
    ),
    "",
    "**INTERPRETATION**",
    paste(
      interpret_model_p_value(model_p_value),
      recommend_imaging_text(result$positive_prob),
      "Use clinical judgment if the clinical picture disagrees with the model output."
    ),
    sep = "\n"
  )
}

build_shap_plot <- function(result) {
  # risk_blue <- "#7FA6C9"
  # risk_red <- "#D89AA4"
  risk_blue <- "#00468B99"
  risk_red <- "#ED000099"

  if (!requireNamespace("shapviz", quietly = TRUE)) {
    stop("Package 'shapviz' is required for the SHAP waterfall plot.")
  }
  if (!requireNamespace("ggpubr", quietly = TRUE)) {
    stop("Package 'ggpubr' is required for the SHAP risk bar.")
  }

  plot_df <- result$shap
  if (nrow(plot_df) == 0) {
    return(NULL)
  }
  plot_df <- plot_df[abs(plot_df$contribution) > 1e-10, , drop = FALSE]
  if (nrow(plot_df) == 0) {
    return(NULL)
  }

  top_n <- min(15, nrow(plot_df))
  plot_df <- plot_df[seq_len(top_n), , drop = FALSE]

  other_sum <- sum(result$shap$contribution) - sum(plot_df$contribution)
  if (abs(other_sum) > 1e-10) {
    plot_df <- rbind(
      plot_df,
      data.frame(
        feature = "Other features",
        value = NA_real_,
        baseline = NA_real_,
        contribution = other_sum,
        feature_clean = "Other features",
        stringsAsFactors = FALSE
      )
    )
  }

  waterfall_start <- result$expected_logit + c(0, cumsum(head(plot_df$contribution, -1)))
  waterfall_end <- waterfall_start + plot_df$contribution
  x_min <- min(c(waterfall_start, waterfall_end, result$expected_logit, result$sample_logit), na.rm = TRUE)
  x_max <- max(c(waterfall_start, waterfall_end, result$expected_logit, result$sample_logit), na.rm = TRUE)
  x_min <- min(x_min, 0)
  x_max <- max(x_max, 0)
  x_pad <- max(0.25, 0.10 * (x_max - x_min))
  risk_pct <- max(0, min(100, 100 * result$sample_prob))

  feature_labels <- disambiguate_labels(clean_shap_waterfall_labels(plot_df$feature_clean))
  shap_matrix <- matrix(plot_df$contribution, nrow = 1)
  colnames(shap_matrix) <- feature_labels

  feature_values <- as.data.frame(as.list(plot_df$value), check.names = FALSE)
  names(feature_values) <- feature_labels

  sv <- shapviz::shapviz(
    shap_matrix,
    X = feature_values,
    baseline = result$expected_logit
  )

  p <- suppressMessages(
    shapviz::sv_waterfall(
      sv,
      row_id = 1,
      max_display = ncol(shap_matrix),
      fill_colors = c(risk_red, risk_blue)
    )
  ) +
    ggplot2::labs(
      x = "Log-odds scale for narrowing probability"
    ) +
    ggplot2::scale_y_discrete(labels = clean_shap_waterfall_labels) +
    ggplot2::scale_x_continuous(limits = c(x_min - x_pad, x_max + x_pad)) +
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(color = "black"),
      axis.line.y = ggplot2::element_line(color = "black"),
      axis.ticks.x = ggplot2::element_line(color = "black"),
      axis.ticks.y = ggplot2::element_line(color = "black"),
      axis.text.x = ggplot2::element_text(color = "black"),
      axis.text.y = ggplot2::element_text(color = "black"),
      axis.title.x = ggplot2::element_text(color = "black", size = 12.5),
      axis.title.y = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(size = 13),
      plot.margin = grid::unit(c(4, 12, 2, 12), "pt")
    )

  bar_plot <- ggplot2::ggplot() +
    ggplot2::annotate(
      "rect",
      xmin = 0, xmax = 50,
      ymin = 0.06, ymax = 1.10,
      fill = grDevices::adjustcolor(risk_blue, alpha.f = 0.88),
      color = "black", linewidth = 0.6
    ) +
    ggplot2::annotate(
      "rect",
      xmin = 50, xmax = 100,
      ymin = 0.06, ymax = 1.10,
      fill = grDevices::adjustcolor(risk_red, alpha.f = 0.88),
      color = "black", linewidth = 0.6
    ) +
    ggplot2::annotate("text", x = 25, y = 0.57, label = "Lower probability", fontface = "bold", color = "white", size = 4.8) +
    ggplot2::annotate("text", x = 75, y = 0.57, label = "Higher probability", fontface = "bold", color = "white", size = 4.8) +
    ggplot2::annotate(
      "segment",
      x = risk_pct, xend = risk_pct,
      y = 0.02, yend = 1.16,
      linewidth = 2.5, color = "black"
    ) +
    ggplot2::annotate(
      "text",
      x = risk_pct,
      y = 1.31,
      label = sprintf("Patient (probability = %.1f%%)", risk_pct),
      fontface = "bold",
      size = 4
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1.34), expand = c(0, 0)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = grid::unit(c(0, 12, 8, 12), "pt")
    )

  ggpubr::ggarrange(
    .addBackGround(p),
    bar_plot,
    ncol = 1,
    heights = c(5.2, 1.15),
    align = "hv"
  )
}

model_path <- "glmnet/glmnet_caret_model.rds"
artifact_path <- "glmnet/glmnet_artifacts.rds"

if (!file.exists(model_path) || !file.exists(artifact_path)) {
  stop("Missing glmnet artifacts. Run `Rscript 6_cracked.R` first.")
}

model_fit <- readRDS(model_path)
artifacts <- readRDS(artifact_path)

feature_names <- artifacts$feature_names
feature_means <- artifacts$feature_means[feature_names]
feature_means[is.na(feature_means)] <- 0
feature_medians <- artifacts$feature_medians[feature_names]
feature_medians[is.na(feature_medians)] <- feature_means[is.na(feature_medians)]

if (is.null(feature_names) || length(feature_names) == 0) {
  stop("No feature metadata found in glmnet artifacts.")
}

class_levels <- artifacts$class_levels %||% c("No_Narrowing", "Has_Narrowing")
neg_class <- artifacts$neg_class %||% class_levels[1]
pos_class <- artifacts$pos_class %||% class_levels[2]
model_p_value <- suppressWarnings(as.numeric(artifacts$model_p_value %||% NA_real_))

input_spec <- artifacts$input_spec %||% list(
  sex = list(levels = c("female", "male"), male_label = "male"),
  fasting_sugar = list(levels = c("<6.7mM", ">6.7mM"), high_label = normalize_text(">6.7mM")),
  chest_pain = list(levels = c("typical angina", "atypical angina", "non-anginal pain", "asymptomatic")),
  resting_ecg = list(levels = c("normal", "ST-T wave abnormality", "left ventricular hypertrophy")),
  exercise_angina = list(levels = c("no", "yes"), yes_label = "yes")
)

feature_map <- artifacts$feature_map %||% list(
  age = "age",
  sex_male = "sex_male",
  resting_bp = "resting_bp",
  cholestoral = "cholestoral",
  fasting_sugar_high = "fasting_sugar_high",
  max_hr = "max_hr",
  exercise_angina_yes = "exercise_angina_yes",
  exercise_st_depression = "exercise_st_depression",
  chest_pain = c(
    "typical angina" = "chest_pain_typical_angina",
    "atypical angina" = "chest_pain_atypical_angina",
    "non-anginal pain" = "chest_pain_non_anginal_pain",
    "asymptomatic" = "chest_pain_asymptomatic"
  ),
  resting_ecg = c(
    "normal" = "resting_ecg_normal",
    "st-t wave abnormality" = "resting_ecg_st_t_wave_abnormality",
    "left ventricular hypertrophy" = "resting_ecg_left_ventricular_hypertrophy"
  )
)

input_spec$sex$male_label <- normalize_text(input_spec$sex$male_label %||% "male")
input_spec$fasting_sugar$high_label <- normalize_text(input_spec$fasting_sugar$high_label %||% ">6.7mM")
input_spec$exercise_angina$yes_label <- normalize_text(input_spec$exercise_angina$yes_label %||% "yes")

chest_feature_map <- normalize_named_vector(feature_map$chest_pain)
ecg_feature_map <- normalize_named_vector(feature_map$resting_ecg)

extract_glmnet_coefficients <- function(model, features) {
  best_lambda <- model$bestTune$lambda
  coef_mat <- as.matrix(coef(model$finalModel, s = best_lambda))

  intercept <- if ("(Intercept)" %in% rownames(coef_mat)) {
    as.numeric(coef_mat["(Intercept)", 1])
  } else {
    0
  }

  coef_vec <- setNames(rep(0, length(features)), features)
  available <- setdiff(rownames(coef_mat), "(Intercept)")
  matched <- intersect(available, features)
  coef_vec[matched] <- as.numeric(coef_mat[matched, 1])

  list(intercept = intercept, coefficients = coef_vec)
}

coef_info <- extract_glmnet_coefficients(model_fit, feature_names)

feature_default <- function(feature_key, fallback = 0) {
  col <- feature_map[[feature_key]]
  if (is.null(col) || length(col) != 1 || !(col %in% feature_names)) {
    return(fallback)
  }
  v <- feature_medians[[col]]
  if (is.null(v) || is.na(v) || !is.finite(v)) v <- feature_means[[col]]
  if (is.null(v) || is.na(v) || !is.finite(v)) v <- fallback
  as.numeric(v)
}

build_feature_vector <- function(raw_input) {
  values <- setNames(as.numeric(feature_means[feature_names]), feature_names)
  values[is.na(values) | !is.finite(values)] <- 0

  assign_feature <- function(key, value) {
    col <- feature_map[[key]]
    if (!is.null(col) && length(col) == 1 && col %in% names(values)) {
      values[[col]] <<- as.numeric(value)
    }
  }

  assign_feature("age", raw_input$age)
  assign_feature("resting_bp", raw_input$resting_bp)
  assign_feature("cholestoral", raw_input$cholestoral)
  assign_feature("max_hr", raw_input$max_hr)

  assign_feature("sex_male", as.integer(normalize_text(raw_input$sex) == input_spec$sex$male_label))
  assign_feature("fasting_sugar_high", as.integer(normalize_text(raw_input$fasting_sugar) == input_spec$fasting_sugar$high_label))
  assign_feature("exercise_angina_yes", as.integer(normalize_text(raw_input$exercise_angina) == input_spec$exercise_angina$yes_label))
  assign_feature("exercise_st_depression", as.numeric(raw_input$exercise_st_depression))

  if (length(chest_feature_map) > 0) {
    for (col in unname(chest_feature_map)) {
      if (col %in% names(values)) values[[col]] <- 0
    }
    selected_chest <- chest_feature_map[[normalize_text(raw_input$chest_pain)]]
    if (length(selected_chest) == 1 && selected_chest %in% names(values)) {
      values[[selected_chest]] <- 1
    }
  }

  if (length(ecg_feature_map) > 0) {
    for (col in unname(ecg_feature_map)) {
      if (col %in% names(values)) values[[col]] <- 0
    }
    selected_ecg <- ecg_feature_map[[normalize_text(raw_input$resting_ecg)]]
    if (length(selected_ecg) == 1 && selected_ecg %in% names(values)) {
      values[[selected_ecg]] <- 1
    }
  }

  values
}

sex_levels <- input_spec$sex$levels %||% c("female", "male")
fasting_levels <- input_spec$fasting_sugar$levels %||% c("<6.7mM", ">6.7mM")
chest_levels <- input_spec$chest_pain$levels %||% c("typical angina", "atypical angina", "non-anginal pain", "asymptomatic")
ecg_levels <- input_spec$resting_ecg$levels %||% c("normal", "ST-T wave abnormality", "left ventricular hypertrophy")
exercise_angina_levels <- input_spec$exercise_angina$levels %||% c("no", "yes")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .result-card {
        background: white;
        border: 1px solid #d9d9d9;
        border-radius: 10px;
        padding: 14px;
        margin-bottom: 14px;
        width: 100%;
        height: 100%;
      }
      .result-card h4 {
        margin-top: 0;
        margin-bottom: 10px;
        font-size: 1.72rem;
      }
      .results-wrap {
        width: 100%;
        max-width: 1120px;
        margin: 0 auto;
      }
      .results-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 12px;
        align-items: stretch;
      }
      .app-hero {
        text-align: center;
        margin-bottom: 12px;
      }
      .app-title {
        font-size: 2.35rem;
        font-weight: 800;
        letter-spacing: 0.04em;
        line-height: 1.05;
        margin: 0;
      }
      .app-subtitle {
        font-size: 1.08rem;
        line-height: 1.35;
        color: #4d4d4d;
        margin: 4px 0 0 0;
      }
      .top-layout {
        display: grid;
        grid-template-columns: minmax(0, 1120px);
        gap: 6px;
        align-items: stretch;
        justify-content: center;
        margin: 0 auto 12px auto;
      }
      .input-strip {
        background: white;
        border: 1px solid #d9d9d9;
        border-radius: 10px;
        padding: 8px 10px;
        margin: 0;
      }
      .input-strip h4 {
        margin-top: 0;
        margin-bottom: 6px;
        font-size: 1.36rem;
      }
      .input-grid {
        display: grid;
        grid-template-columns: repeat(5, minmax(0, 1fr));
        gap: 8px;
        align-items: start;
      }
      .input-strip .form-group {
        margin-bottom: 5px;
      }
      .input-strip .control-label {
        font-size: 1.08rem;
        margin-bottom: 2px;
      }
      .input-strip .form-control {
        height: 36px;
        padding: 5px 8px;
        font-size: 1.08rem;
      }
      .action-side {
        grid-column: 1;
        grid-row: 3;
        display: flex;
        justify-content: center;
      }
      .analyze-btn {
        width: 230px;
        min-height: 46px;
        font-size: 1.24rem;
        font-weight: 700;
      }
      .result-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 16px;
        margin-bottom: 12px;
      }
      .prediction-box {
        font-size: 1.2rem;
        white-space: pre-line;
        margin-bottom: 14px;
      }
      .report-body p, .report-body li {
        font-size: 1.22rem;
        line-height: 1.5;
      }
      .report-body ul {
        padding-left: 22px;
        margin-bottom: 12px;
      }
      @media (max-width: 1200px) {
        .input-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
      }
      @media (max-width: 768px) {
        .results-wrap {
          max-width: 100%;
        }
        .results-grid {
          grid-template-columns: 1fr;
        }
        .top-layout {
          grid-template-columns: 1fr;
        }
        .action-side {
          grid-column: 1;
          grid-row: auto;
          justify-content: center;
        }
        .analyze-btn {
          min-height: 56px;
          width: 100%;
        }
        .input-grid {
          grid-template-columns: 1fr;
        }
      }
    "))
  ),
  tags$div(
    class = "app-hero",
    tags$h1(class = "app-title", "CLEAR"),
    tags$p(
      class = "app-subtitle",
      "Care Logic for Exercise Assessment and Referral machine learning model"
    )
  ),
  tags$div(
    class = "top-layout",
    tags$div(
      class = "input-strip",
      style = "grid-column: 1; grid-row: 1;",
      tags$h4("At Presentation"),
      tags$div(
        class = "input-grid",
        div(numericInput("age", "Age", value = 60, min = 0, max = 120, step = 1)),
        div(selectInput("sex", "Sex", choices = format_choice_vector(sex_levels), selected = sex_levels[1])),
        div(numericInput("cholestoral", "Cholesterol (mg/dL)", value = 240, min = 50, max = 700, step = 1)),
        div(selectInput("fasting_sugar", "Fasting sugar", choices = format_choice_vector(fasting_levels), selected = fasting_levels[1])),
        div(selectInput("chest_pain", "Chest pain", choices = format_choice_vector(chest_levels), selected = chest_levels[1]))
      )
    ),
    tags$div(
      class = "input-strip",
      style = "grid-column: 1; grid-row: 2;",
      tags$h4("Testing"),
      tags$div(
        class = "input-grid",
        div(numericInput("resting_bp", "Resting BP (mmHg)", value = feature_default("resting_bp", 130), min = 50, max = 300, step = 1)),
        div(selectInput("resting_ecg", "Resting ECG", choices = format_choice_vector(ecg_levels), selected = ecg_levels[1])),
        div(numericInput("max_hr", "Max HR (bpm)", value = 180, min = 40, max = 260, step = 1)),
        div(selectInput(
          "exercise_angina",
          "Exercise angina",
          choices = format_choice_vector(exercise_angina_levels),
          selected = exercise_angina_levels[if (feature_default("exercise_angina_yes", 0) >= 0.5) 2 else 1]
        )),
        div(numericInput(
          "exercise_st_depression",
          "Exercise ST depression (mV)",
          value = 0,
          min = 0,
          max = 10,
          step = 0.1
        ))
      )
    ),
    tags$div(
      class = "action-side",
      actionButton("predict_btn", "Analyze", class = "btn-primary analyze-btn")
    )
  ),
  conditionalPanel(
    condition = "input.predict_btn > 0",
    tags$div(
      class = "results-wrap",
      tags$div(
        class = "results-grid",
        tags$div(
          class = "result-card",
          tags$div(
            class = "result-header",
            tags$h4("SHAP Waterfall"),
            downloadButton("download_shap_plot", "Export SHAP Image")
          ),
          # plotOutput("shap_plot", height = "490px")
          plotOutput("shap_plot", height = "450px")
        ),
        tags$div(
          class = "result-card",
          tags$h4("Clinical Report"),
          div(class = "report-body", uiOutput("shap_report"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  parsed_input <- eventReactive(input$predict_btn, {
    raw_input <- list(
      age = as.numeric(input$age),
      sex = input$sex,
      resting_bp = as.numeric(input$resting_bp),
      cholestoral = as.numeric(input$cholestoral),
      fasting_sugar = input$fasting_sugar,
      chest_pain = input$chest_pain,
      resting_ecg = input$resting_ecg,
      max_hr = as.numeric(input$max_hr),
      exercise_angina = input$exercise_angina,
      exercise_st_depression = as.numeric(input$exercise_st_depression)
    )

    values <- build_feature_vector(raw_input)
    if (any(is.na(values)) || any(!is.finite(values))) {
      return(list(error = "Some inputs could not be converted to valid numeric model features."))
    }

    new_data <- as.data.frame(as.list(values), check.names = FALSE)
    pred_prob <- predict(model_fit, newdata = new_data, type = "prob")
    positive_prob <- as.numeric(pred_prob[[pos_class]][1])

    shap_values <- coef_info$coefficients * (values - feature_means)
    shap_df <- data.frame(
      feature = names(shap_values),
      value = as.numeric(values),
      baseline = as.numeric(feature_means),
      contribution = as.numeric(shap_values),
      stringsAsFactors = FALSE
    )
    shap_df <- shap_df[order(abs(shap_df$contribution), decreasing = TRUE), , drop = FALSE]
    shap_df$feature_clean <- vapply(shap_df$feature, pretty_feature_name, character(1))

    expected_logit <- coef_info$intercept + sum(coef_info$coefficients * feature_means)
    sample_logit <- coef_info$intercept + sum(coef_info$coefficients * values)

    list(
      raw_input = raw_input,
      positive_prob = positive_prob,
      expected_prob = plogis(expected_logit),
      sample_prob = plogis(sample_logit),
      expected_logit = expected_logit,
      sample_logit = sample_logit,
      shap = shap_df,
      error = NULL
    )
  })

  output$prediction_text <- renderText({
    req(input$predict_btn)
    result <- parsed_input()
    if (!is.null(result$error)) {
      return(result$error)
    }

    pval_text <- if (is.finite(model_p_value)) {
      sprintf("Model p-value: %.3g", model_p_value)
    } else {
      "Model p-value: not available (rerun 6_cracked.R)"
    }

    sprintf(
      "Predicted narrowing: %.2f%%\n%s",
      100 * result$positive_prob,
      pval_text
    )
  })

  output$shap_plot <- renderPlot({
    req(input$predict_btn)
    result <- parsed_input()

    if (!is.null(result$error)) {
      plot.new()
      text(0.5, 0.5, result$error, cex = 1.1)
      return()
    }

    p <- build_shap_plot(result)
    if (is.null(p)) {
      plot.new()
      text(0.5, 0.5, "No SHAP contributions available.", cex = 1.1)
      return()
    }

    p
  })

  output$download_shap_plot <- downloadHandler(
    filename = function() {
      paste0("shap_waterfall_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1800, height = 1200, res = 200)
      result <- tryCatch(parsed_input(), error = function(e) NULL)
      if (is.null(result) || !is.null(result$error)) {
        plot.new()
        text(0.5, 0.5, "No SHAP plot available.", cex = 1.1)
      } else {
        p <- build_shap_plot(result)
        if (is.null(p)) {
          plot.new()
          text(0.5, 0.5, "No SHAP plot available.", cex = 1.1)
        } else {
          print(p)
        }
      }
      dev.off()
    }
  )

  report_data <- reactive({
    req(input$predict_btn)
    result <- parsed_input()
    if (!is.null(result$error)) {
      return(NULL)
    }
    if (nrow(result$shap) == 0) {
      return(NULL)
    }

    filtered_df <- result$shap[abs(result$shap$contribution) > 1e-10, , drop = FALSE]
    if (nrow(filtered_df) == 0) {
      return(NULL)
    }
    cutoff <- max(0.03, 0.15 * max(abs(filtered_df$contribution)))
    important_df <- filtered_df[abs(filtered_df$contribution) >= cutoff, , drop = FALSE]
    if (nrow(important_df) == 0) important_df <- filtered_df[seq_len(min(4, nrow(filtered_df))), , drop = FALSE]
    important_df <- important_df[order(abs(important_df$contribution), decreasing = TRUE), , drop = FALSE]
    top_n <- min(8, nrow(important_df))
    top_df <- important_df[seq_len(top_n), , drop = FALSE]
    top_df$feature_label <- vapply(top_df$feature, pretty_feature_name, character(1))

    positive_idx <- which(top_df$contribution > 0)
    negative_idx <- which(top_df$contribution < 0)
    strongest_up <- if (length(positive_idx) > 0) {
      i <- positive_idx[which.max(top_df$contribution[positive_idx])]
      top_df$feature_label[i]
    } else {
      "No strong upward factors"
    }
    strongest_down <- if (length(negative_idx) > 0) {
      i <- negative_idx[which.min(top_df$contribution[negative_idx])]
      top_df$feature_label[i]
    } else {
      "No strong downward factors"
    }

    list(
      result = result,
      top_df = top_df,
      strongest_up = strongest_up,
      strongest_down = strongest_down
    )
  })

  output$shap_report <- renderUI({
    rd <- report_data()
    if (is.null(rd)) {
      return(NULL)
    }

    result <- rd$result
    top_df <- rd$top_df
    raw_input <- result$raw_input

    toward_idx <- which(top_df$contribution > 0)
    away_idx <- which(top_df$contribution < 0)

    clinical_information_text <- sprintf(
      "%s year old %s patient presenting today with %s. Cholesterol %s mg/dL and fasting sugar %s.",
      format_age_years(raw_input$age),
      format_field_value(raw_input$sex),
      format_chest_pain_presentation(raw_input$chest_pain),
      format_field_value(raw_input$cholestoral),
      format_field_value(raw_input$fasting_sugar)
    )

    testing_findings_text <- sprintf(
      "At rest: BP %s mmHg, %s. During exercise, max HR achieved was %s bpm, exercise angina was %s, and %s.",
      format_field_value(raw_input$resting_bp),
      format_ecg_finding(raw_input$resting_ecg),
      format_field_value(raw_input$max_hr),
      format_exercise_angina_status(raw_input$exercise_angina),
      format_st_depression_finding(raw_input$exercise_st_depression)
    )

    toward_factor_text <- collapse_report_factor_text(
      top_df,
      toward_idx,
      raw_input,
      "Factors pushing toward suspicion of narrowing are"
    )
    away_factor_text <- collapse_report_factor_text(
      top_df,
      away_idx,
      raw_input,
      "Factors pushing away from suspicion of narrowing are"
    )

    tags$div(
      style = "background: white;",
      tags$p(style = "font-size: 1.25rem; font-weight: 700;", "CLINICAL INFORMATION"),
      tags$p(clinical_information_text),
      tags$p(style = "font-size: 1.25rem; font-weight: 700;", "FINDINGS"),
      tags$p(testing_findings_text),
      tags$p(
        "Based on the CLEAR glmnet model, Estimated narrowing probability: ",
        sprintf("%.2f%%", 100 * result$positive_prob),
        ". Model p-value: ",
        format_model_p_value(model_p_value),
        "."
      ),
      # Legacy detailed SHAP wording. Uncomment this block if you want the
      # one-line-per-factor odds-ratio version back in the clinical report.
      # tags$p("Factors pushing toward suspicion of narrowing:"),
      # tags$ul(
      #   if (length(toward_idx) > 0) {
      #     lapply(toward_idx, function(i) {
      #       tags$li(
      #         sprintf(
      #           "SHAP value of %s indicates an odds ratio of %s.",
      #           sprintf("%s (%+.3f)", top_df$feature_label[i], top_df$contribution[i]),
      #           format_odds_multiplier(top_df$contribution[i])
      #         )
      #       )
      #     })
      #   } else {
      #     tags$li("none")
      #   }
      # ),
      # tags$p("Factors pushing away from suspicion of narrowing:"),
      # tags$ul(
      #   if (length(away_idx) > 0) {
      #     lapply(away_idx, function(i) {
      #       tags$li(
      #         sprintf(
      #           "SHAP value of %s indicates an odds ratio of %s.",
      #           sprintf("%s (%+.3f)", top_df$feature_label[i], top_df$contribution[i]),
      #           format_odds_multiplier(top_df$contribution[i])
      #         )
      #       )
      #     })
      #   } else {
      #     tags$li("none")
      #   }
      # ),
      tags$p(toward_factor_text),
      tags$p(away_factor_text),
      tags$p(style = "font-size: 1.25rem; font-weight: 700;", "IMPRESSION"),
      tags$p(
        "Estimated narrowing probability: ",
        sprintf("%.2f%%", 100 * result$positive_prob),
        ". Model p-value: ",
        format_model_p_value(model_p_value),
        "."
      ),
      tags$p(style = "font-size: 1.25rem; font-weight: 700;", "INTERPRETATION"),
      tags$p(
        paste(
          interpret_model_p_value(model_p_value),
          recommend_imaging_text(result$positive_prob),
          "Use clinical judgment if the clinical picture disagrees with the model output."
        )
      )
    )
  })
}

shinyApp(ui = ui, server = server)
