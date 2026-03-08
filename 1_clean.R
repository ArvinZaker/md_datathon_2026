colnameConversion <- list(
  "age" = c("skip"),
  "sex" = c("1" = "male", "0" = "female"),
  "chest pain" = c(
    "1" = "typical angina",
    "2" = "atypical angina",
    "3" = "non-anginal pain",
    "4" = "asymptomatic"
  ),
  "resting bp" = c("skip"),
  "cholestoral" = c("skip"),
  "fasting sugar" = c("0" = "<6.7mM", "1" = ">6.7mM"),
  "resting ECG" = c(
    "0" = "normal",
    "1" = "ST-T wave abnormality",
    "2" = "left ventricular hypertrophy"
  ),
  "max HR" = c("skip"),
  "exercise angina" = c("1" = "yes", "0" = "no"),
  "exercise ST depression" = c("skip"),
  "slope ST segment" = c(
    "1" = "upsloping",
    "2" = "flat",
    "3" = "downsloping"
  ),
  "number of vessels flourosopy" = c("skip"),
  "thal" = c(
    "3" = "normal",
    "6" = "fixed defect",
    "7" = "reversable defect"
  ),
  "diagnosis" = c("0" = "<50% narrowing", "1" = ">50% narrowing")
)

clean_heart_dataset <- function(input_path, dataset_name = NULL, col_map = colnameConversion) {
  raw_columns <- c(
    "age", "sex", "cp", "trestbps", "chol", "fbs", "restecg",
    "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num"
  )

  clean_names <- vapply(seq_along(col_map), function(i) {
    nm <- names(col_map)[i]
    if (is.null(nm) || is.na(nm) || nm == "") as.character(col_map[[i]][1]) else nm
  }, character(1))

  raw <- read.csv(
    input_path,
    header = FALSE,
    col.names = raw_columns,
    na.strings = c("?", "-9.0"),
    stringsAsFactors = FALSE,
    strip.white = TRUE
  )

  for (nm in names(raw)) {
    raw[[nm]] <- suppressWarnings(as.numeric(raw[[nm]]))
  }

  cleaned <- raw
  names(cleaned) <- clean_names
  cleaned$cholestoral[cleaned$cholestoral == 0] <- NA

  is_skip <- vapply(col_map, function(x) {
    length(x) == 1 && identical(unname(x), "skip")
  }, logical(1))

  for (i in seq_along(col_map)) {
    if (is_skip[i]) next

    conv <- col_map[[i]]
    col <- clean_names[i]
    if (col == "diagnosis") {
      code <- ifelse(is.na(cleaned[[col]]), NA_character_, ifelse(cleaned[[col]] > 0, "1", "0"))
    } else {
      code <- ifelse(is.na(cleaned[[col]]), NA_character_, as.character(as.integer(cleaned[[col]])))
    }
    cleaned[[col]] <- unname(conv[code])
  }

  if (!is.null(dataset_name)) {
    cleaned$dataset <- dataset_name
  }
  cleaned
}

dataset_inputs <- c(
  cleveland = "data/heart/processed.cleveland.data",
  hungarian = "data/heart/processed.hungarian.data",
  switzerland = "data/heart/processed.switzerland.data",
  va = "data/heart/processed.va.data"
)

# 1) Cleveland only, fully cleaned, root output
cleavland_cleaned <- clean_heart_dataset(
  input_path = dataset_inputs[["cleveland"]],
  dataset_name = NULL,
  col_map = colnameConversion
)
cleavland_cleaned <- cleavland_cleaned[complete.cases(cleavland_cleaned), , drop = FALSE]

write.csv(cleavland_cleaned, "cleavland_cleaned.csv", row.names = FALSE)

# 2) All datasets merged, remove only thal + vessels, root output
all_cleaned_list <- list()

for (dataset_name in names(dataset_inputs)) {
  cleaned <- clean_heart_dataset(
    input_path = dataset_inputs[[dataset_name]],
    dataset_name = dataset_name,
    col_map = colnameConversion
  )
  all_cleaned_list[[dataset_name]] <- cleaned
}

all_cleaned <- do.call(rbind, all_cleaned_list)
write.csv(all_cleaned, "all.csv", row.names = FALSE)
# hist(all_cleaned2$cholestoral)
# table(all_cleaned2$cholestoral < 50)

### manual intervention
# table(all_cleaned$cholestoral == 0)
# zeros are treated as missing
# all_cleaned$"chest pain" <- NULL
###
all_cleaned$thal <- NULL
all_cleaned$`number of vessels flourosopy` <- NULL
all_cleaned2 <- all_cleaned
all_cleaned <- all_cleaned[complete.cases(all_cleaned), , drop = FALSE]
write.csv(all_cleaned, "all_cleaned.csv", row.names = FALSE)
all_cleaned2$`slope ST segment` <- NULL
# all_cleaned2$cholestoral <- NULL
# all_cleaned2$"chest pain" <- NULL
all_cleaned2 <- all_cleaned2[complete.cases(all_cleaned2), , drop = FALSE]
write.csv(all_cleaned2, "all_cleaned2.csv", row.names = FALSE)

message("Wrote cleavland_cleaned.csv and all_cleaned.csv in root")
