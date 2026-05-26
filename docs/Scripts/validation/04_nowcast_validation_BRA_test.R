library(dplyr)
library(readr)
library(stringr)

# Legacy helpers (full moving-window validation); kept for this ad hoc country test only.
source("Scripts/validation/FUNCTIONS/00_FUN_validation_metrics_legacy_BRA.R")

prep <- prepare_validation_dataset()
validation_data <- prep$validation_data

if (!is.data.frame(validation_data) || nrow(validation_data) == 0) {
  stop("Prepared validation_data is empty; cannot run Brazil test.")
}

filter_country <- function(df, iso3_code, country_label) {
  if ("iso3" %in% names(df)) {
    return(df %>% dplyr::filter(.data$iso3 == iso3_code))
  }
  if ("country" %in% names(df)) {
    return(df %>% dplyr::filter(stringr::str_to_lower(.data$country) == stringr::str_to_lower(country_label)))
  }
  stop("validation_data has neither `iso3` nor `country` columns; cannot filter to a country.")
}

run_country_validation <- function(validation_data, iso3_code, country_label, out_subdir) {
  out_dir <- file.path("Output", "validation", out_subdir)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  validation_country <- filter_country(validation_data, iso3_code, country_label)
  if (!is.data.frame(validation_country) || nrow(validation_country) == 0) {
    stop(sprintf("Filter produced zero rows for %s (%s).", country_label, iso3_code))
  }

  validation_results <- run_moving_window_validation(validation_country) # nolint
  if (!is.data.frame(validation_results) || nrow(validation_results) == 0) {
    stop(sprintf("%s moving-window validation produced zero rows.", country_label))
  }

  write_csv(validation_results, file.path(out_dir, sprintf("validation_results_detail_%s.csv", iso3_code)))

  metrics <- compute_error_metrics(validation_results) # nolint

  country_metrics <- metrics$by_country %>%
    dplyr::arrange(.data$RMSPE, .by_group = FALSE)
  write_csv(country_metrics, file.path(out_dir, sprintf("country_validation_summary_%s.csv", iso3_code)))

  if (!is.null(metrics$by_cutoff)) {
    write_csv(metrics$by_cutoff, file.path(out_dir, sprintf("cutoff_accuracy_summary_%s.csv", iso3_code)))
  }

  if (!is.null(metrics$by_country_cutoff)) {
    write_csv(metrics$by_country_cutoff, file.path(out_dir, sprintf("country_cutoff_detail_%s.csv", iso3_code)))
  }

  invisible(list(results = validation_results, metrics = metrics))
}

run_country_validation(validation_data, "BRA", "Brazil", "brazil_test")
run_country_validation(validation_data, "CUB", "Cuba", "cuba_test")

