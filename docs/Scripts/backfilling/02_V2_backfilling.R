#' Overview
#' ========
#' 
#' V2 post-selection backfilling.
#' 
#' Note: This script is dependent on 02_V2_source_selection.R and uses
#' monthly cached RF values from Output/RF/empirical_rf_latest.csv.
#'
#' Delay alignment: RF tables use the same delay definitions as
#' 00_FUN_dengue_rf_pipeline.R (monthly sources: round(days/30.44) from month
#' start; PAHO: weekly delay, using observed week delay when present else weeks
#' since month start). This matches the RF artifact, not necessarily the raw
#' `d` column from WHO downloads. Optional env: PIPELINE_REF_DATE=YYYY-MM-DD.
#' 

## Set up 
library(ggplot2)
library(countrycode)
library(readr)
library(dplyr)
# Functions: 
source("Scripts/backfilling/FUNCTIONS/00_FUN_dengue_rf_pipeline.R")


if (!exists("log_message")) {
  source("Scripts/utils/logging.R")
  ensure_logger(console = TRUE)
}

if (!exists("record_countries_at_step")) {
  source("Scripts/utils/country_tracking.R")
}

log_message("Running 02_V2_backfilling.R")

if (!exists("current_data")) {
  stop("current_data not found. Run Scripts/data_sourcing/02_V2_source_selection.R before backfilling.")
}

rf_path <- file.path("Output", "RF", "empirical_rf_latest.csv")

# Reference "as of" date for delay indexing (align with RF pipeline where possible).
# Optional: set PIPELINE_REF_DATE=YYYY-MM-DD for reproducibility.
ref_date <- {
  rd <- Sys.getenv("PIPELINE_REF_DATE", unset = "")
  if (nzchar(rd)) {
    tryCatch(as.Date(rd), error = function(e) Sys.Date())
  } else {
    Sys.Date()
  }
}

log_message("Backfill reference date (delay indexing): " %+% as.character(ref_date))

# Same month-delay definition as .calculate_months_diff() in 00_FUN_dengue_rf_pipeline.R
.calc_rf_delay_months <- function(year, month, ext_date) {
  data_date <- as.Date(sprintf("%04d-%02d-01", as.integer(year), as.integer(month)))
  as.integer(round(as.numeric(ext_date - data_date) / 30.44))
}

# PAHO RF table uses weekly delay; monthly grid rows may only have month start — use weeks since month start.
.calc_rf_delay_weeks_month_start <- function(year, month, ext_date) {
  data_date <- as.Date(sprintf("%04d-%02d-01", as.integer(year), as.integer(month)))
  as.integer(round(as.numeric(difftime(ext_date, data_date, units = "weeks"))))
}

# Keep a copy of unadjusted values for provenance.
current_data <- current_data %>%
  mutate(cases_raw = cases)

if (!file.exists(rf_path)) {
  log_message("RF cache not found at " %+% rf_path %+% ". Using fallback RF=1 for all rows.", level = "WARNING")
  
  current_data <- current_data %>%
    mutate(
      source_rf = dplyr::case_when(
        source == "WHO" ~ "WHO_Global",
        TRUE ~ source
      ),
      rf_time_resolution = dplyr::case_when(
        source == "PAHO" ~ "weekly",
        source == "SEARO" ~ "monthly",
        source == "WHO" ~ "monthly",
        TRUE ~ NA_character_
      ),
      rf_delay_d = dplyr::case_when(
        source == "PAHO" & !is.na(d) & !is.na(d_unit) & d_unit == "week" ~ as.integer(round(d)),
        source == "PAHO" ~ .calc_rf_delay_weeks_month_start(Year, Month, ref_date),
        TRUE ~ .calc_rf_delay_months(Year, Month, ref_date)
      ),
      rf_used = 1,
      rf_available = FALSE,
      rf_fallback = !is.na(cases_raw),
      backfill_applied = FALSE,
      backfill_note = dplyr::case_when(
        is.na(cases_raw) ~ "no_cases_to_adjust",
        TRUE ~ "rf_cache_missing_fallback_1"
      )
    )
} else {
  rf_tbl <- readr::read_csv(rf_path, show_col_types = FALSE)

  if ("iso3" %in% names(rf_tbl)) {
    rf_tbl$iso3_rf <- rf_tbl$iso3
  } else {
    rf_tbl$iso3_rf <- countrycode::countrycode(
      rf_tbl$country,
      origin = "country.name",
      destination = "iso3c",
      custom_match = c(
        "Saint Martin (French part)" = "MAF",
        "Saint Martin" = "MAF"
      )
    )
  }

  rf_data <- rf_tbl %>%
    dplyr::mutate(
      source_rf = .data$source,
      rf_delay_d = as.integer(round(.data$d)),
      rf_time_resolution = .data$time_resolution
    ) %>%
    dplyr::filter(
      !is.na(.data$source_rf),
      !is.na(.data$iso3_rf),
      !is.na(.data$rf_delay_d),
      !is.na(.data$rf_time_resolution)
    ) %>%
    dplyr::distinct(
      .data$source_rf,
      .data$iso3_rf,
      .data$rf_delay_d,
      .data$rf_time_resolution,
      .keep_all = TRUE
    ) %>%
    dplyr::select(
      source_rf,
      iso3_rf,
      rf_delay_d,
      rf_time_resolution,
      dplyr::any_of(c("mean_rf", "median_rf"))
    )

  current_data <- current_data %>%
    dplyr::mutate(
      source_rf = dplyr::case_when(
        source == "WHO" ~ "WHO_Global",
        TRUE ~ source
      ),
      rf_time_resolution = dplyr::case_when(
        source == "PAHO" ~ "weekly",
        source == "SEARO" ~ "monthly",
        source == "WHO" ~ "monthly",
        TRUE ~ NA_character_
      ),
      # Delay index for RF join: must match RF artifact (not necessarily raw `d` from WHO download).
      rf_delay_d = dplyr::case_when(
        source == "PAHO" & !is.na(d) & !is.na(d_unit) & d_unit == "week" ~ as.integer(round(d)),
        source == "PAHO" ~ .calc_rf_delay_weeks_month_start(Year, Month, ref_date),
        TRUE ~ .calc_rf_delay_months(Year, Month, ref_date)
      )
    ) %>%
    dplyr::left_join(
      rf_data,
      by = c(
        "source_rf",
        "iso3" = "iso3_rf",
        "rf_delay_d",
        "rf_time_resolution"
      )
    ) %>%
    dplyr::mutate(
      rf_candidate = dplyr::coalesce(.data$median_rf, .data$mean_rf),
      rf_valid = !is.na(.data$rf_candidate) & .data$rf_candidate > 0 & is.finite(.data$rf_candidate),
      rf_used = dplyr::if_else(.data$rf_valid, .data$rf_candidate, 1),
      rf_available = .data$rf_valid,
      rf_fallback = !is.na(.data$cases_raw) & !.data$rf_valid,
      cases = dplyr::if_else(
        !is.na(.data$cases_raw),
        round(.data$cases_raw * .data$rf_used, 0),
        NA_real_
      ),
      backfill_applied = !is.na(.data$cases_raw) & .data$rf_used != 1,
      backfill_note = dplyr::case_when(
        is.na(.data$cases_raw) ~ "no_cases_to_adjust",
        .data$rf_valid ~ "rf_applied",
        TRUE ~ "rf_missing_or_invalid_fallback_1"
      )
    ) %>%
    dplyr::select(-dplyr::any_of(c("rf_candidate", "rf_valid", "mean_rf", "median_rf")))
}

if (exists("log_message")) {
  n_rows <- nrow(current_data)
  n_non_missing_cases <- sum(!is.na(current_data$cases_raw), na.rm = TRUE)
  n_backfill_applied <- sum(current_data$backfill_applied, na.rm = TRUE)
  n_fallback <- sum(current_data$rf_fallback, na.rm = TRUE)
  
  log_message("Backfilling rows processed: " %+% n_rows)
  log_message("Rows with observed cases: " %+% n_non_missing_cases)
  log_message("Rows with RF applied: " %+% n_backfill_applied)
  log_message("Rows using fallback RF=1: " %+% n_fallback)
}

# Record country presence after backfilling stage.
if (exists("record_countries_at_step")) {
  tryCatch({
    current_countries <- current_data %>%
      dplyr::select(country, iso3) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(iso3))
    
    record_countries_at_step(current_countries, "Step_4c_Post_Selection_Backfill")
  }, error = function(e) {
    if (exists("log_message")) {
      log_message("Warning: Country tracking failed at Step_4c_Post_Selection_Backfill: " %+% conditionMessage(e), level = "WARNING")
    }
  })
}
