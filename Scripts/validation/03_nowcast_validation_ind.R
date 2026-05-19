# =============================================================================
# 03_nowcast_validation_ind.R
# Retrospective LOSO validation — row-level predictions and errors
# =============================================================================
# Reads `full_data_season_monthly_proportions.csv` from the latest dated
# pipeline run under Output/YYYY_MM_DD/ (written by V1_Pipeline.R Step 8).
# For each country with >= 3 complete seasons: hold out each season, fit the
# mean seasonal profile on the remaining seasons, apply cutoffs k = 1..11,
# and compare predicted vs actual monthly cases for months after k.
# Output: Output/validation/validation_detail.csv
# =============================================================================

library(tidyverse)

source("Scripts/validation/FUNCTIONS/00_FUN_validation_metrics.R")

# --- Constants ---------------------------------------------------------------
MIN_SEASONS <- 3L          # Minimum distinct seasons per country (methods)
MIN_TRAIN_SEASONS <- 2L    # After holding out one season, need >=2 training seasons

out_dir <- "Output/validation"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

# --- Locate latest pipeline run that saved the proportions table ------------
# Only top-level Output/YYYY_MM_DD directories are considered (same convention
# as snapshot scripts).
run_dirs <- list.dirs("Output", recursive = FALSE) %>%
  keep(~ str_detect(basename(.x), "^\\d{4}_\\d{2}_\\d{2}$")) %>%
  sort()

prop_paths <- file.path(run_dirs, "full_data_season_monthly_proportions.csv")
prop_existing <- prop_paths[file.exists(prop_paths)]

if (length(prop_existing) == 0) {
  stop(
    "No full_data_season_monthly_proportions.csv found under Output/YYYY_MM_DD/. ",
    "Run Scripts/V1_Pipeline.R first so Step 8 writes this file."
  )
}

prop_csv <- prop_existing[length(prop_existing)]
message("Using proportions file: ", prop_csv)

# --- Load season-monthly proportions + attach Region ------------------------
# One row per country × season × season_nMonth (12 months per complete season).
validation_data <- read_csv(prop_csv, show_col_types = FALSE) %>%
  dplyr::select(
    country, iso3, Year, season, season_nMonth, Month, cases,
    Actual_monthly_proportion, Actual_cum_monthly_proportion
  )

# Region labels from OpenDengue national file (stable asset; avoids re-sourcing PAHO/WHO).
region_lookup <- read_csv(
  "Assets/Stable/OD_maps/pred_downscale_with_ci_V3.csv",
  show_col_types = FALSE
) %>%
  transmute(
    iso3 = ISO_A0,
    Region = od_region
  ) %>%
  distinct(iso3, .keep_all = TRUE)

validation_data <- validation_data %>%
  left_join(region_lookup, by = "iso3")

# --- LOSO: outer = country, middle = held-out season, inner = cutoff k -----
# Emits one row per prediction (future month) with errors.
validation_detail <- validation_data %>%
  group_by(iso3) %>%
  filter(n_distinct(season) >= MIN_SEASONS) %>%
  group_split() %>%
  map_dfr(function(country_df) {
    # Stable labels for this ISO3 (avoid length(unique(.)) > 1 if names drift)
    iso3_i <- dplyr::first(country_df$iso3)
    country_i <- dplyr::first(country_df$country)
    region_i <- dplyr::first(country_df$Region)

    seasons <- sort(unique(country_df$season))

    map_dfr(seasons, function(s) {
      test_df <- country_df %>% filter(season == s)
      train_df <- country_df %>% filter(season != s)

      # Need at least two training seasons to estimate a mean profile
      if (dplyr::n_distinct(train_df$season) < MIN_TRAIN_SEASONS) {
        return(tibble())
      }

      baseline <- fit_baseline_profile(train_df)

      # Cutoffs 1..11: month 12 is never a cutoff (nothing left to predict)
      map_dfr(1:11, function(k) {
        nowcast_one_cutoff(test_df, baseline, k)
      }) %>%
        mutate(
          iso3 = iso3_i,
          country = country_i,
          Region = region_i,
          season = s
        )
    })
  })

if (nrow(validation_detail) == 0) {
  stop("Validation produced zero rows; check input data and filters.")
}

# --- Errors (methods): absolute, squared, relative --------------------------
# relative_error only defined when actual_cases > 0 (division by zero guard).
validation_detail <- validation_detail %>%
  mutate(
    absolute_error = predicted_cases - actual_cases,
    squared_error = absolute_error^2,
    relative_error = if_else(
      actual_cases > 0,
      (predicted_cases - actual_cases) / actual_cases,
      NA_real_
    )
  ) %>%
  dplyr::select(
    iso3, country, Region, season, cutoff_month, prediction_month, Month,
    actual_cases, predicted_total, predicted_cases,
    absolute_error, squared_error, relative_error
  )

write_csv(validation_detail, file.path(out_dir, "validation_detail.csv"))
message("Wrote ", file.path(out_dir, "validation_detail.csv"), " (nrow = ", nrow(validation_detail), ")")
