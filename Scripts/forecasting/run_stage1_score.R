#!/usr/bin/env Rscript

#' ---
#' title: "run_stage1_score"
#' author: "K M Susong"
#' ---
#'
#' Overview:
#' ========
#' Stage 1 ONLY scoring runner. 
#' 
#' Reads every `stage1_hindcast/forecasts/ <model>__<window_type>.csv` 
#' (from `run_stage1_hindcast.R`), scores every row against `actual`, 
#' joins strata + season, and writes the score tables the
#' plan's §5 outputs and the notebook's #sec-stage1 need. No model is fit or
#' refit here.
#'
#' See the 2026-09-14 design discussion for note on stage1 vs stage2 score runner. 
#'
#' Only rows with both `actual` and `.pred` observed are scored. Counts of 
#' dropped rows are logged.
#'
#' `unit_cols <- c(forecast_unit_cols, "window_type")` throughout (all windows'
#' forecasts are stacked into one call per table) - See `01_scoring.R`'s 
#' 14-09-2026 timeline entry.
#'
#' n_seasons: the number of distinct (iso3, season) pairs backing a score cell, 
#' not the raw row count (many monthly origins in
#' the same country-season are correlated evidence, not independent seasons).
#' `thin_evidence = n_seasons < 5`, flagged, not enforced (nothing is dropped 
#' for being thin).
#'
#' uMAE/uRMSE reuse `Scripts/validation/FUNCTIONS/00_FUN_validation_metrics.R`'s
#' `mean_actual_by_prediction_month()` for the burden denominator (GDO's
#' existing convention: errors scaled by mean historical cases in the *target*
#' month, not by that row's own actual) and `01_scoring.R`'s own
#' `normalise_by_burden()` (already built, generic) rather than pulling in
#' that file's `add_validation_error_columns()` wholesale - only the burden
#' reference is reused; `squared_error` is one line, not worth importing a
#' second column-renaming machinery for.
#'
#' Operational metrics (`peak_timing_diff`, `dtw_distance`) are trajectory-
#' level, not row-level: computed once per (model, window_type, iso3,
#' origin_date) across that origin's 1..6-month path, using the path's own
#' first target month for its season tag. No `lead_time` dimension for these
#' (a whole-trajectory metric has none) - `operational.csv` mirrors
#' `scores_by_stratum.csv`'s long shape minus the lead column.
#'
#' `dtw_distance()` (in `01_scoring.R`) returns `dtw::dtw()$normalizedDistance`,
#' not raw `$distance` - fixed there (not here) on 15-09-2026, found while
#' building this runner's operational.csv: raw distance scales with
#' trajectory length, and trajectories near the edge of the training panel
#' have fewer resolved months than a mid-history one, which inverted the real
#' quality signal when averaged. See 01_scoring.R's timeline for detail.
#'
#' Input : Output/forecasting/stage1_hindcast/forecasts/<model>__<window_type>.csv
#'         Output/forecasting/training_data/training_panel_<snapshot_date>.csv
#'         Output/forecasting/data_summary/strata_country.csv
#' Output: Output/forecasting/stage1_hindcast/scores/
#'           scores_row.csv, scores_by_lead.csv, scores_by_stratum.csv,
#'           leaderboard.csv, pit.csv, coverage.csv, operational.csv
#'
#' Timeline:
#' ========
#' 15-09-2026: Created.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(stringr)
  library(cli)
})

# ---- Locate project root ---------------------------------------
if (!dir.exists("Output") && dir.exists("../../Output")) setwd("../..")
if (!dir.exists("Output")) {
  cli::cli_abort("Run from the DENV_global_observatory repo root (cannot find {.path Output/}).")
}

source("Scripts/forecasting/00_config.R")
source("Scripts/forecasting/models/utils/forecast_helpers.R")
source("Scripts/forecasting/validation/01_scoring.R")
source("Scripts/validation/FUNCTIONS/00_FUN_validation_metrics.R")

hindcast_dir  <- file.path(forecast_out, "stage1_hindcast")
forecasts_dir <- file.path(hindcast_dir, "forecasts")
scores_dir    <- file.path(hindcast_dir, "scores")
dir.create(scores_dir, recursive = TRUE, showWarnings = FALSE)

unit_cols <- c(forecast_unit_cols, "window_type")

# ---- Load inputs -----------------------------------------------------------
#---check: at least one forecasts CSV exists (run run_stage1_hindcast.R if not)
forecast_files <- sort(list.files(forecasts_dir, pattern = "\\.csv$", full.names = TRUE))
if (length(forecast_files) == 0L) {
  cli::cli_abort(c(
    "No forecast files in {.path {forecasts_dir}}.",
    "i" = "Run {.path Scripts/forecasting/run_stage1_hindcast.R} first."
  ))
}

#' Parse `<model>__<window_type>.csv` out of a forecasts file's basename.
parse_forecast_filename <- function(path) {
  base <- tools::file_path_sans_ext(basename(path))
  parts <- str_match(base, "^(.+)__(.+)$")
  if (any(is.na(parts))) {
    cli::cli_abort("Can't parse model/window_type out of filename: {.path {path}}.")
  }
  list(model = parts[, 2], window_type = parts[, 3])
}

all_df <- purrr::map_dfr(forecast_files, function(path) {
  parsed <- parse_forecast_filename(path)
  df <- read_csv(path, show_col_types = FALSE) %>%
    dplyr::mutate(
      origin_date = as.Date(origin_date),
      target_date = as.Date(target_date)
    )
  # window_type is already a column in the file, but the filename is the
  # authoritative source (guards against a stale/renamed column).
  df$window_type <- parsed$window_type
  df
})
#---log: what got stacked
cli::cli_inform("Loaded {nrow(all_df)} forecast rows from {length(forecast_files)} file{?s}: {.val {basename(forecast_files)}}")

#---check: the pinned training panel exists
panel_path <- file.path(forecast_out, "training_data",
                        paste0("training_panel_", snapshot_date, ".csv"))
if (!file.exists(panel_path)) {
  cli::cli_abort(c(
    "Training panel not found: {.path {panel_path}}.",
    "i" = "Run {.path Scripts/forecasting/01_prepare_training_data.R} first."
  ))
}
panel <- read_csv(panel_path, show_col_types = FALSE) %>%
  dplyr::mutate(date = as.Date(date))

#---check: the strata table exists
strata_path <- file.path(forecast_out, "data_summary", "strata_country.csv")
if (!file.exists(strata_path)) {
  cli::cli_abort(c(
    "Strata table not found: {.path {strata_path}}.",
    "i" = "Run {.path Scripts/forecasting/02_data_summary.R} first."
  ))
}
strata_country <- read_csv(strata_path, show_col_types = FALSE) %>%
  dplyr::select(iso3, region, seasonality_signal, endemicity, size_class)

# ---- Drop rows that can't be scored -----------------------------------------
n_before <- nrow(all_df)
n_no_truth <- sum(is.na(all_df$actual))
n_no_pred  <- sum(is.na(all_df$.pred))
scorable <- all_df %>% dplyr::filter(!is.na(actual), !is.na(.pred))
#---log: how much of the grid is actually scorable, and why the rest isn't
cli::cli_inform(c(
  ">" = "{nrow(scorable)}/{n_before} rows scorable",
  ">" = "{n_no_truth} row{?s} with no truth yet (target_date beyond panel coverage)",
  ">" = "{n_no_pred} row{?s} with no prediction (model returned NA)"
))
#---check: something is left to score
if (nrow(scorable) == 0L) {
  cli::cli_abort("No scorable rows (every row is missing truth and/or a prediction).")
}

# ---- Season + burden reference (from the FULL historical panel) -----------
season_ref <- panel %>%
  dplyr::distinct(iso3, target_date = date, season, season_nMonth)

# Burden = mean historical cases in the TARGET month (GDO's u* convention -
# scaled by burden in the month being predicted, not the row's own actual).
burden_ref <- mean_actual_by_prediction_month(
  panel, month_col = "season_nMonth", cases_col = "cases", by_country = TRUE
)

# ---- Row-level scoring (Stage 1's own concern: window_type in the unit) ---
scores_row <- score_forecast(scorable, actual_col = "actual", unit_cols = unit_cols) %>%
  dplyr::left_join(
    dplyr::select(scorable, dplyr::all_of(unit_cols), .pred, actual),
    by = unit_cols
  ) %>%
  dplyr::mutate(lead_time = horizon, squared_error = (actual - .pred)^2) %>%
  dplyr::left_join(season_ref, by = c("iso3", "target_date")) %>%
  dplyr::left_join(strata_country, by = "iso3") %>%
  dplyr::mutate(prediction_month = season_nMonth) %>%
  dplyr::left_join(burden_ref, by = c("iso3", "prediction_month")) %>%
  dplyr::mutate(
    umae_row      = normalise_by_burden(ae_median, mean_actual_predicted_month),
    urmse_sq_row  = normalise_by_burden(squared_error, mean_actual_predicted_month^2)
  ) %>%
  dplyr::select(-prediction_month)

write_csv(scores_row, file.path(scores_dir, "scores_row.csv"))
#---log: confirm the row-level table was written, and its size
cli::cli_inform(c(">" = "wrote {.path {file.path(scores_dir, 'scores_row.csv')}} ({nrow(scores_row)} rows)"))

# ---- Aggregation helper (shared by scores_by_lead / scores_by_stratum) ----
#' Summarise `scores_row` over whatever `dplyr::group_by()` columns the
#' caller already applied - one set of metric formulas, reused for every
#' aggregation axis so the definitions can't drift between tables.
#' @param grouped A grouped `scores_row` (or subset), via `dplyr::group_by()`.
#' @return One row per group: n_obs, n_seasons, thin_evidence, then the
#'   primary/secondary metrics, mean/aggregated.
summarise_scores <- function(grouped) {
  grouped %>%
    dplyr::summarise(
      n_obs         = dplyr::n(),
      n_seasons     = dplyr::n_distinct(paste(iso3, season)),
      thin_evidence = n_seasons < 5,
      crps          = mean(crps, na.rm = TRUE),
      crps_log      = mean(crps_log, na.rm = TRUE),
      dispersion        = mean(dispersion, na.rm = TRUE),
      dispersion_log    = mean(dispersion_log, na.rm = TRUE),
      overprediction    = mean(overprediction, na.rm = TRUE),
      overprediction_log = mean(overprediction_log, na.rm = TRUE),
      underprediction    = mean(underprediction, na.rm = TRUE),
      underprediction_log = mean(underprediction_log, na.rm = TRUE),
      bias          = mean(bias, na.rm = TRUE),
      coverage_50   = mean(interval_coverage_50, na.rm = TRUE),
      coverage_90   = mean(interval_coverage_90, na.rm = TRUE),
      uMAE          = mean(umae_row, na.rm = TRUE),
      uRMSE         = sqrt(mean(urmse_sq_row, na.rm = TRUE)),
      .groups = "drop"
    )
}

# ---- scores_by_lead.csv: pooled across strata, by (model, window, lead) ---
scores_by_lead <- scores_row %>%
  dplyr::group_by(model, window_type, lead_time) %>%
  summarise_scores()
write_csv(scores_by_lead, file.path(scores_dir, "scores_by_lead.csv"))
#---log: confirm the by-lead table was written, and its size
cli::cli_inform(c(">" = "wrote {.path {file.path(scores_dir, 'scores_by_lead.csv')}} ({nrow(scores_by_lead)} rows)"))

# ---- scores_by_stratum.csv: lead x each stratum axis, long format ---------
stratum_axes <- c("seasonality_signal", "endemicity", "size_class")
scores_by_stratum <- purrr::map_dfr(stratum_axes, function(axis) {
  scores_row %>%
    dplyr::rename(stratum_value = dplyr::all_of(axis)) %>%
    dplyr::group_by(model, window_type, lead_time, stratum_value) %>%
    summarise_scores() %>%
    dplyr::mutate(stratum_axis = axis, .after = window_type)
})
write_csv(scores_by_stratum, file.path(scores_dir, "scores_by_stratum.csv"))
#---log: confirm the by-stratum table (all 3 axes, long format) was written, and its size
cli::cli_inform(c(">" = "wrote {.path {file.path(scores_dir, 'scores_by_stratum.csv')}} ({nrow(scores_by_stratum)} rows)"))

# ---- leaderboard.csv: relative CRPS vs each mandatory/candidate baseline --
# scoringutils' pairwise-comparison machinery still calls this metric "wis"
# internally (see relative_skill()'s docstring) - it's the CRPS-equivalent
# ratio at today's quantile resolution, not a separate WIS calculation.
# "prior best model" (the plan's third relative comparison) isn't yet
# distinct from `nowcast` - with only two models in the roster, nowcast IS
# the only earlier candidate. Add its own baseline once a third model exists;
# nothing here needs to change to support that.
candidate_baselines <- intersect(c("nowcast", "glm_ar1"), unique(scorable$model))
leaderboard <- purrr::map_dfr(candidate_baselines, function(bl) {
  if (dplyr::n_distinct(scorable$model) < 2L) return(NULL)
  relative_skill(
    scorable, baseline = bl, actual_col = "actual",
    by = c("window_type", "horizon"), log_scale = TRUE, unit_cols = unit_cols
  ) %>%
    dplyr::mutate(baseline = bl)
})
write_csv(leaderboard, file.path(scores_dir, "leaderboard.csv"))
#---log: confirm the relative-skill leaderboard was written, and its size
cli::cli_inform(c(">" = "wrote {.path {file.path(scores_dir, 'leaderboard.csv')}} ({nrow(leaderboard)} rows)"))

# ---- pit.csv / coverage.csv: calibration diagnostics, per model x window -
pit <- pit_histogram(scorable, actual_col = "actual", by = c("model", "window_type"), unit_cols = unit_cols)
write_csv(pit, file.path(scores_dir, "pit.csv"))
#---log: confirm the PIT histogram table was written, and its size
cli::cli_inform(c(">" = "wrote {.path {file.path(scores_dir, 'pit.csv')}} ({nrow(pit)} rows)"))

coverage <- coverage_diagnostics(scorable, actual_col = "actual", by = c("model", "window_type"), unit_cols = unit_cols)
write_csv(coverage, file.path(scores_dir, "coverage.csv"))
#---log: confirm the coverage diagnostics table was written, and its size
cli::cli_inform(c(">" = "wrote {.path {file.path(scores_dir, 'coverage.csv')}} ({nrow(coverage)} rows)"))

# ---- operational.csv: peak-timing + DTW, one trajectory per (model, window,
# iso3, origin_date), then aggregated the same long way as scores_by_stratum
# (no lead_time - these are whole-trajectory metrics) ------------------------
trajectory_meta <- scorable %>%
  dplyr::filter(horizon == 1L) %>%
  dplyr::left_join(season_ref, by = c("iso3", "target_date")) %>%
  dplyr::left_join(strata_country, by = "iso3") %>%
  dplyr::select(model, window_type, iso3, origin_date, season, region,
               seasonality_signal, endemicity, size_class)

operational_row <- scorable %>%
  dplyr::group_by(model, window_type, iso3, origin_date) %>%
  dplyr::summarise(
    n_months        = dplyr::n(),
    peak_timing_diff = peak_timing_diff(target_date, actual, .pred),
    dtw_distance     = dtw_distance(log1p(actual), log1p(.pred)),
    .groups = "drop"
  ) %>%
  dplyr::inner_join(trajectory_meta, by = c("model", "window_type", "iso3", "origin_date"))

summarise_operational <- function(grouped) {
  grouped %>%
    dplyr::summarise(
      n_obs                = dplyr::n(),
      n_seasons            = dplyr::n_distinct(paste(iso3, season)),
      thin_evidence        = n_seasons < 5,
      mean_peak_timing_diff   = mean(peak_timing_diff, na.rm = TRUE),
      median_peak_timing_diff = stats::median(peak_timing_diff, na.rm = TRUE),
      mean_dtw_distance       = mean(dtw_distance, na.rm = TRUE),
      .groups = "drop"
    )
}

operational_overall <- operational_row %>%
  dplyr::group_by(model, window_type) %>%
  summarise_operational() %>%
  dplyr::mutate(stratum_axis = "overall", stratum_value = "all", .after = window_type)

operational_by_stratum <- purrr::map_dfr(stratum_axes, function(axis) {
  operational_row %>%
    dplyr::rename(stratum_value = dplyr::all_of(axis)) %>%
    dplyr::group_by(model, window_type, stratum_value) %>%
    summarise_operational() %>%
    dplyr::mutate(stratum_axis = axis, .after = window_type)
})

operational <- dplyr::bind_rows(operational_overall, operational_by_stratum)
write_csv(operational, file.path(scores_dir, "operational.csv"))
#---log: confirm the operational (peak-timing + DTW) table was written, and its size
cli::cli_inform(c(">" = "wrote {.path {file.path(scores_dir, 'operational.csv')}} ({nrow(operational)} rows)"))

# ---- Console summary --------------------------------------------------------
cli::cli_h2("Stage 1 scoring complete")
cli::cli_inform("{.path {scores_dir}}")
scores_by_lead %>%
  dplyr::select(model, window_type, lead_time, n_obs, n_seasons, crps, crps_log, coverage_90) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
