# =============================================================================
# 03_nowcast_validation_summary.R
# Aggregate validation_detail.csv → tables, quantiles, calibrated lookup, coverage
# =============================================================================
# Reads:  Output/validation/validation_detail.csv (from 03_nowcast_validation_ind.R)
# Writes: summary_country.csv, summary_pair.csv, summary_country_pair.csv,
#         quantiles_country.csv, quantiles_region.csv, quantiles_global.csv,
#         calibrated_prediction_intervals.csv (country; Assets/Stable mirror),
#         calibrated_prediction_intervals_region.csv (Assets/Stable mirror),
#         calibrated_prediction_intervals_global.csv (Assets/Stable mirror),
#         coverage_summary.csv
# All three summary_* tables expose mean_monthly_cases and burden-scaled
# RMSE_scaled = RMSE / mean_monthly_cases. Country and country×pair use the
# country-wide mean_monthly_cases; global pair uses the stratum-pooled mean.
# The three Assets/Stable lookups are consumed by Scripts/V1_Dashboard_setup.R
# via Scripts/utils/apply_calibrated_intervals.R for country-page uncertainty
# whiskers (country → region → global fallback).
# =============================================================================

library(tidyverse)

# --- Paths / constants -------------------------------------------------------
out_dir <- "Output/validation"
detail_path <- file.path(out_dir, "validation_detail.csv")

# Minimum residual count per (iso3, cutoff, prediction_month) for operational lookup
MIN_OBS <- 5L

if (!file.exists(detail_path)) {
  stop("Missing ", detail_path, " — run 03_nowcast_validation_ind.R first.")
}

detail <- read_csv(detail_path, show_col_types = FALSE)

# --- (i) Country-level performance -------------------------------------------
# One row per iso3: MAE, RMSE, signed and absolute mean relative error, counts.
summary_country <- detail %>%
  group_by(iso3, country, Region) %>%
  dplyr::summarise(
    n_seasons = n_distinct(season),
    n_predictions = n(),
    MAE = mean(abs(absolute_error), na.rm = TRUE),
    RMSE = sqrt(mean(squared_error, na.rm = TRUE)),
    MRE_signed = mean(relative_error, na.rm = TRUE),
    MRE_abs = mean(abs(relative_error), na.rm = TRUE),
    .groups = "drop"
  )

# --- Mean monthly *actual* burden (for scaling RMSE across countries) --------
mean_cases <- detail %>%
  group_by(iso3) %>%
  dplyr::summarise(mean_monthly_cases = mean(actual_cases, na.rm = TRUE), .groups = "drop")

# --- Composite z tiering (methods): RMSE scaled by burden + |signed MRE| -----
# z-scores are within-country sample of one row each; sd=0 → replace NaN with 0.
summary_country <- summary_country %>%
  left_join(mean_cases, by = "iso3") %>%
  mutate(
    # calculate scaled RMSE
    RMSE_scaled = RMSE / mean_monthly_cases,
    z_RMSE = as.numeric(scale(RMSE_scaled)),
    z_MRE = as.numeric(scale(abs(MRE_signed)))
  ) %>%
  mutate(
    # scale() returns NaN when variance is 0 (e.g. very few countries); treat as 0 contribution
    z_RMSE = if_else(is.nan(z_RMSE) | is.na(z_RMSE), 0, z_RMSE),
    z_MRE = if_else(is.nan(z_MRE) | is.na(z_MRE), 0, z_MRE),
    composite_score = (z_RMSE + z_MRE) / 2
  )

tertiles <- quantile(summary_country$composite_score, probs = c(1 / 3, 2 / 3), na.rm = TRUE)

summary_country <- summary_country %>%
  mutate(
    performance_tier = case_when(
      composite_score <= tertiles[1] ~ "Good",
      composite_score <= tertiles[2] ~ "Moderate",
      TRUE ~ "Poor"
    )
  )

# --- (ii) Global pair summary: last-obs month × prediction month ------------
# RMSE_scaled uses the pooled mean actual within the (cutoff, prediction_month)
# stratum so the scaled metric is comparable across operational pairs.
summary_pair <- detail %>%
  group_by(cutoff_month, prediction_month) %>%
  dplyr::summarise(
    n_country_seasons = n_distinct(paste(iso3, season)),
    n_predictions = n(),
    MAE = mean(abs(absolute_error), na.rm = TRUE),
    RMSE = sqrt(mean(squared_error, na.rm = TRUE)),
    mean_monthly_cases = mean(actual_cases, na.rm = TRUE),
    RMSE_scaled = RMSE / mean_monthly_cases,
    MRE_signed = mean(relative_error, na.rm = TRUE),
    MRE_abs = mean(abs(relative_error), na.rm = TRUE),
    .groups = "drop"
  )

# --- (iii) Country × pair summary --------------------------------------------
# RMSE_scaled uses the country-wide mean_monthly_cases (same denominator as
# summary_country), so the burden-scaled metric is comparable across cells
# within a country and across countries for the same operational pair.
summary_country_pair <- detail %>%
  group_by(iso3, country, Region, cutoff_month, prediction_month) %>%
  dplyr::summarise(
    n_seasons = n_distinct(season),
    MAE = mean(abs(absolute_error), na.rm = TRUE),
    RMSE = sqrt(mean(squared_error, na.rm = TRUE)),
    MRE_signed = mean(relative_error, na.rm = TRUE),
    MRE_abs = mean(abs(relative_error), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(mean_cases, by = "iso3") %>%
  mutate(RMSE_scaled = RMSE / mean_monthly_cases)

# --- Relative-error quantiles: helper row per group -------------------------
# n_obs counts finite relative_error values used for quantiles.
qcols <- function(x) {
  tibble(
    n_obs = sum(is.finite(x)),
    q025 = as.numeric(quantile(x, 0.025, na.rm = TRUE)),
    q25 = as.numeric(quantile(x, 0.25, na.rm = TRUE)),
    q75 = as.numeric(quantile(x, 0.75, na.rm = TRUE)),
    q975 = as.numeric(quantile(x, 0.975, na.rm = TRUE))
  )
}

quantiles_country <- detail %>%
  filter(is.finite(relative_error)) %>%
  group_by(iso3, Region, cutoff_month, prediction_month) %>%
  reframe(qcols(relative_error))

quantiles_region <- detail %>%
  filter(is.finite(relative_error)) %>%
  group_by(Region, cutoff_month, prediction_month) %>%
  reframe(qcols(relative_error))

quantiles_global <- detail %>%
  filter(is.finite(relative_error)) %>%
  group_by(cutoff_month, prediction_month) %>%
  reframe(qcols(relative_error))

# --- Operational lookups: country / region / global; drop sparse cells -----
# Country lookup is the primary table used for validation coverage statistics
# (methods). Region and global mirrors exist only as runtime fallbacks for the
# observatory when a country × cutoff × prediction-month cell has < MIN_OBS
# residuals; they are NOT used in the coverage calculation below.
calibrated_prediction_intervals <- quantiles_country %>%
  filter(n_obs >= MIN_OBS)

calibrated_prediction_intervals_region <- quantiles_region %>%
  filter(n_obs >= MIN_OBS)

calibrated_prediction_intervals_global <- quantiles_global %>%
  filter(n_obs >= MIN_OBS)

if (!dir.exists("Assets/Stable")) {
  dir.create("Assets/Stable", recursive = TRUE, showWarnings = FALSE)
}

write_csv(summary_country, file.path(out_dir, "summary_country.csv"))
write_csv(summary_pair, file.path(out_dir, "summary_pair.csv"))
write_csv(summary_country_pair, file.path(out_dir, "summary_country_pair.csv"))
write_csv(quantiles_country, file.path(out_dir, "quantiles_country.csv"))
write_csv(quantiles_region, file.path(out_dir, "quantiles_region.csv"))
write_csv(quantiles_global, file.path(out_dir, "quantiles_global.csv"))
write_csv(calibrated_prediction_intervals, file.path(out_dir, "calibrated_prediction_intervals.csv"))
write_csv(calibrated_prediction_intervals_region, file.path(out_dir, "calibrated_prediction_intervals_region.csv"))
write_csv(calibrated_prediction_intervals_global, file.path(out_dir, "calibrated_prediction_intervals_global.csv"))
write_csv(calibrated_prediction_intervals, "Assets/Stable/calibrated_prediction_intervals.csv")
write_csv(calibrated_prediction_intervals_region, "Assets/Stable/calibrated_prediction_intervals_region.csv")
write_csv(calibrated_prediction_intervals_global, "Assets/Stable/calibrated_prediction_intervals_global.csv")

# --- Empirical coverage of calibrated 95% / 50% intervals --------------------
# Methods: L = max(0, C_hat * (1 + q_alpha)), U = max(0, C_hat * (1 + q_{1-alpha}))
# Only rows with a matching operational lookup contribute (inner join).
covered <- detail %>%
  inner_join(
    calibrated_prediction_intervals %>%
      dplyr::select(iso3, cutoff_month, prediction_month, q025, q25, q75, q975),
    by = c("iso3", "cutoff_month", "prediction_month")
  ) %>%
  mutate(
    lower_95 = pmax(0, predicted_cases * (1 + q025)),
    upper_95 = pmax(0, predicted_cases * (1 + q975)),
    lower_50 = pmax(0, predicted_cases * (1 + q25)),
    upper_50 = pmax(0, predicted_cases * (1 + q75)),
    in_95 = actual_cases >= lower_95 & actual_cases <= upper_95,
    in_50 = actual_cases >= lower_50 & actual_cases <= upper_50
  )

coverage_summary <- tibble(
  interval = c("95", "50"),
  nominal = c(0.95, 0.50),
  empirical = c(mean(covered$in_95, na.rm = TRUE), mean(covered$in_50, na.rm = TRUE)),
  n = c(sum(!is.na(covered$in_95)), sum(!is.na(covered$in_50)))
)

write_csv(coverage_summary, file.path(out_dir, "coverage_summary.csv"))

message("Summary tables and coverage written to ", out_dir)
