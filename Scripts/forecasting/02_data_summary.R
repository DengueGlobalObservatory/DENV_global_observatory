#!/usr/bin/env Rscript

#' ---
#' title: "02_data_summary"
#' author: "K M Susong"
#' ---
#'
#' Overview:
#' ========
#' Characterise the training panel by country and by region, and assign the
#' provisional scenario strata the Stage 1 backtest will be sliced on.
#'
#' **Per country:** coverage (complete seasons, months, span), data composition
#' (observed / corrected / estimated share), magnitude (mean / median / max
#' monthly cases, CV, zero-month fraction) and seasonality shape, read from the
#' mean seasonal profile (`DENV_average_season.csv`):
#'   - seasonal_concentration : HHI of the mean monthly proportions
#'                              (1/12 ~ flat, -> 1 as one month dominates)
#'   - peak_to_trough_ratio   : mean peak month / mean trough month
#'   - peak_month, top3_month_share
#'
#' **Provisional strata (thresholds in 00_config.R, flagged `strata_provisional`):**
#'   - seasonality_signal : weak / moderate / strong, by tertiles of
#'                          seasonal_concentration across the 96 countries
#'   - endemicity         : endemic if mean monthly cases, complete seasons and
#'                          zero-month fraction all clear their cut-points;
#'                          otherwise emerging
#'   NOTE: an alternative to the tertile split is to adopt the seasonality
#'   clusters from Joshi et al. once published - see 00_config.R.
#'
#' Input : Output/forecasting/training_data/training_panel_<snapshot_date>.csv
#'         Output/<snapshot_date>/DENV_average_season.csv
#' Output: Output/forecasting/data_summary/{summary_country,summary_region,strata_country}.csv
#'
#' Timeline:
#' ========
#' 02-09-2026: Created. Stage 0 scaffold.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(cli)
})

# ---- Define project configs ----------------------------------------
source("Scripts/forecasting/00_config.R")

panel_path <- file.path(forecast_out, "training_data",
                        paste0("training_panel_", snapshot_date, ".csv"))

for (f in c(panel_path, seasonal_profile)) {
  if (!file.exists(f)) {
    cli::cli_abort(c(
      "Required input not found: {.path {f}}.",
      "i" = "Run {.path Scripts/forecasting/01_prepare_training_data.R} first."
    ))
  }
}

out_dir <- file.path(forecast_out, "data_summary")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

panel <- read_csv(panel_path, show_col_types = FALSE)
profile <- read_csv(seasonal_profile, show_col_types = FALSE) %>%
  dplyr::mutate(iso3 = str_to_upper(iso3))

# ---- Coverage and composition (per country) ----------------------
valued <- panel %>% dplyr::filter(!is.na(cases))

complete_seasons <- panel %>%
  dplyr::filter(!is.na(season)) %>%
  dplyr::group_by(iso3, season) %>%
  dplyr::summarise(n_month = dplyr::n_distinct(season_nMonth), .groups = "drop") %>%
  dplyr::filter(n_month == 12L) %>%
  dplyr::group_by(iso3) %>%
  dplyr::summarise(
    n_complete_seasons = dplyr::n(),
    first_season       = min(season),
    last_season        = max(season),
    .groups            = "drop"
  )

coverage <- valued %>%
  dplyr::group_by(iso3, country, region) %>%
  dplyr::summarise(
    first_date    = min(date),
    last_date     = max(date),
    n_months      = dplyr::n(),
    n_observed    = sum(data_status == "observed"),
    n_corrected   = sum(data_status == "corrected"),
    n_estimated   = sum(data_status == "estimated"),
    .groups       = "drop"
  ) %>%
  dplyr::mutate(
    span_years    = as.numeric(last_date - first_date) / 365.25,
    pct_estimated = n_estimated / n_months
  )

n_gap <- panel %>%
  dplyr::filter(data_status == "gap") %>%
  dplyr::count(iso3, name = "n_gap_months")

# ---- Magnitude (per country) ----------------------------------
magnitude <- valued %>%
  dplyr::group_by(iso3) %>%
  dplyr::summarise(
    total_cases          = sum(cases),
    mean_monthly_cases   = mean(cases),
    median_monthly_cases = median(cases),
    max_monthly_cases    = max(cases),
    cv_monthly           = ifelse(mean(cases) > 0, sd(cases) / mean(cases), NA_real_),
    zero_month_fraction  = mean(cases == 0),
    .groups              = "drop"
  )

# ---- Seasonality shape (from the mean seasonal profile) --------
seasonality <- profile %>%
  dplyr::group_by(iso3) %>%
  dplyr::summarise(
    prop_sum              = sum(Ave_monthly_proportion, na.rm = TRUE),
    seasonal_concentration = sum(Ave_monthly_proportion^2, na.rm = TRUE),
    top3_month_share      = sum(sort(Ave_monthly_proportion, decreasing = TRUE)[1:3], na.rm = TRUE),
    peak_month            = Month[which.max(Ave_season_monthly_cases)],
    .peak                 = max(Ave_season_monthly_cases, na.rm = TRUE),
    .trough               = min(Ave_season_monthly_cases, na.rm = TRUE),
    .groups               = "drop"
  ) %>%
  dplyr::mutate(
    peak_to_trough_ratio = dplyr::if_else(.trough > 0, .peak / .trough, NA_real_)
  ) %>%
  dplyr::select(-.peak, -.trough)

bad_prop <- seasonality %>% dplyr::filter(abs(prop_sum - 1) > 0.01)
if (nrow(bad_prop) > 0) {
  cli::cli_warn("Mean seasonal proportions do not sum to ~1 for: {.val {bad_prop$iso3}}.")
}
seasonality <- seasonality %>% dplyr::select(-prop_sum)

# ---- Assemble the country summary --------------------------
summary_country <- coverage %>%
  dplyr::left_join(complete_seasons, by = "iso3") %>%
  dplyr::left_join(n_gap, by = "iso3") %>%
  dplyr::left_join(magnitude, by = "iso3") %>%
  dplyr::left_join(seasonality, by = "iso3") %>%
  dplyr::mutate(
    n_complete_seasons = tidyr::replace_na(n_complete_seasons, 0L),
    n_gap_months       = tidyr::replace_na(n_gap_months, 0L)
  )

# ---- Provisional scenario strata --------------------------
summary_country <- summary_country %>%
  dplyr::mutate(
    seasonality_signal = c("weak", "moderate", "strong")[
      dplyr::ntile(seasonal_concentration, 3)
    ],
    endemicity = dplyr::if_else(
      mean_monthly_cases  >= endemic_min_mean_cases &
        n_complete_seasons >= endemic_min_seasons &
        zero_month_fraction <  endemic_max_zero_fraction,
      "endemic", "emerging"
    ),
    strata_provisional = TRUE
  ) %>%
  dplyr::arrange(region, dplyr::desc(mean_monthly_cases))

signal_cuts <- stats::quantile(
  summary_country$seasonal_concentration, c(1 / 3, 2 / 3), na.rm = TRUE
)

# ---- Region summary ---------------------------------------
summary_region <- summary_country %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(
    n_countries          = dplyr::n(),
    total_cases          = sum(total_cases),
    median_country_mean  = median(mean_monthly_cases),
    n_endemic            = sum(endemicity == "endemic"),
    n_emerging           = sum(endemicity == "emerging"),
    n_signal_strong      = sum(seasonality_signal == "strong"),
    n_signal_moderate    = sum(seasonality_signal == "moderate"),
    n_signal_weak        = sum(seasonality_signal == "weak"),
    seasons_min          = min(n_complete_seasons),
    seasons_median       = median(n_complete_seasons),
    seasons_max          = max(n_complete_seasons),
    .groups              = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(total_cases))

# ---- Strata key (focused join table for scoring) ---------
strata_country <- summary_country %>%
  dplyr::select(
    iso3, country, region, seasonality_signal, endemicity,
    seasonal_concentration, mean_monthly_cases, n_complete_seasons,
    zero_month_fraction, strata_provisional
  )

# ---- Write -----------------------------------------------
write_csv(summary_country, file.path(out_dir, "summary_country.csv"))
write_csv(summary_region,  file.path(out_dir, "summary_region.csv"))
write_csv(strata_country,  file.path(out_dir, "strata_country.csv"))

# ---- Console summary ------------------------------------
cli::cli_h2("Data summary written")
cli::cli_inform(c(
  "*" = "{.path {file.path(out_dir, 'summary_country.csv')}}  ({nrow(summary_country)} countries)",
  "*" = "{.path {file.path(out_dir, 'summary_region.csv')}}",
  "*" = "{.path {file.path(out_dir, 'strata_country.csv')}}"
))

cli::cli_h3("seasonal_concentration tertile cut-points (provisional)")
cli::cli_inform("weak < {round(signal_cuts[1], 3)} <= moderate < {round(signal_cuts[2], 3)} <= strong")

cli::cli_h3("provisional strata: seasonality_signal x endemicity")
summary_country %>%
  dplyr::count(seasonality_signal, endemicity) %>%
  tidyr::pivot_wider(names_from = endemicity, values_from = n, values_fill = 0L) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cli::cli_h3("by region")
summary_region %>%
  dplyr::select(region, n_countries, n_endemic, n_emerging, seasons_median, total_cases) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
