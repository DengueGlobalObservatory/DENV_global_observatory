#' ---
#' title: "04_peak_months"
#' ---
#'
#' From the final data (df), compute per country (iso3):
#' - max_Month: month with the highest average proportion of monthly cases (peak month).
#' - start_peak_Month: last month prior to the peak where the average proportion
#'   of monthly cases is higher than the prior month (start of the run-up to peak).
#'
#' Expects final data with columns including: iso3, season_nMonth, Month,
#' Ave_monthly_proportion, and optionally country, Region.
#'
#' Usage:
#'   source("Scripts/04_peak_months.R")
#'   peak_df <- compute_peak_months(data)
#' Or run standalone after defining/loading `data`.
#'

library(dplyr)

#' Compute peak and start-of-peak month per country from seasonal proportion profile.
#'
#' @param df Data frame with columns: iso3, season_nMonth, Month, Ave_monthly_proportion.
#'   Optional: country, Region (passed through if present).
#' @return A data frame with one row per iso3: iso3, max_Month, start_peak_Month,
#'   and optional country, Region.
compute_peak_months <- function(df) {
  # One row per (iso3, season_nMonth) with proportion and Month (take first within group)
  profile <- df %>%
    dplyr::filter(!is.na(Ave_monthly_proportion), !is.na(season_nMonth)) %>%
    dplyr::group_by(iso3, season_nMonth) %>%
    dplyr::summarise(
      Ave_monthly_proportion = dplyr::first(Ave_monthly_proportion),
      Month = dplyr::first(Month),
      .groups = "drop"
    ) %>%
    dplyr::arrange(iso3, season_nMonth)

  # max_Month: month with highest Ave_monthly_proportion per iso3
  max_row <- profile %>%
    dplyr::group_by(iso3) %>%
    dplyr::slice(which.max(Ave_monthly_proportion)) %>%
    dplyr::ungroup() %>%
    dplyr::select(iso3, max_Month = Month)

  # start_peak_Month: last month *before* the peak where proportion > prior month (in seasonal order)
  start_peak_row <- profile %>%
    dplyr::group_by(iso3) %>%
    dplyr::mutate(
      prop_prior = dplyr::lag(Ave_monthly_proportion),
      is_increase = Ave_monthly_proportion > prop_prior,
      season_nMonth_peak = season_nMonth[which.max(Ave_monthly_proportion)]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(season_nMonth < season_nMonth_peak, is_increase == TRUE) %>%
    dplyr::group_by(iso3) %>%
    dplyr::slice_max(season_nMonth, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(iso3, start_peak_Month = Month)

  # One row per iso3: max_Month for all; start_peak_Month NA when none (e.g. peak at position 1)
  out <- max_row %>%
    dplyr::left_join(start_peak_row, by = "iso3")

  # Optional: attach country, Region from df if present
  extra <- df %>%
    dplyr::group_by(iso3) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup() %>%
    dplyr::select(iso3, dplyr::any_of(c("country", "Country", "Region")))
  if (ncol(extra) > 1L) {
    out <- out %>% dplyr::left_join(extra, by = "iso3")
  }
  out
}


# ----- Run when script is sourced with data in environment

if (exists("data") && is.data.frame(data) && "Ave_monthly_proportion" %in% names(data)) {
  peak_months_df <- compute_peak_months(data)
  message("Created peak_months_df with ", nrow(peak_months_df), " countries (max_Month, start_peak_Month).")
}
