# =============================================================================
# apply_calibrated_intervals.R
# Attach validation-calibrated 95% prediction intervals to dashboard / pipeline
# rows using empirical relative-error quantiles from Assets/Stable.
# =============================================================================
# Inputs:
#   df              : data frame with iso3, Region, season, season_nMonth,
#                     cases, source (and/or Data_status). One row per country
#                     × season × season month.
#   lookup_country  : `calibrated_prediction_intervals.csv` (country level)
#   lookup_region   : `calibrated_prediction_intervals_region.csv`
#   lookup_global   : `calibrated_prediction_intervals_global.csv`
#
# Operational keys (match validation):
#   cutoff_month     = max(season_nMonth) where source != "Estimates" (per
#                      iso3 + season). NA if no observed month.
#   prediction_month = season_nMonth of the row being displayed.
#
# Interval formula (methods):
#   lower95 = max(0, cases * (1 + q025))
#   upper95 = max(0, cases * (1 + q975))
# Bounds are rounded outward to integers (floor lower, ceiling upper) so the
# displayed case-count interval never tightens the underlying empirical one.
# Applied only to rows with source == "Estimates" (estimated months).
#
# Quantile coalesce order:
#   1. country  (iso3, cutoff_month, prediction_month)
#   2. region   (Region, cutoff_month, prediction_month)
#   3. global   (cutoff_month, prediction_month)
# A `interval_source` column records which level supplied the quantiles.
# =============================================================================

library(dplyr)

#' Compute calibrated cutoff_month per iso3 + season
#'
#' The last observed season month (`source != "Estimates"`, non-missing cases).
#' Rows from a season with no observed month receive NA and will not match.
.compute_cutoff_month <- function(df) {
  df %>%
    dplyr::mutate(
      .is_observed = !is.na(cases) &
        (is.na(source) | as.character(source) != "Estimates")
    ) %>%
    dplyr::group_by(iso3, season) %>%
    dplyr::mutate(
      cutoff_month = suppressWarnings(max(
        ifelse(.is_observed, season_nMonth, NA_integer_),
        na.rm = TRUE
      ))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      cutoff_month = suppressWarnings(
        dplyr::if_else(is.finite(cutoff_month), as.integer(cutoff_month), NA_integer_)
      )
    ) %>%
    dplyr::select(-.is_observed)
}

#' Apply calibrated 95% (and 50%) prediction intervals to a data frame
#'
#' @param df data frame as described above
#' @param lookup_country tibble with iso3, cutoff_month, prediction_month, q025, q25, q75, q975
#' @param lookup_region tibble with Region, cutoff_month, prediction_month, q025, q25, q75, q975
#' @param lookup_global tibble with cutoff_month, prediction_month, q025, q25, q75, q975
#' @return df with added columns: cutoff_month, lower95, upper95, lower50,
#'   upper50, interval_source ("country" / "region" / "global" / NA).
apply_calibrated_intervals <- function(df,
                                       lookup_country = NULL,
                                       lookup_region = NULL,
                                       lookup_global = NULL) {
  required <- c("iso3", "season", "season_nMonth", "cases")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "apply_calibrated_intervals(): df missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  if (!"source" %in% names(df)) df$source <- NA_character_
  if (!"Region" %in% names(df)) df$Region <- NA_character_

  q_cols <- c("q025", "q25", "q75", "q975")

  # Country lookup
  country_tbl <- if (!is.null(lookup_country) && nrow(lookup_country) > 0) {
    lookup_country %>%
      dplyr::select(dplyr::all_of(c("iso3", "cutoff_month", "prediction_month", q_cols))) %>%
      dplyr::rename_with(~ paste0(.x, "_c"), dplyr::all_of(q_cols))
  } else {
    NULL
  }

  region_tbl <- if (!is.null(lookup_region) && nrow(lookup_region) > 0) {
    lookup_region %>%
      dplyr::select(dplyr::all_of(c("Region", "cutoff_month", "prediction_month", q_cols))) %>%
      dplyr::rename_with(~ paste0(.x, "_r"), dplyr::all_of(q_cols))
  } else {
    NULL
  }

  global_tbl <- if (!is.null(lookup_global) && nrow(lookup_global) > 0) {
    lookup_global %>%
      dplyr::select(dplyr::all_of(c("cutoff_month", "prediction_month", q_cols))) %>%
      dplyr::rename_with(~ paste0(.x, "_g"), dplyr::all_of(q_cols))
  } else {
    NULL
  }

  out <- .compute_cutoff_month(df) %>%
    dplyr::mutate(prediction_month = as.integer(season_nMonth))

  if (!is.null(country_tbl)) {
    out <- out %>% dplyr::left_join(country_tbl, by = c("iso3", "cutoff_month", "prediction_month"))
  } else {
    for (q in q_cols) out[[paste0(q, "_c")]] <- NA_real_
  }
  if (!is.null(region_tbl)) {
    out <- out %>% dplyr::left_join(region_tbl, by = c("Region", "cutoff_month", "prediction_month"))
  } else {
    for (q in q_cols) out[[paste0(q, "_r")]] <- NA_real_
  }
  if (!is.null(global_tbl)) {
    out <- out %>% dplyr::left_join(global_tbl, by = c("cutoff_month", "prediction_month"))
  } else {
    for (q in q_cols) out[[paste0(q, "_g")]] <- NA_real_
  }

  out <- out %>%
    dplyr::mutate(
      interval_source = dplyr::case_when(
        !is.na(q025_c) ~ "country",
        !is.na(q025_r) ~ "region",
        !is.na(q025_g) ~ "global",
        TRUE ~ NA_character_
      ),
      q025 = dplyr::coalesce(q025_c, q025_r, q025_g),
      q25  = dplyr::coalesce(q25_c, q25_r, q25_g),
      q75  = dplyr::coalesce(q75_c, q75_r, q75_g),
      q975 = dplyr::coalesce(q975_c, q975_r, q975_g)
    )

  is_estimated <- !is.na(out$source) & as.character(out$source) == "Estimates"

  out <- out %>%
    dplyr::mutate(
      lower95 = dplyr::if_else(is_estimated & !is.na(cases) & !is.na(q025),
                               floor(pmax(0, cases * (1 + q025))), NA_real_),
      upper95 = dplyr::if_else(is_estimated & !is.na(cases) & !is.na(q975),
                               ceiling(pmax(0, cases * (1 + q975))), NA_real_),
      lower50 = dplyr::if_else(is_estimated & !is.na(cases) & !is.na(q25),
                               floor(pmax(0, cases * (1 + q25))), NA_real_),
      upper50 = dplyr::if_else(is_estimated & !is.na(cases) & !is.na(q75),
                               ceiling(pmax(0, cases * (1 + q75))), NA_real_),
      interval_source = dplyr::if_else(is_estimated, interval_source, NA_character_)
    ) %>%
    dplyr::select(
      -dplyr::any_of(c(
        "q025_c", "q25_c", "q75_c", "q975_c",
        "q025_r", "q25_r", "q75_r", "q975_r",
        "q025_g", "q25_g", "q75_g", "q975_g",
        "q025", "q25", "q75", "q975"
      ))
    )

  out
}

#' Convenience loader: read country/region/global lookup CSVs if present
#'
#' @param stable_dir directory holding the three CSVs (default Assets/Stable)
#' @return named list with elements `country`, `region`, `global` (each either
#'   a tibble or NULL if the file is missing or empty).
load_calibrated_lookups <- function(stable_dir = "Assets/Stable") {
  read_if <- function(path, required_cols) {
    if (!file.exists(path)) return(NULL)
    tbl <- tryCatch(
      readr::read_csv(path, show_col_types = FALSE),
      error = function(e) NULL
    )
    if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
    missing <- setdiff(required_cols, names(tbl))
    if (length(missing) > 0) {
      warning("load_calibrated_lookups(): ", path,
              " is missing columns ", paste(missing, collapse = ", "),
              " (expected current validation schema). Returning NULL.")
      return(NULL)
    }
    tbl
  }

  list(
    country = read_if(
      file.path(stable_dir, "calibrated_prediction_intervals.csv"),
      c("iso3", "cutoff_month", "prediction_month", "q025", "q25", "q75", "q975")
    ),
    region = read_if(
      file.path(stable_dir, "calibrated_prediction_intervals_region.csv"),
      c("Region", "cutoff_month", "prediction_month", "q025", "q25", "q75", "q975")
    ),
    global = read_if(
      file.path(stable_dir, "calibrated_prediction_intervals_global.csv"),
      c("cutoff_month", "prediction_month", "q025", "q25", "q75", "q975")
    )
  )
}
