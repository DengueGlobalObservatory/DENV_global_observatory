#' ---
#' title: "00b_baseline_nowcast"
#' author: "K M Susong"
#' ---
#'
#' Overview:
#' ========
#' Baseline 2 for the forecast evaluation: GDO's seasonal-proportion nowcast,
#' repurposed as a forecast. DEFINITION ONLY - this file builds `nowcast_model`
#' and does nothing else on source (it does read the stable calibrated-interval
#' assets inside fit(); see spec$calibration_dir).
#'
#' Every function here is prefixed `nowcast_` and is private to this file.
#'
#' Method
#' ======
#' Works in season-month space (season_nMonth = 1..12 counting from the
#' country's mean lowest-transmission month).
#'
#'   1. fit() builds, per country with >= spec$min_train_seasons complete
#'      historical seasons:
#'        - the mean seasonal profile (share of the season's cases in each
#'          season-month, and the cumulative share) via the validation helper
#'          fit_baseline_profile()
#'        - the mean season total
#'        - the current season's cases observed up to the origin, and the
#'          origin's season-month k
#'   2. predict() for a target at season-month m:
#'        - estimated season total  T = (cases observed to date) / (mean
#'          cumulative proportion at k)   -- only trusted once k >=
#'          spec$min_cutoff and some cases have been observed; otherwise the
#'          mean season total is used
#'        - .pred = T * (mean monthly proportion at m)
#'      Targets in the *next* season (k + horizon crosses season end) use
#'      spec$next_season_method: "climatology" (mean season total), "carry"
#'      (this year's T), or "none" (NA).
#'   3. Intervals are multiplicative and read from GDO's calibrated relative-
#'      error quantiles (Assets/Stable/calibrated_prediction_intervals*.csv),
#'      country -> region -> global fallback, keyed on (cutoff_month k,
#'      prediction_month m). Next-season targets look up at cutoff_month 1.
#'
#' Timeline:
#' ========
#' 03-09-2026: Created.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(lubridate)
})

if (!exists("new_forecast_model")) {
  source("Scripts/forecasting/models/utils/forecast_helpers.R")
}
if (!exists("fit_baseline_profile")) {
  source("Scripts/validation/FUNCTIONS/00_FUN_validation_metrics.R")
}

nowcast_spec <- list(
  min_train_seasons  = 3L,   # complete historical seasons needed for a profile
  min_cutoff         = 3L,   # season-months observed before the current-season
                             # signal (cases / cum. proportion) is trusted
  next_season_method = "climatology",  # "climatology" | "carry" | "none"
  calibrate_point    = TRUE, # shift .pred to the calibrated IQR midpoint so the
                             # point sits inside the interval (FALSE keeps the
                             # raw estimate and widens the bands to bracket it)
  calibration_dir    = "Assets/Stable"
)

# ---- internal helpers -------------------------------------------

#' Which season a date sits in, and its position within that season.
#'
#' A season runs 12 months starting in `low` (the country's mean lowest-
#' transmission month). season_nMonth counts 1..12 from there.
#'
#' @param dates Date vector.
#' @param low Integer month 1..12 (scalar or same length as `dates`).
#' @return tibble(season, season_nMonth, season_start_year), one row per date.
nowcast_season_position <- function(dates, low) {
  mo <- lubridate::month(dates)
  yr <- lubridate::year(dates)
  offset     <- (mo - low) %% 12L          # 0 in the low month, 11 the month before
  start_year <- ifelse(mo >= low, yr, yr - 1L)
  tibble::tibble(
    season            = sprintf("%d/%d", start_year, start_year + 1L),
    season_nMonth     = as.integer(offset + 1L),
    season_start_year = as.integer(start_year)
  )
}

#' Read the three calibrated relative-error quantile files, if present.
nowcast_load_calibration <- function(dir) {
  files <- c(
    country = "calibrated_prediction_intervals.csv",
    region  = "calibrated_prediction_intervals_region.csv",
    global  = "calibrated_prediction_intervals_global.csv"
  )
  out <- lapply(files, function(fn) {
    p <- file.path(dir, fn)
    if (file.exists(p)) readr::read_csv(p, show_col_types = FALSE) else NULL
  })
  stats::setNames(out, names(files))
}

#' Turn a raw seasonal-proportion estimate into a calibrated predictive
#' distribution using GDO's relative-error quantiles.
#'
#' `df` must already have iso3, region, cutoff_month, prediction_month and
#' `.pred_raw`. The relative-error quantiles q_p (quantiles of (actual - est)/est
#' from the LOSO nowcast validation) are taken from the country file where
#' available, else the region file, else the global file - one source per row so
#' the quantiles stay monotone. Then, per row:
#'
#'   est(1 + q_p) is the p-th predictive quantile for the truth
#'   -> .pred_lower95 = est(1 + q_0.025), .pred_lower50 = est(1 + q_0.25), etc.
#'
#' The point forecast:
#'   calibrate_point = TRUE  -> est(1 + (q_0.25 + q_0.75)/2), the IQR midpoint,
#'       a bias-corrected median proxy (the calibrated files carry no q_0.5),
#'       so .pred always sits inside the interval.
#'   calibrate_point = FALSE -> the raw estimate, with the interval bounds
#'       clamped outward to bracket it.
#'
#' Rows with no calibrated match keep .pred = .pred_raw and get NA intervals.
nowcast_attach_intervals <- function(df, calib, calibrate_point = TRUE) {
  qn <- c("q025", "q25", "q75", "q975")

  prep <- function(tbl, keys, sfx) {
    if (is.null(tbl)) return(NULL)
    tbl %>%
      dplyr::select(dplyr::all_of(keys), dplyr::all_of(qn)) %>%
      dplyr::rename_with(~ paste0(.x, sfx), dplyr::all_of(qn))
  }

  region_tbl <- if (!is.null(calib$region)) {
    dplyr::rename(calib$region, region = Region)
  } else {
    NULL
  }

  cc <- prep(calib$country, c("iso3", "cutoff_month", "prediction_month"), "_c")
  rc <- prep(region_tbl,    c("region", "cutoff_month", "prediction_month"), "_r")
  gc <- prep(calib$global,  c("cutoff_month", "prediction_month"), "_g")

  out <- df
  if (!is.null(cc)) out <- dplyr::left_join(out, cc, by = c("iso3", "cutoff_month", "prediction_month"))
  if (!is.null(rc)) out <- dplyr::left_join(out, rc, by = c("region", "cutoff_month", "prediction_month"))
  if (!is.null(gc)) out <- dplyr::left_join(out, gc, by = c("cutoff_month", "prediction_month"))

  # Choose ONE source per row (country, else region, else global) so the four
  # quantiles are always internally consistent - mixing sources can break
  # monotonicity.
  matched <- function(sfx) {
    col <- paste0("q025", sfx)
    if (col %in% names(out)) !is.na(out[[col]]) else rep(FALSE, nrow(out))
  }
  src <- dplyr::case_when(
    matched("_c") ~ "_c",
    matched("_r") ~ "_r",
    matched("_g") ~ "_g",
    TRUE          ~ NA_character_
  )
  take <- function(q) {
    v <- rep(NA_real_, nrow(out))
    for (sfx in c("_c", "_r", "_g")) {
      col <- paste0(q, sfx)
      if (col %in% names(out)) {
        rows <- !is.na(src) & src == sfx
        v[rows] <- out[[col]][rows]
      }
    }
    v
  }
  q025 <- take("q025"); q25 <- take("q25"); q75 <- take("q75"); q975 <- take("q975")
  have_q <- !is.na(q025)

  est <- out$.pred_raw
  point <- if (isTRUE(calibrate_point)) {
    dplyr::if_else(have_q, est * (1 + (q25 + q75) / 2), est)
  } else {
    est
  }

  lo95 <- est * (1 + q025)
  lo50 <- est * (1 + q25)
  up50 <- est * (1 + q75)
  up95 <- est * (1 + q975)

  if (!isTRUE(calibrate_point)) {
    # keep the raw point; widen the bands so they bracket it
    lo95 <- pmin(lo95, point); lo50 <- pmin(lo50, point)
    up50 <- pmax(up50, point); up95 <- pmax(up95, point)
  }

  out %>%
    dplyr::mutate(
      .pred         = pmax(0, round(dplyr::if_else(is.na(point), est, point))),
      .pred_lower50 = dplyr::if_else(have_q, pmax(0, round(lo50)), NA_real_),
      .pred_upper50 = dplyr::if_else(have_q, pmax(0, round(up50)), NA_real_),
      .pred_lower95 = dplyr::if_else(have_q, pmax(0, round(lo95)), NA_real_),
      .pred_upper95 = dplyr::if_else(have_q, pmax(0, round(up95)), NA_real_)
    ) %>%
    dplyr::select(-dplyr::matches("^q(025|25|75|975)_[crg]$"))
}

# ---- fit ------------------------------------------------------
nowcast_fit <- function(train_df, spec = nowcast_spec) {
  req <- c("iso3", "region", "date", "cases", "mean_low_month",
           "Actual_monthly_proportion", "Actual_cum_monthly_proportion")
  miss <- setdiff(req, names(train_df))
  if (length(miss) > 0) {
    cli::cli_abort("`train_df` missing column{?s}: {.field {miss}}.")
  }

  df <- train_df %>%
    dplyr::select(dplyr::all_of(req)) %>%
    dplyr::filter(!is.na(mean_low_month)) %>%
    dplyr::arrange(iso3, date)
  df <- dplyr::bind_cols(df, nowcast_season_position(df$date, df$mean_low_month))

  region_map <- dplyr::distinct(df, iso3, region)
  low_map    <- dplyr::distinct(df, iso3, mean_low_month)

  # Complete training seasons only: 12 season-months, each with a proportion
  # value. Only fully-observed historical seasons carry those, so an ongoing or
  # partly-seen season is excluded and there is no leakage.
  complete_seasons <- df %>%
    dplyr::group_by(iso3, season) %>%
    dplyr::filter(
      dplyr::n_distinct(season_nMonth) == 12L,
      sum(!is.na(Actual_monthly_proportion)) == 12L
    ) %>%
    dplyr::ungroup()

  profiles <- complete_seasons %>%
    dplyr::group_by(iso3) %>%
    dplyr::group_modify(~ fit_baseline_profile(.x)) %>%
    dplyr::ungroup()

  season_totals <- complete_seasons %>%
    dplyr::group_by(iso3, season) %>%
    dplyr::summarise(total = sum(cases, na.rm = TRUE), .groups = "drop_last") %>%
    dplyr::summarise(
      mean_season_total = mean(total),
      n_train_seasons   = dplyr::n(),
      .groups           = "drop"
    )

  eligible <- season_totals %>%
    dplyr::filter(n_train_seasons >= spec$min_train_seasons) %>%
    dplyr::pull(iso3)

  # Current-season state at the origin (the last training month).
  origin_date <- max(df$date)
  origin_state <- low_map %>%
    dplyr::bind_cols(
      nowcast_season_position(rep(origin_date, nrow(low_map)), low_map$mean_low_month)
    ) %>%
    dplyr::transmute(
      iso3,
      origin_season     = season,
      origin_start_year = season_start_year,
      k                 = season_nMonth
    )

  cum_to_date <- df %>%
    dplyr::inner_join(origin_state, by = "iso3") %>%
    dplyr::filter(season == origin_season, season_nMonth <= k) %>%
    dplyr::group_by(iso3) %>%
    dplyr::summarise(cum_to_date = sum(cases, na.rm = TRUE), .groups = "drop")

  structure(list(
    name          = "nowcast",
    spec          = spec,
    origin_date   = origin_date,
    profiles      = dplyr::filter(profiles, iso3 %in% eligible),
    season_totals = dplyr::filter(season_totals, iso3 %in% eligible),
    origin_state  = origin_state,
    cum_to_date   = cum_to_date,
    region_map    = region_map,
    low_map       = low_map,
    calibration   = nowcast_load_calibration(spec$calibration_dir)
  ), class = "nowcast_model_fit")
}

# ---- predict -----------------------------------------------
nowcast_predict <- function(fitted, targets, spec = fitted$spec) {
  need <- c("iso3", "origin_date", "horizon", "target_date")
  miss <- setdiff(need, names(targets))
  if (length(miss) > 0) {
    cli::cli_abort("`targets` missing column{?s}: {.field {miss}}.")
  }

  t <- dplyr::left_join(targets, fitted$low_map, by = "iso3")
  t <- dplyr::bind_cols(
    t,
    nowcast_season_position(t$target_date, t$mean_low_month) %>%
      dplyr::rename(
        target_season     = season,
        m                 = season_nMonth,
        target_start_year = season_start_year
      )
  )

  prof_pk <- fitted$profiles %>%
    dplyr::select(iso3, k = season_nMonth, p_cum_k = Ave_cum_monthly_proportion)
  prof_pm <- fitted$profiles %>%
    dplyr::select(iso3, m = season_nMonth, p_m = Ave_monthly_proportion)

  t <- t %>%
    dplyr::left_join(fitted$origin_state, by = "iso3") %>%
    dplyr::left_join(fitted$cum_to_date, by = "iso3") %>%
    dplyr::left_join(
      dplyr::select(fitted$season_totals, iso3, mean_season_total), by = "iso3"
    ) %>%
    dplyr::left_join(fitted$region_map, by = "iso3") %>%
    dplyr::left_join(prof_pk, by = c("iso3", "k")) %>%
    dplyr::left_join(prof_pm, by = c("iso3", "m")) %>%
    dplyr::mutate(
      is_current = !is.na(origin_season) & target_season == origin_season,
      is_next    = !is.na(origin_start_year) & target_start_year == origin_start_year + 1L,
      # this-year season total, trusted only once enough of the season is in
      season_total_signal = dplyr::if_else(
        k >= spec$min_cutoff & !is.na(p_cum_k) & p_cum_k > 0 &
          !is.na(cum_to_date) & cum_to_date > 0,
        cum_to_date / p_cum_k, NA_real_
      ),
      season_total_used = dplyr::case_when(
        is_current & !is.na(season_total_signal)            ~ season_total_signal,
        is_current                                          ~ mean_season_total,
        is_next & spec$next_season_method == "carry"        ~ dplyr::coalesce(season_total_signal, mean_season_total),
        is_next & spec$next_season_method == "climatology"  ~ mean_season_total,
        TRUE                                                ~ NA_real_
      ),
      .pred_raw = dplyr::if_else(!is.na(p_m), pmax(0, season_total_used * p_m), NA_real_),
      cutoff_month     = dplyr::if_else(is_current, k, 1L),
      prediction_month = m
    )

  t <- nowcast_attach_intervals(t, fitted$calibration, spec$calibrate_point)

  dplyr::select(t, dplyr::all_of(forecast_output_cols))
}

# ---- diagnose ---------------------------------------------
nowcast_diagnose <- function(fitted, train_df = NULL, spec = fitted$spec) {
  all_iso      <- unique(fitted$region_map$iso3)
  with_profile <- unique(fitted$profiles$iso3)
  st           <- fitted$season_totals
  calib_ok     <- !is.null(fitted$calibration$global)

  list(
    model                    = "nowcast",
    n_countries              = length(all_iso),
    n_countries_with_profile = length(with_profile),
    n_countries_no_profile   = length(setdiff(all_iso, with_profile)),
    median_train_seasons     = if (nrow(st) > 0) stats::median(st$n_train_seasons) else NA_real_,
    min_train_seasons        = spec$min_train_seasons,
    next_season_method       = spec$next_season_method,
    calibration_loaded       = !is.null(fitted$calibration$country),
    pass                     = length(with_profile) > 0 && calib_ok,
    notes = sprintf(
      "seasonal-proportion baseline; profile for %d/%d countries (>= %d seasons); next-season = %s; calibration %s",
      length(with_profile), length(all_iso), spec$min_train_seasons,
      spec$next_season_method,
      if (!is.null(fitted$calibration$country)) "loaded" else "MISSING"
    )
  )
}

# ---- model object ---------------------------------------
nowcast_model <- new_forecast_model(
  name     = "nowcast",
  spec     = nowcast_spec,
  fit      = nowcast_fit,
  predict  = nowcast_predict,
  diagnose = nowcast_diagnose
)
