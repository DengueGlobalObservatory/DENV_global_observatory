#' ---
#' title: "00b_baseline_nowcast"
#' author: "K M Susong"
#' ---
#'
#' Overview:
#' ========
#' Baseline 2 for the forecast evaluation: GDO's seasonal-proportion nowcast,
#' repurposed as a forecast. 
#' 
#' DEFINITION ONLY - this file builds `nowcast_model`and does nothing else on 
#' source.  
#' (it does read the stable calibrated-interval assets inside fit(); 
#' see spec$calibration_dir).
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
#'        - the mean seasonal profile via the validation helper
#'          fit_baseline_profile()
#'        - the mean season total
#'        - the current season's cases observed up to the origin, and the
#'          origin's season-month k
#'   2. predict() for a target at season-month m:
#'        - estimated season total
#'            -T = (cases observed to date) / (mean
#'              cumulative proportion at k)   -- only trusted once k >=
#'              spec$min_cutoff and some cases have been observed; otherwise the
#'              mean season total is used
#'        - .pred = T * (mean monthly proportion at m)
#'        
#'      ! Targets in the *next* season (k + horizon crosses season end) use
#'      spec$next_season_method: "mean_T" (mean season total), "carry_T"
#'      (this year's T carried forward), or "NA".
#'      * "NA" is how this is currently handled in the deployed GDO nowcast and
#'      results in no estimate if cases have not been observed this season.
#'
#'   3. [UNDER REVIEW - see 04-09-2026 note] Intervals are multiplicative and
#'      read from GDO's *static* calibrated relative-error quantiles
#'      (Assets/Stable/calibrated_prediction_intervals*.csv), country -> region
#'      -> global fallback, keyed on (cutoff_month k, prediction_month m).
#'      Next-season targets look up at cutoff_month 1. This file is fit once,
#'      on the full historical record, via LOSO across complete seasons - it is
#'      NOT recomputed per training window. That is fine for a single Stage 0
#'      fit on the full panel, but is not internally consistent with a Stage 1
#'      backtest, where each (window, origin) fit sees a different, often much
#'      smaller, set of seasons: the borrowed calibration would not reflect
#'      that fit's own uncertainty. Before Stage 1 this needs replacing with an
#'      uncertainty method computed FROM each fit's own training window (see
#'      nowcast_attach_intervals() below for the current placeholder and the
#'      planned direction).
#'
#' Timeline:
#' ========
#' 03-09-2026: Created.
#' 04-09-2026: Second review. Renamed next_season_method values. Flagged the
#'   static-calibration interval method as needing a per-window redesign before
#'   Stage 1 (see notes on nowcast_attach_intervals()). min_cutoff confirmed to
#'   NOT be a GDO parameter.

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
  min_cutoff         = 3L,   # NOT a GDO parameter -- added here as a guard
                             # against a wild C/P_k at k=1. The deployed nowcast
                             # (03_proportion_nowcast.R) and its LOSO validation
                             # (03_nowcast_validation_ind.R, k = 1:11) apply no
                             # such floor. Revisit: keep, drop, or make it 1
                             # (= off) to match GDO exactly.
  next_season_method = "mean_T",  # "mean_T" | "carry_T" | "NA"
                             # (a *string* "NA", not R's NA -- matches how the
                             # deployed nowcast labels "no estimate this season")
  # [UNDER REVIEW] calibrate_point + calibration_dir below implement the
  # interim, static-file interval method. Expected to be replaced by a
  # per-training-window empirical method before Stage 1 -- see
  # nowcast_attach_intervals(). calibrate_point should become unnecessary once
  # that method supplies a coherent, self-consistent quantile set directly.
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
#' Matches the GDO pipeline's own season assignment exactly (same formula as
#' `circular_encode()` in Scripts/seasonal_baseline/02_identify_seasonal_baseline.R);
#' verified empirically against the panel's GDO-assigned season/season_nMonth
#' columns on 04-09-2026: 0 mismatches across 13,803 rows.
#'
#' @param dates Date vector.
#' @param low Integer month 1..12 (scalar or same length as `dates`).
#' @return tibble(season, season_nMonth, season_start_year), one row per date.
nowcast_season_position <- function(dates, low) {
  mo <- lubridate::month(dates)
  yr <- lubridate::year(dates)
  offset <- (mo - low) %% 12L  # 0 in the low month, 11 the month before
  # Once the month reaches `low`, the season is counted as starting THIS
  # calendar year (yr); before `low`, we're still in the tail of the season
  # that started LAST calendar year (yr - 1). E.g. low = 7 (Jul): Aug -> yr,
  # season "yr/yr+1"; Mar -> yr - 1, season "yr-1/yr".
  start_year <- ifelse(mo >= low, yr, yr - 1L)
  tibble::tibble(
    season            = sprintf("%d/%d", start_year, start_year + 1L),
    season_nMonth     = as.integer(offset + 1L),
    season_start_year = as.integer(start_year)
  )
}

#' Read the three calibrated relative-error quantile files, if present.
#'
#' [UNDER REVIEW] These are GDO's static, whole-history files - see the
#' UNDER REVIEW note in the file overview and on nowcast_attach_intervals().
#'
#' @param dir Directory holding the three calibrated_prediction_intervals*.csv
#'   files (spec$calibration_dir).
#' @return Named list `country`/`region`/`global`; an element is NULL if its
#'   file is missing.
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
#' [UNDER REVIEW, 04-09-2026] `calib` here is GDO's *static* file, fit once via
#' LOSO on the full historical record (see nowcast_load_calibration()). That is
#' consistent with a Stage 0 fit on the full panel, but not with Stage 1: each
#' rolling-origin (window, origin) fit trains on a different, often much
#' smaller, set of seasons, and this function would apply the SAME borrowed
#' calibration to all of them regardless. The planned fix is a per-window
#' empirical method computed inside nowcast_fit() itself (e.g. a LOSO sweep
#' over just that fit's training seasons, mirroring
#' Scripts/validation/nowcasting/03_nowcast_validation_ind.R but scoped to
#' `train_df`), stored on the fitted object and consumed here (or replacing
#' this function). That also removes the need for `calibrate_point` (a
#' self-computed residual distribution is already a coherent set of quantiles
#' around a real centre) and frees the interval schema from being locked to
#' whatever quantile levels happen to be in GDO's file - e.g. moving to 50/90 -
#' which could then be retrofitted onto the live GDO nowcast too.
#'
#' Until that lands, this is the interim behaviour: `df` must already have
#' iso3, region, cutoff_month, prediction_month and `.pred_raw`. The
#' relative-error quantiles q_p (quantiles of (actual - est)/est from the LOSO
#' nowcast validation) are taken from the country file where available, else
#' the region file, else the global file - one source per row so the quantiles
#' stay monotone. Then, per row, est(1 + q_p) is treated as the p-th predictive
#' quantile for the truth.
#'
#' @param df Rows needing intervals; must have `iso3`, `region`, `cutoff_month`,
#'   `prediction_month`, `.pred_raw` (the uncalibrated point estimate).
#' @param calib Output of nowcast_load_calibration().
#' @param calibrate_point If TRUE (default), `.pred` is shifted to
#'   `est * (1 + (q_0.25 + q_0.75)/2)` - the IQR midpoint, a bias-corrected
#'   median proxy (the calibrated files carry no q_0.5) - so it always sits
#'   inside the interval. If FALSE, `.pred` stays the raw estimate and the
#'   interval bounds are clamped outward to bracket it instead.
#' @return `df` with `.pred`, `.pred_lower50`, `.pred_upper50`,
#'   `.pred_lower90`, `.pred_upper90` added (all four NA together on rows with
#'   no calibrated match at any level; `.pred` falls back to `.pred_raw` there).
#'   See the INTERIM note in the body: the `_90` columns currently carry the
#'   static file's 2.5 / 97.5 quantiles pending the Step 3 redesign.
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

  # INTERIM (10-09-2026): the contract's outer interval moved to 90% for Stage 1,
  # but GDO's static calibration file only carries 2.5 / 97.5 quantiles. Until the
  # Step 3 per-window self-calibration lands (which computes a real 5 / 95), these
  # `_lower90` / `_upper90` columns actually hold the file's 2.5 / 97.5 quantiles -
  # slightly wider than a true 90%. Only matters for the Stage 0 wiring check.
  out %>%
    dplyr::mutate(
      .pred         = pmax(0, round(dplyr::if_else(is.na(point), est, point))),
      .pred_lower50 = dplyr::if_else(have_q, pmax(0, round(lo50)), NA_real_),
      .pred_upper50 = dplyr::if_else(have_q, pmax(0, round(up50)), NA_real_),
      .pred_lower90 = dplyr::if_else(have_q, pmax(0, round(lo95)), NA_real_),
      .pred_upper90 = dplyr::if_else(have_q, pmax(0, round(up95)), NA_real_)
    ) %>%
    dplyr::select(-dplyr::matches("^q(025|25|75|975)_[crg]$"))
}

#' Fit the seasonal-proportion baseline on one training panel slice.
#'
#' Builds, per country with >= `spec$min_train_seasons` *complete* historical
#' seasons (all 12 season-months present with a proportion value, so an
#' ongoing or partly-seen season cannot leak in): the mean seasonal profile
#' (fit_baseline_profile()) and the mean season total. Also records the
#' current-season state as of the origin (= `max(train_df$date)`): cases
#' observed so far this season, and the season-month `k` that represents.
#'
#' Also loads GDO's calibrated interval files (spec$calibration_dir) - see the
#' UNDER REVIEW note on nowcast_attach_intervals().
#'
#' @param train_df Training panel slice. Needs `iso3`, `region`, `date`,
#'   `cases`, `mean_low_month`, `Actual_monthly_proportion`,
#'   `Actual_cum_monthly_proportion`.
#' @param spec `nowcast_spec`, or a runner's override of it.
#' @return A `"nowcast_model_fit"` list: `spec`, `origin_date`, `profiles`,
#'   `season_totals`, `origin_state`, `cum_to_date`, `region_map`, `low_map`,
#'   `calibration`.
nowcast_fit <- function(train_df, spec = nowcast_spec) {
  spec$next_season_method <- match.arg(spec$next_season_method,
                                       c("mean_T", "carry_T", "NA"))

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

#' Forecast each target from a nowcast_fit() result.
#'
#' For every target row, works out its season position (`nowcast_season_position()`
#' on `target_date`) relative to the fit's origin season, then:
#'   - current-season target (`is_current`): estimated season total
#'     `T = cum_to_date / p_cum_k` once `k >= spec$min_cutoff` and some cases
#'     are observed, else the mean season total; `.pred_raw = T * p_m`
#'   - next-season target (`is_next`): `T` per `spec$next_season_method`
#'     ("mean_T" / "carry_T" / "NA" - see the file overview)
#'   - any other case (further than one season out, or no profile/mean_low_month
#'     for the country): `.pred_raw` is NA
#' Intervals are then attached by `nowcast_attach_intervals()` - see its
#' [UNDER REVIEW] note on the static-calibration method.
#'
#' @param fitted Output of `nowcast_fit()`.
#' @param targets `tibble(iso3, origin_date, horizon, target_date)` - the
#'   contract's target rows (`origin_date` is carried through but not otherwise
#'   used here; the fit's own `origin_date` from `nowcast_fit()` is what
#'   defines "current" vs "next" season).
#' @param spec `nowcast_spec`, or a runner's override of it (defaults to the
#'   spec captured at fit time).
#' @return Tibble with exactly `forecast_output_cols` (`iso3`, `origin_date`,
#'   `horizon`, `target_date`, `.pred`, `.pred_lower50`, `.pred_upper50`,
#'   `.pred_lower90`, `.pred_upper90`).
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
        is_current & !is.na(season_total_signal)          ~ season_total_signal,
        is_current                                        ~ mean_season_total,
        is_next & spec$next_season_method == "carry_T"    ~ dplyr::coalesce(season_total_signal, mean_season_total),
        is_next & spec$next_season_method == "mean_T"     ~ mean_season_total,
        is_next & spec$next_season_method == "NA"         ~ NA_real_,  # deployed-nowcast behaviour: no estimate
        TRUE                                              ~ NA_real_
      ),
      .pred_raw = dplyr::if_else(!is.na(p_m), pmax(0, season_total_used * p_m), NA_real_),
      cutoff_month     = dplyr::if_else(is_current, k, 1L),
      prediction_month = m
    )

  t <- nowcast_attach_intervals(t, fitted$calibration, spec$calibrate_point)

  dplyr::select(t, dplyr::all_of(forecast_output_cols))
}

#' Stage 0 checklist for the nowcast baseline.
#'
#' This isn't a statistical fit in the GLM sense (no likelihood, no
#' convergence, no dispersion parameter), so the checklist is instead: how many
#' countries does the method actually apply to, and did its calibration inputs
#' load. `pass` requires at least one country with a usable profile AND the
#' global calibration file present (without it every interval is NA).
#'
#' @param fitted Output of `nowcast_fit()`.
#' @param train_df Unused; present only for interface parity with the other
#'   models (the contract's `diagnose(fitted, train_df, spec)` signature).
#' @param spec `nowcast_spec`, or a runner's override of it.
#' @return Named list: `n_countries`, `n_countries_with_profile`,
#'   `n_countries_no_profile`, `median_train_seasons`, `min_train_seasons`,
#'   `next_season_method`, `calibration_loaded`, `pass`, `notes`.
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
