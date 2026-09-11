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
#' DEFINITION ONLY - this file builds `nowcast_model` and does nothing else on
#' source. No file I/O: the predictive intervals are computed from each fit's
#' own training seasons, not read from disk.
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
#'        - the mean seasonal profile via fit_baseline_profile()
#'        - the mean season total
#'        - the current season's cases observed up to the origin, and the
#'          origin's season-month k
#'        - relative-error quantiles from a within-window forward-origin sweep
#'          (nowcast_calibrate(); see "Uncertainty" below)
#'   2. predict() for a target at season-month m:
#'        - estimated season total T = (cases observed to date) / (mean
#'          cumulative proportion at k); the mean season total is used when no
#'          cases are in yet or the ratio is not usable
#'        - raw estimate = T * (mean monthly proportion at m)
#'        - point + intervals = raw * (1 + q_p), where q_p are the relative-error
#'          quantiles for this (country, k, m). `.pred` is the calibrated median
#'          (raw * (1 + q_0.5)); the 50% / 90% bands are raw * (1 + q_0.25/0.75)
#'          and raw * (1 + q_0.05/0.95). One source per row, so monotone.
#'
#'      ! Targets in the *next* season use spec$next_season_method: "mean_T"
#'      (mean season total), "carry_T" (this year's T carried forward), or "NA"
#'      (deployed-GDO behaviour: no estimate if no cases seen this season).
#'
#' Uncertainty (nowcast_calibrate)
#' ===============================
#' Interval width is derived FROM the training window handed to fit(), not from
#' a static whole-history file. For each eligible country and each complete
#' season S with >= spec$calib_min_prior seasons before it, a profile is fit on
#' the seasons strictly before S and used to nowcast S at every cutoff
#' k = 1..11; the relative error (actual - predicted) / predicted is recorded
#' per predicted month. Quantiles of those residuals are the predictive
#' intervals. A profile is only ever tested on a season later than every season
#' it trained on, so this mirrors the Stage 1 rolling-origin design nested
#' inside one window and leaks nothing - and a 5-year window and a 15-year
#' window get appropriately different interval widths.
#'
#' Residuals are pooled with a graded fallback when a stratum is thin
#' (< spec$calib_min_obs): (iso3, k, m) -> (iso3, k) -> (k, m) global ->
#' (k) global. A row matching nothing keeps the raw point and gets NA intervals.
#'
#' Timeline:
#' ========
#' 03-09-2026: Created.
#' 04-09-2026: Second review. Renamed next_season_method values. Flagged the
#'   static-calibration interval method as needing a per-window redesign before
#'   Stage 1. min_cutoff confirmed to NOT be a GDO parameter.
#' 10-09-2026: Interval redesign (Step 3). Replaced the static
#'   calibrated_prediction_intervals*.csv lookup with an in-fit forward-origin
#'   residual sweep (nowcast_calibrate); intervals are now 50% / 90% and
#'   self-consistent per training window. Removed spec$calibration_dir,
#'   spec$calibrate_point and spec$min_cutoff - the k=1 guard is absorbed: a
#'   wild low-k estimate now just carries wide calibrated intervals, which
#'   matches deployed GDO (it applies no floor).

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(lubridate)
  library(purrr)
})

if (!exists("interval_probs")) {
  source("Scripts/forecasting/00_config.R")
}
if (!exists("new_forecast_model")) {
  source("Scripts/forecasting/models/utils/forecast_helpers.R")
}
if (!exists("fit_baseline_profile")) {
  source("Scripts/validation/FUNCTIONS/00_FUN_validation_metrics.R")
}

# Predictive quantile probabilities - q05 / q95 are the bounds of the 90%
# interval, q25 / q75 the 50%, q50 the median (matches forecast_output_cols).
# Single source of truth is 00_config.R's `interval_probs`, so this can't drift
# from what the GLM factory (or any other model) targets.
nowcast_probs <- interval_probs

nowcast_spec <- list(
  min_train_seasons  = 3L,   # complete historical seasons needed for a profile
  next_season_method = "mean_T",  # "mean_T" | "carry_T" | "NA"
                             # (a *string* "NA", not R's NA -- matches how the
                             # deployed nowcast labels "no estimate this season")
  calib_min_prior    = 2L,   # seasons that must precede a held-out season for
                             # it to be used as a calibration test season
  calib_min_obs      = 5L    # residuals a (country,k,m) etc. stratum needs
                             # before its own quantiles are trusted; below this
                             # the graded fallback takes over
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

#' Relative-error quantiles from a within-window forward-origin sweep.
#'
#' See the "Uncertainty" section of the file overview. For each country in `cs`
#' and each complete season with at least `min_prior` seasons before it, fit a
#' profile on the earlier seasons only and nowcast the held-out season at every
#' cutoff k = 1..11 (reusing `nowcast_one_cutoff()`); collect the relative
#' errors and take quantiles at several strata.
#'
#' @param cs Complete-seasons frame in season-month space: needs `iso3`,
#'   `season`, `season_start_year`, `season_nMonth`, `Month`, `cases`,
#'   `Actual_monthly_proportion`, `Actual_cum_monthly_proportion`.
#' @param min_prior,min_obs `spec$calib_min_prior`, `spec$calib_min_obs`.
#' @return list: `ckm` (iso3 x k x m), `ck` (iso3 x k), `km` (k x m, global),
#'   `k` (k, global) quantile tables (columns n, q05, q25, q50, q75, q95);
#'   `n_calib` (per-country residual-season / residual counts); `n_resid`.
nowcast_calibrate <- function(cs, min_prior = 2L, min_obs = 5L) {
  # One country's forward-origin residual rows (empty if too few seasons).
  one_country <- function(cd) {
    s <- cd %>%
      dplyr::distinct(season, season_start_year) %>%
      dplyr::arrange(season_start_year, season)
    if (nrow(s) <= min_prior) return(NULL)
    purrr::map_dfr((min_prior + 1L):nrow(s), function(i) {
      base <- fit_baseline_profile(
        dplyr::filter(cd, season %in% s$season[seq_len(i - 1L)])
      )
      test_df <- cd %>%
        dplyr::filter(season == s$season[i]) %>%
        dplyr::select(season_nMonth, Month, cases)
      rows <- purrr::map_dfr(1:11, ~ nowcast_one_cutoff(test_df, base, .x))
      if (nrow(rows) == 0) return(NULL)
      rows$test_season <- s$season[i]
      rows
    })
  }

  resid <- cs %>%
    dplyr::group_by(iso3) %>%
    dplyr::group_modify(~ {
      r <- one_country(.x)
      if (is.null(r) || nrow(r) == 0) {
        tibble::tibble(
          test_season = character(), cutoff_month = integer(),
          prediction_month = integer(), predicted_cases = double(),
          actual_cases = double()
        )
      } else {
        r
      }
    }) %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.finite(predicted_cases), predicted_cases > 0,
                  is.finite(actual_cases)) %>%
    dplyr::mutate(rel_err = (actual_cases - predicted_cases) / predicted_cases)

  q_tbl <- function(df, ...) {
    df %>%
      dplyr::group_by(...) %>%
      dplyr::summarise(
        n   = dplyr::n(),
        q05 = stats::quantile(rel_err, nowcast_probs[["q05"]], names = FALSE, type = 7),
        q25 = stats::quantile(rel_err, nowcast_probs[["q25"]], names = FALSE, type = 7),
        q50 = stats::quantile(rel_err, nowcast_probs[["q50"]], names = FALSE, type = 7),
        q75 = stats::quantile(rel_err, nowcast_probs[["q75"]], names = FALSE, type = 7),
        q95 = stats::quantile(rel_err, nowcast_probs[["q95"]], names = FALSE, type = 7),
        .groups = "drop"
      )
  }

  n_calib <- resid %>%
    dplyr::group_by(iso3) %>%
    dplyr::summarise(
      n_calib_seasons = dplyr::n_distinct(test_season),
      n_calib_resid   = dplyr::n(),
      .groups = "drop"
    )

  list(
    ckm     = dplyr::filter(q_tbl(resid, iso3, cutoff_month, prediction_month), n >= min_obs),
    ck      = dplyr::filter(q_tbl(resid, iso3, cutoff_month), n >= min_obs),
    km      = dplyr::filter(q_tbl(resid, cutoff_month, prediction_month), n >= min_obs),
    k       = q_tbl(resid, cutoff_month),
    n_calib = n_calib,
    n_resid = nrow(resid)
  )
}

#' Attach point + 50% / 90% intervals from the calibration tables.
#'
#' Per row, take the (iso3, k, m) quantiles where available, else (iso3, k),
#' else global (k, m), else global (k) - one source per row so q05 < ... < q95
#' stays true. `.pred` becomes the calibrated median raw*(1 + q50); the bands
#' are raw*(1 + q_p). A row with no `.pred_raw` gets all NA; a row with a raw
#' estimate but no calibration match keeps the raw point and gets NA intervals.
#'
#' @param t Rows needing intervals; must have `iso3`, `cutoff_month`,
#'   `prediction_month`, `.pred_raw`.
#' @param cal Output of `nowcast_calibrate()`.
#' @return `t` with `.pred`, `.pred_lower50/upper50`, `.pred_lower90/upper90`.
nowcast_apply_intervals <- function(t, cal) {
  qn <- c("q05", "q25", "q50", "q75", "q95")
  tag <- function(tbl, sfx) dplyr::rename_with(tbl, ~ paste0(.x, sfx), dplyr::all_of(qn))

  drop_n <- function(tbl) dplyr::select(tbl, -dplyr::any_of("n"))
  out <- t %>%
    dplyr::left_join(tag(drop_n(cal$ckm), "_ckm"),
      by = c("iso3", "cutoff_month", "prediction_month")) %>%
    dplyr::left_join(tag(drop_n(cal$ck), "_ck"),
      by = c("iso3", "cutoff_month")) %>%
    dplyr::left_join(tag(drop_n(cal$km), "_km"),
      by = c("cutoff_month", "prediction_month")) %>%
    dplyr::left_join(tag(drop_n(cal$k), "_k"),
      by = "cutoff_month")

  src <- dplyr::case_when(
    !is.na(out$q50_ckm) ~ "ckm",
    !is.na(out$q50_ck)  ~ "ck",
    !is.na(out$q50_km)  ~ "km",
    !is.na(out$q50_k)   ~ "k",
    TRUE                ~ NA_character_
  )
  pick <- function(q) {
    v <- rep(NA_real_, nrow(out))
    for (sfx in c("ckm", "ck", "km", "k")) {
      col <- paste0(q, "_", sfx)
      if (col %in% names(out)) {
        rows <- !is.na(src) & src == sfx
        v[rows] <- out[[col]][rows]
      }
    }
    v
  }
  q05 <- pick("q05"); q25 <- pick("q25"); q50 <- pick("q50")
  q75 <- pick("q75"); q95 <- pick("q95")

  raw  <- out$.pred_raw
  have <- !is.na(raw) & !is.na(q50)

  out %>%
    dplyr::mutate(
      .pred = dplyr::case_when(
        is.na(raw) ~ NA_real_,
        have       ~ pmax(0, round(raw * (1 + q50))),
        TRUE       ~ pmax(0, round(raw))
      ),
      .pred_lower50 = dplyr::if_else(have, pmax(0, round(raw * (1 + q25))), NA_real_),
      .pred_upper50 = dplyr::if_else(have, pmax(0, round(raw * (1 + q75))), NA_real_),
      .pred_lower90 = dplyr::if_else(have, pmax(0, round(raw * (1 + q05))), NA_real_),
      .pred_upper90 = dplyr::if_else(have, pmax(0, round(raw * (1 + q95))), NA_real_)
    ) %>%
    dplyr::select(-dplyr::matches("^q(05|25|50|75|95)_(ckm|ck|km|k)$"))
}

#' Fit the seasonal-proportion baseline on one training panel slice.
#'
#' Builds, per country with >= `spec$min_train_seasons` *complete* historical
#' seasons (all 12 season-months present with a proportion value, so an
#' ongoing or partly-seen season cannot leak in): the mean seasonal profile
#' (fit_baseline_profile()), the mean season total, the current-season state as
#' of the origin (= `max(train_df$date)`), and the relative-error quantiles
#' from `nowcast_calibrate()`.
#'
#' @param train_df Training panel slice. Needs `iso3`, `region`, `date`,
#'   `cases`, `mean_low_month`, `Actual_monthly_proportion`,
#'   `Actual_cum_monthly_proportion`.
#' @param spec `nowcast_spec`, or a runner's override of it.
#' @return A `"nowcast_model_fit"` list: `spec`, `origin_date`, `profiles`,
#'   `season_totals`, `origin_state`, `cum_to_date`, `region_map`, `low_map`,
#'   `rel_err_q`.
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
  df$Month <- lubridate::month(df$date)

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

  rel_err_q <- nowcast_calibrate(
    dplyr::filter(complete_seasons, iso3 %in% eligible),
    min_prior = spec$calib_min_prior,
    min_obs   = spec$calib_min_obs
  )

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
    rel_err_q     = rel_err_q
  ), class = "nowcast_model_fit")
}

#' Forecast each target from a nowcast_fit() result.
#'
#' For every target row, works out its season position (`nowcast_season_position()`
#' on `target_date`) relative to the fit's origin season, then:
#'   - current-season target (`is_current`): estimated season total
#'     `T = cum_to_date / p_cum_k` when that ratio is usable, else the mean
#'     season total; `.pred_raw = T * p_m`
#'   - next-season target (`is_next`): `T` per `spec$next_season_method`
#'   - anything else (further than one season out, or no profile for the
#'     country): `.pred_raw` is NA
#' Intervals are then attached by `nowcast_apply_intervals()`.
#'
#' @param fitted Output of `nowcast_fit()`.
#' @param targets `tibble(iso3, origin_date, horizon, target_date)`.
#' @param spec `nowcast_spec`, or a runner's override of it.
#' @return Tibble with exactly `forecast_output_cols`.
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
    dplyr::left_join(prof_pk, by = c("iso3", "k")) %>%
    dplyr::left_join(prof_pm, by = c("iso3", "m")) %>%
    dplyr::mutate(
      is_current = !is.na(origin_season) & target_season == origin_season,
      is_next    = !is.na(origin_start_year) & target_start_year == origin_start_year + 1L,
      # this-year season total; usable once some cases are in and p_cum_k is
      # positive (no k floor - matches deployed GDO; low-k noise is carried by
      # the calibrated intervals instead).
      season_total_signal = dplyr::if_else(
        !is.na(p_cum_k) & p_cum_k > 0 & !is.na(cum_to_date) & cum_to_date > 0,
        cum_to_date / p_cum_k, NA_real_
      ),
      season_total_used = dplyr::case_when(
        is_current & !is.na(season_total_signal)          ~ season_total_signal,
        is_current                                        ~ mean_season_total,
        is_next & spec$next_season_method == "carry_T"    ~ dplyr::coalesce(season_total_signal, mean_season_total),
        is_next & spec$next_season_method == "mean_T"     ~ mean_season_total,
        is_next & spec$next_season_method == "NA"         ~ NA_real_,
        TRUE                                              ~ NA_real_
      ),
      .pred_raw = dplyr::if_else(!is.na(p_m), pmax(0, season_total_used * p_m), NA_real_),
      cutoff_month     = dplyr::if_else(is_current, k, 1L),
      prediction_month = m
    )

  t <- nowcast_apply_intervals(t, fitted$rel_err_q)

  dplyr::select(t, dplyr::all_of(forecast_output_cols))
}

#' Stage 0 checklist for the nowcast baseline.
#'
#' Not a likelihood fit, so the checklist is: how many countries does the method
#' apply to, and did the self-calibration produce residuals. `pass` requires at
#' least one country with a usable profile.
#'
#' @param fitted Output of `nowcast_fit()`.
#' @param train_df Unused; interface parity only.
#' @param spec `nowcast_spec`, or a runner's override of it.
#' @return Named list of scalars + `pass` + `notes`.
nowcast_diagnose <- function(fitted, train_df = NULL, spec = fitted$spec) {
  all_iso      <- unique(fitted$region_map$iso3)
  with_profile <- unique(fitted$profiles$iso3)
  st           <- fitted$season_totals
  nc           <- fitted$rel_err_q$n_calib

  list(
    model                    = "nowcast",
    n_countries              = length(all_iso),
    n_countries_with_profile = length(with_profile),
    n_countries_no_profile   = length(setdiff(all_iso, with_profile)),
    median_train_seasons     = if (nrow(st) > 0) stats::median(st$n_train_seasons) else NA_real_,
    min_train_seasons        = spec$min_train_seasons,
    next_season_method       = spec$next_season_method,
    n_countries_calibrated   = nrow(nc),
    median_calib_seasons     = if (nrow(nc) > 0) stats::median(nc$n_calib_seasons) else 0,
    n_calib_residuals        = fitted$rel_err_q$n_resid,
    n_cells_country_km       = nrow(fitted$rel_err_q$ckm),
    pass                     = length(with_profile) > 0,
    notes = sprintf(
      "seasonal-proportion baseline; profile for %d/%d countries (>= %d seasons); next-season = %s; self-calibrated from %d residuals across %d countries",
      length(with_profile), length(all_iso), spec$min_train_seasons,
      spec$next_season_method, fitted$rel_err_q$n_resid, nrow(nc)
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
