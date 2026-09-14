#' ---
#' title: "01_scoring"
#' author: "K M Susong"
#' ---
#'
#' Overview:
#' ========
#' The Stage 1 metric library. DEFINITION ONLY - pure functions, no file I/O,
#' no side effects on source. Every scorer works directly off a model's
#' `forecast_output_cols` (iso3, origin_date, horizon, target_date, .pred,
#' .pred_lower50, .pred_upper50, .pred_lower90, .pred_upper90) joined to the
#' observed truth - so the Stage 1 hindcast runner (and any ad hoc use in the
#' notebook) can score any model with the same code.
#'
#' Quantile levels are NOT hard-coded here: every function derives the tau/
#' alpha it needs from 00_config.R's `interval_probs`, the same single source
#' of truth the models (models/00b_baseline_nowcast.R, models/utils/glm_factory.R)
#' already use. If the interval schema ever changes, this file changes with it.
#'
#' Metrics, per the plan (planning/2026_forecast_evaluation/03_Evaluation_Plan_DRAFT.md
#' §3) and the 2026-09-08 team meeting:
#'   - PRIMARY:   WIS (log scale primary, raw scale kept alongside), with its
#'                dispersion / overprediction / underprediction decomposition
#'                (`wis_components()`, Bracher, Ray, Gneiting & Reich 2021).
#'   - SECONDARY: CRPS (`crps_approx()`, a quantile-based approximation - a
#'                cross-check on WIS, not a promotion gate); PIT (`pit_approx()`)
#'                and interval coverage (`in_interval()`); MAE/RMSE building
#'                blocks for GDO's uMAE/uRMSE convention (`normalise_by_burden()`).
#'   - OPERATIONAL (trajectory-level, one row per (iso3, origin_date)):
#'                peak-timing difference (`peak_timing_diff()`) and DTW distance
#'                (`dtw_distance()`, following Campbell et al. 2026's D-MOSS
#'                evaluation). No `dtw` package dependency - a plain
#'                dynamic-programming DTW is ~15 lines and keeps renv untouched.
#'   - RELATIVE:  `relative_skill()` - the pairwise geometric-mean ratio used
#'                for relative WIS (Cramer et al. 2022) and reused for "vs
#'                nowcast" / "vs GLM AR" / "vs prior best model" comparisons.
#'                Alignment across models (matching rows by iso3 x origin_date
#'                x horizon) is the caller's job, done once real multi-model
#'                Stage 1 output exists.
#'
#' Approximations flagged explicitly (worth a team look before Stage 1 sign-off):
#'   - PIT is approximated by a piecewise-linear CDF through the 5 quantile
#'     points, flat-extrapolated (`stats::approx(..., rule = 2)`) outside
#'     [q05, q95] - so a truth far in either tail piles up at PIT = 0.05 / 0.95
#'     rather than resolving further. A named, common limitation of PIT-from-
#'     quantiles, not a bug.
#'
#' Timeline:
#' ========
#' 11-09-2026: Created.

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(lubridate)
})

if (!exists("interval_probs")) {
  source("Scripts/forecasting/00_config.R")
}

# alpha_k for each central interval implied by interval_probs (1 - the
# interval's nominal coverage). Derived, not hard-coded, so a schema change in
# 00_config.R (e.g. 50/90 -> 50/80) flows straight through.
wis_alphas <- c(
  pi50 = 1 - (interval_probs[["q75"]] - interval_probs[["q25"]]),
  pi90 = 1 - (interval_probs[["q95"]] - interval_probs[["q05"]])
)

# ---- low-level scorers (plain numeric vectors in, vector out) -------------

#' Quantile (pinball) loss.
#'
#' @param actual,q Numeric vectors (recycled against each other): truth and
#'   the tau-th predictive quantile.
#' @param tau A single probability, or a vector the same length as `actual`.
#' @return Numeric vector, same length as `actual`.
pinball_loss <- function(actual, q, tau) {
  (actual - q) * (tau - as.numeric(actual < q))
}

#' Interval score at one central-interval level (Bracher et al. 2021, eq. 4).
#'
#' `IS_alpha(y) = (upper - lower) + (2/alpha)(lower - y)_+ + (2/alpha)(y - upper)_+`
#'
#' @param actual,lower,upper Numeric vectors.
#' @param alpha 1 - nominal interval coverage (e.g. 0.5 for a 50% interval).
#' @return Numeric vector.
interval_score <- function(actual, lower, upper, alpha) {
  (upper - lower) +
    (2 / alpha) * pmax(lower - actual, 0) +
    (2 / alpha) * pmax(actual - upper, 0)
}

#' Weighted interval score, decomposed into dispersion / over- / under-
#' prediction (Bracher, Ray, Gneiting & Reich 2021, *PLOS Comp Biol*).
#'
#' `WIS = 1/(K+0.5) * [ 0.5|y-m| + sum_k (alpha_k/2) IS_{alpha_k}(y) ]`, K = 2
#' here (the 50% and 90% intervals `interval_probs` defines). The three
#' components sum exactly to `wis`:
#'   - `dispersion`: the median term plus each interval's width, weighted -
#'     how sharp the forecast is, before any penalty for missing the truth.
#'   - `overprediction`: penalty when the truth falls BELOW a lower bound
#'     (the forecast's central mass sat too high).
#'   - `underprediction`: penalty when the truth falls ABOVE an upper bound
#'     (the forecast's central mass sat too low).
#'
#' @param actual,median,lower50,upper50,lower90,upper90 Numeric vectors, same
#'   length (recycled). Pass log1p-transformed values for the log-scale WIS -
#'   the formula is scale-agnostic as long as `lower <= median <= upper` holds
#'   on whatever scale is passed in (true after any monotone transform).
#' @return A data frame with columns `wis`, `dispersion`, `overprediction`,
#'   `underprediction`, one row per input.
wis_components <- function(actual, median, lower50, upper50, lower90, upper90) {
  k <- length(wis_alphas)
  denom <- k + 0.5
  
  is50 <- interval_score(actual, lower50, upper50, wis_alphas[["pi50"]])
  is90 <- interval_score(actual, lower90, upper90, wis_alphas[["pi90"]])
  w50  <- wis_alphas[["pi50"]] / 2
  w90  <- wis_alphas[["pi90"]] / 2
  
  median_term  <- 0.5 * abs(actual - median)
  dispersion   <- (median_term + w50 * (upper50 - lower50) + w90 * (upper90 - lower90)) / denom
  # w_k * (2/alpha_k) * (.)_+ = (.)_+ exactly, for any alpha_k - see the file
  # docstring's decomposition note. NA in any bound already propagates through
  # pmax()/abs(); the explicit NA-out below only covers the one gap that
  # doesn't propagate on its own - a NA median with valid bounds (never
  # produced by GDO's own models, whose predict() nulls all five columns
  # together, but not guaranteed for an arbitrary caller).
  overpred  <- (pmax(lower50 - actual, 0) + pmax(lower90 - actual, 0)) / denom
  underpred <- (pmax(actual - upper50, 0) + pmax(actual - upper90, 0)) / denom
  wis <- (median_term + w50 * is50 + w90 * is90) / denom
  
  no_median <- is.na(actual) | is.na(median)
  dispersion[no_median] <- NA_real_
  overpred[no_median]   <- NA_real_
  underpred[no_median]  <- NA_real_
  wis[no_median]        <- NA_real_
  
  data.frame(wis = wis, dispersion = dispersion,
             overprediction = overpred, underprediction = underpred)
}

#' Quantile-based CRPS approximation (Gneiting & Raftery 2007; the convention
#' `scoringutils`/hubverse fall back to when only a finite quantile set is
#' available). `CRPS ~= 2 * mean_tau[ pinball_loss(y, q_tau, tau) ]` over the
#' quantile levels in `interval_probs`.
#'
#' [NOTE, verified 11-09-2026] With exactly this quantile set - one median
#' plus the bounds of every interval WIS uses - this is not merely close to
#' `wis`/`wis_log`, it is ALGEBRAICALLY IDENTICAL to it: `pinball(y,l,a/2) +
#' pinball(y,u,1-a/2) = (a/2) IS_a(y)` is an exact identity for any interval,
#' and `pinball(y,m,0.5) = 0.5|y-m|` is the WIS median term, so summing all 5
#' pinball losses and dividing by 5 (this function) reproduces `wis_components()`'s
#' numerator divided by `K + 0.5` (that function) up to the constant that makes
#' the two coincide exactly - confirmed numerically (max diff ~1e-13 over 2000
#' random cases). So today `crps`/`crps_log` in `score_forecast()`'s output add
#' no information beyond `wis`/`wis_log` - kept for continuity with the plan's
#' "keep CRPS alongside WIS" decision, and because it stops being an identity
#' the moment a model's quantile set differs from `interval_probs` (e.g. a
#' future model exposing a denser posterior).
#'
#' @param actual Numeric vector.
#' @param q05,q25,q50,q75,q95 Numeric vectors, same length as `actual`.
#' @return Numeric vector.
crps_approx <- function(actual, q05, q25, q50, q75, q95) {
  qs   <- list(q05 = q05, q25 = q25, q50 = q50, q75 = q75, q95 = q95)
  taus <- interval_probs[names(qs)]
  losses <- Map(function(q, tau) pinball_loss(actual, q, tau), qs, taus)
  2 * Reduce(`+`, losses) / length(losses)
}

#' Approximate PIT value from a 5-point quantile forecast.
#'
#' Piecewise-linear interpolation of the CDF through
#' `(q05,0.05), (q25,0.25), (q50,0.5), (q75,0.75), (q95,0.95)`, evaluated at
#' `actual`; flat-extrapolated outside `[q05, q95]` (`stats::approx(rule = 2)`),
#' so a truth beyond the outer quantiles returns 0.05 / 0.95 rather than a
#' fully resolved tail value - see the file overview. Degenerate rows (all five
#' quantiles identical, e.g. an all-zero-history country) return `0.5` when
#' `actual` equals that value and `0`/`1` otherwise; PARTIALLY tied quantiles
#' (e.g. `q05 == q25 == 0`, routine for a low-incidence country's lower bound)
#' are handled by `stats::approx()` itself (duplicate x-values average their
#' tau) and its resulting "collapsing to unique x values" message is expected
#' here, so suppressed.
#'
#' @param actual Numeric scalar or vector.
#' @param q05,q25,q50,q75,q95 Numeric, same length as `actual` (recycled if
#'   scalar-per-row is easier for the caller: this function is row-wise, call
#'   it via e.g. `purrr::pmap_dbl()` for a whole data frame).
#' @return A single PIT value in `[0, 1]`, or `NA` if any input is `NA`.
pit_approx <- function(actual, q05, q25, q50, q75, q95) {
  qs   <- c(q05, q25, q50, q75, q95)
  taus <- unname(interval_probs[c("q05", "q25", "q50", "q75", "q95")])
  if (anyNA(c(actual, qs))) return(NA_real_)
  if (length(unique(qs)) < 2) {
    return(dplyr::case_when(actual == qs[1] ~ 0.5, actual < qs[1] ~ 0, TRUE ~ 1))
  }
  # Partially-tied quantiles (e.g. q05 == q25 == 0, routine for a low-incidence
  # country's lower bound) make approx() collapse duplicate x-values (it
  # averages their tau) and warn about it - expected here, so suppressed.
  suppressWarnings(
    as.numeric(stats::approx(x = qs, y = taus, xout = actual, rule = 2)$y)
  )
}

#' Is `actual` inside the nominal interval `[lower, upper]`?
#'
#' @param actual,lower,upper Numeric vectors.
#' @return Logical vector.
in_interval <- function(actual, lower, upper) {
  actual >= lower & actual <= upper
}

#' Burden-normalise an error metric (GDO's u* convention: uMAE, uRMSE, ...).
#'
#' `burden` should be the same "mean_actual_predicted_month" quantity used
#' throughout `Scripts/validation/` (`mean_actual_by_prediction_month()` in
#' `00_FUN_validation_metrics.R`) - typically a country's mean historical
#' cases for the target calendar/season month, computed once by the caller and
#' joined in, not recomputed here.
#'
#' @param x A row-level error (e.g. `abs_error`); aggregate (e.g. `mean(abs_error)`)
#'   before dividing if you want the usual uMAE/uRMSE, not a per-row ratio.
#' @param burden Numeric vector/scalar, same length as `x` or length 1.
#' @return `x / burden`, `NA` where `burden` is 0 or missing.
normalise_by_burden <- function(x, burden) {
  dplyr::if_else(!is.na(burden) & burden > 0, x / burden, NA_real_)
}

# ---- row-level scoring wrapper ---------------------------------------------

#' Score one model's forecasts against observed truth, row by row.
#'
#' @param df A data frame with the model contract's `forecast_output_cols`
#'   (`iso3`, `origin_date`, `horizon`, `target_date`, `.pred`,
#'   `.pred_lower50`, `.pred_upper50`, `.pred_lower90`, `.pred_upper90`) plus
#'   an actual-value column (`actual_col`, default `"actual"`) already joined
#'   in by the caller (e.g. from the training panel's `cases`).
#' @param actual_col Name of the truth column in `df`.
#' @return `df` with added columns: `error` (actual - .pred, signed: positive
#'   = model underpredicted), `abs_error`, `sq_error`; `wis`/`wis_dispersion`/
#'   `wis_overprediction`/`wis_underprediction` (raw scale) and the same four
#'   `wis_log_*` (log1p scale, PRIMARY per the plan); `crps`, `crps_log`;
#'   `in50`, `in90` (interval coverage indicators); `pit` (approximate PIT).
#'   Rows with any `NA` among the required inputs score `NA` throughout except
#'   `error`/`abs_error`/`sq_error`, which are `NA` individually as usual.
score_forecast <- function(df, actual_col = "actual") {
  need <- c("iso3", "origin_date", "horizon", "target_date",
            ".pred", ".pred_lower50", ".pred_upper50",
            ".pred_lower90", ".pred_upper90", actual_col)
  miss <- setdiff(need, names(df))
  if (length(miss) > 0) {
    cli::cli_abort("`df` is missing column{?s}: {.field {miss}}.")
  }
  
  actual <- df[[actual_col]]
  raw <- wis_components(actual, df$.pred, df$.pred_lower50, df$.pred_upper50,
                        df$.pred_lower90, df$.pred_upper90)
  log_raw <- wis_components(
    log1p(actual), log1p(df$.pred), log1p(df$.pred_lower50), log1p(df$.pred_upper50),
    log1p(df$.pred_lower90), log1p(df$.pred_upper90)
  )
  
  df %>%
    dplyr::mutate(
      error     = actual - .pred,
      abs_error = abs(error),
      sq_error  = error^2,
      
      wis                = raw$wis,
      wis_dispersion     = raw$dispersion,
      wis_overprediction = raw$overprediction,
      wis_underprediction = raw$underprediction,
      
      wis_log                = log_raw$wis,
      wis_log_dispersion     = log_raw$dispersion,
      wis_log_overprediction = log_raw$overprediction,
      wis_log_underprediction = log_raw$underprediction,
      
      crps     = crps_approx(actual, .pred_lower90, .pred_lower50, .pred,
                             .pred_upper50, .pred_upper90),
      crps_log = crps_approx(log1p(actual), log1p(.pred_lower90), log1p(.pred_lower50),
                             log1p(.pred), log1p(.pred_upper50), log1p(.pred_upper90)),
      
      # A valid interval with no point forecast shouldn't happen under the
      # model contract (predict() nulls all five columns together), but the
      # explicit NA here keeps coverage consistent with wis/wis_log if it ever
      # did, rather than reading as a (wrong) miss.
      in50 = dplyr::if_else(is.na(.pred), NA, in_interval(actual, .pred_lower50, .pred_upper50)),
      in90 = dplyr::if_else(is.na(.pred), NA, in_interval(actual, .pred_lower90, .pred_upper90)),
      
      pit = purrr::pmap_dbl(
        list(actual, .pred_lower90, .pred_lower50, .pred, .pred_upper50, .pred_upper90),
        pit_approx
      )
    )
}

# ---- trajectory-level scorers (one (iso3, origin_date) at a time) ---------

#' Peak-timing difference, in months, between an observed and a predicted
#' trajectory (Campbell et al. 2026's D-MOSS metric).
#'
#' Positive = the model's peak lands AFTER the observed peak; negative = the
#' model peaks too early. Restricted to rows where both `actual` and
#' `predicted` are observed, so the two peaks are found over the same window;
#' returns `NA` if fewer than `min_months` such rows remain (a peak read off
#' 1-2 months is not a meaningful comparison).
#'
#' @param target_date Date vector (first-of-month), one per horizon.
#' @param actual,predicted Numeric vectors, same length as `target_date`.
#' @param min_months Minimum complete (actual & predicted both present) months
#'   required to compute a peak at all.
#' @return A single integer (months), or `NA`.
peak_timing_diff <- function(target_date, actual, predicted, min_months = 3L) {
  ok <- !is.na(actual) & !is.na(predicted)
  if (sum(ok) < min_months) return(NA_integer_)
  d  <- target_date[ok]
  a  <- actual[ok]
  p  <- predicted[ok]
  peak_actual    <- d[which.max(a)]
  peak_predicted <- d[which.max(p)]
  (lubridate::year(peak_predicted)  - lubridate::year(peak_actual))  * 12L +
    (lubridate::month(peak_predicted) - lubridate::month(peak_actual))
}

#' Dynamic time warping distance between an observed and a predicted
#' trajectory (Campbell et al. 2026's D-MOSS metric: trajectory shape,
#' tolerant of a timing offset the pointwise metrics above penalise). Plain
#' O(n*m) dynamic programming - no `dtw` package dependency, since GDO's
#' trajectories are always a handful of months long.
#'
#' @param actual,predicted Numeric vectors (need not be the same length; `NA`s
#'   are dropped from each independently before alignment).
#' @param dist "absolute" (default) or "squared" local cost.
#' @return A single numeric distance (cumulative, unnormalised), or `NA` if
#'   either input has fewer than 1 non-`NA` value after dropping `NA`s.
dtw_distance <- function(actual, predicted, dist = c("absolute", "squared")) {
  dist <- match.arg(dist)
  x <- actual[!is.na(actual)]
  y <- predicted[!is.na(predicted)]
  nx <- length(x); ny <- length(y)
  if (nx < 1L || ny < 1L) return(NA_real_)
  
  cost_fn <- if (dist == "squared") function(a, b) (a - b)^2 else function(a, b) abs(a - b)
  cost <- outer(x, y, cost_fn)
  
  acc <- matrix(Inf, nx, ny)
  acc[1, 1] <- cost[1, 1]
  if (nx > 1L) for (i in 2:nx) acc[i, 1] <- acc[i - 1, 1] + cost[i, 1]
  if (ny > 1L) for (j in 2:ny) acc[1, j] <- acc[1, j - 1] + cost[1, j]
  if (nx > 1L && ny > 1L) {
    for (i in 2:nx) {
      for (j in 2:ny) {
        acc[i, j] <- cost[i, j] + min(acc[i - 1, j], acc[i, j - 1], acc[i - 1, j - 1])
      }
    }
  }
  acc[nx, ny]
}

# ---- relative comparison ---------------------------------------------------

#' Pairwise relative skill (relative WIS convention: Cramer et al. 2022, PNAS
#' - the COVID-19 Forecast Hub's primary cross-model metric).
#'
#' The geometric mean of the score ratio over every row where `score_model`
#' and `score_baseline` are both finite and positive - robust to the handful
#' of large scores that would dominate an arithmetic-mean ratio. `< 1` means
#' `score_model` beats `score_baseline` (lower score = better, for WIS/CRPS/MAE
#' etc.). Alignment (matching rows by iso3 x origin_date x horizon, or
#' whatever grain the comparison is at) is the caller's responsibility - this
#' function only takes two already-paired vectors.
#'
#' @param score_model,score_baseline Numeric vectors, same length, paired
#'   row-for-row (e.g. two models' `wis_log` on identical forecast rows).
#' @return A single numeric ratio (`NA` if no row has both finite and positive).
relative_skill <- function(score_model, score_baseline) {
  ratio <- score_model / score_baseline
  ok <- is.finite(ratio) & ratio > 0
  if (!any(ok)) return(NA_real_)
  exp(mean(log(ratio[ok])))
}
