#' ---
#' title: "01_scoring"
#' author: "K M Susong"
#' ---
#'
#' Overview:
#' ========
#' The GDO forecasting objective's metric library (Stage 1 hindcast, Stage 2
#' shadow-deployment, ad hoc notebook use - all call the same code).
#' 
#' Built on `scoringutils` (Bosse et al. 2022; epiforecasts.io) and `dtw`, 
#' rather than hand-rolled WIS/PIT/DTW. Quantile levels come from `00_config.R`'s .
#' `interval_probs` - currently 5 (50%/90% intervals + median). 
#' `score_forecast()` takes either shape a model can
#' emit: QUANTILE (`forecast_output_cols`, today's only model type) or SAMPLE
#' (`sample_id` + `predicted`, for a future hierarchical/INLA model).
#'
#' Metrics, by role:
#'   - PRIMARY:   CRPS (log scale primary, raw kept alongside) - approximated
#'                from the 5 quantiles for a quantile forecast (`crps_approx()`,
#'                equivelent to WIS at quartile number = 5, Bracher, Ray, Gneiting &
#'                Reich 2021), real (`scoringRules::crps_sample()`) for a sample forecast.
#'                Decomposed into dispersion/overprediction/
#'                underprediction regardless of shape. 
#'   - SECONDARY: PIT (`pit_histogram()`) and coverage (`coverage_diagnostics()`,
#'                `interval_coverage_50/90`, quantile forecasts only); `ae_median`
#'                (`_log`) for GDO's uMAE/uRMSE convention (`normalise_by_burden()`).
#'   - OPERATIONAL: peak-timing difference (`peak_timing_diff()`, GDO-specific)
#'                and DTW distance (`dtw_distance()`, wraps `dtw::dtw()` -
#'                Campbell et al. 2026's D-MOSS metric; two axes - one
#'                forecast's own trajectory, or one lead across rolling origins).
#'   - RELATIVE:  `relative_skill()` wraps `get_pairwise_comparisons()` for
#'                relative WIS (Cramer et al. 2022) - internally still scored
#'                as "wis" (scoringutils' own column name for a quantile
#'                forecast), identical to `crps` above.
#'
#' Timeline:
#' ========
#' 11-09-2026: Created (hand-rolled WIS/CRPS/PIT).
#' 11-09-2026: Rewritten on `scoringutils` + `dtw`.
#' 14-09-2026: Re-added the CRPS approximation (`crps_approx()`) and made
#'   `score_forecast()` dispatch on forecast shape: approx CRPS for quantile
#'   forecasts, real `scoringutils` CRPS for sample forecasts.
#' 14-09-2026: Dropped `wis`/`wis_log` from `score_forecast()`'s output -
#'   confirmed identical to `crps`/`crps_log` in both forecast shapes, so
#'   carrying both was redundant. `dispersion`/`overprediction`/`underprediction`
#'   (that decomposition) kept, now read as decomposing `crps` directly.
#' 14-09-2026: `forecast_unit_cols` threaded through as a `unit_cols` argument
#'   (defaulting to the constant) on every function that builds a
#'   `scoringutils` forecast object or joins on the unit, instead of being
#'   read as a fixed global. Stage 1 will need `window_type` added to the unit
#'   whenever a call stacks >1 window; Stage 2 will need its own extra
#'   dimension (e.g. champion/shadow). Neither requires editing this file -
#'   the caller just passes a wider `unit_cols`.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(scoringutils)
  library(dtw)
  library(lubridate)
})

if (!exists("interval_probs")) {
  source("Scripts/forecasting/00_config.R")
}

forecast_unit_cols <- c("iso3", "origin_date", "horizon", "target_date", "model")
log_metric_cols <- c("dispersion", "overprediction", "underprediction", "ae_median")

# ---- wide/long -> scoringutils forecast objects ----------------------------

#' Reshape a wide quantile forecast (`forecast_output_cols`) into a
#' `scoringutils` `forecast_quantile` object.
#'
#' @param df Data frame with `forecast_output_cols` + `model` + `actual_col`.
#' @param actual_col Name of the truth column in `df`.
#' @param log_scale Log1p-transform predicted and observed first
#'   (`scoringutils::log_shift(offset = 1)` - the Bosse et al. 2023 convention).
#' @param unit_cols Columns that jointly identify one forecast (`scoringutils`'
#'   "forecast unit"). Defaults to `forecast_unit_cols`
#'   (iso3/origin_date/horizon/target_date/model) - correct whenever a call
#'   covers only one value of every other grouping dimension (e.g. one
#'   window_type's file). Widen it (e.g. `c(forecast_unit_cols,
#'   "window_type")`) whenever a single call stacks rows that would otherwise
#'   collide as "the same forecast" - Stage 1 scoring several windows
#'   together, or Stage 2 scoring champion + shadow candidates together.
#' @return A `forecast_quantile` object, unit `unit_cols`.
to_forecast_quantile <- function(df, actual_col = "actual", log_scale = FALSE,
                                 unit_cols = forecast_unit_cols) {
  long <- df %>%
    dplyr::rename(observed = dplyr::all_of(actual_col)) %>%
    tidyr::pivot_longer(
      cols = c(.pred_lower90, .pred_lower50, .pred, .pred_upper50, .pred_upper90),
      names_to = "which_q", values_to = "predicted"
    ) %>%
    dplyr::mutate(
      quantile_level = dplyr::case_when(
        which_q == ".pred_lower90" ~ interval_probs[["q05"]],
        which_q == ".pred_lower50" ~ interval_probs[["q25"]],
        which_q == ".pred"         ~ interval_probs[["q50"]],
        which_q == ".pred_upper50" ~ interval_probs[["q75"]],
        which_q == ".pred_upper90" ~ interval_probs[["q95"]]
      )
    ) %>%
    dplyr::select(-which_q)

  if (isTRUE(log_scale)) {
    long <- long %>%
      dplyr::mutate(
        observed  = scoringutils::log_shift(observed, offset = 1),
        predicted = scoringutils::log_shift(predicted, offset = 1)
      )
  }
  scoringutils::as_forecast_quantile(long, forecast_unit = unit_cols)
}

#' Reshape a long sample forecast (`sample_id` + `predicted`, one row per
#' draw) into a `scoringutils` `forecast_sample` object. For a future model
#' that exposes a posterior sample rather than fixed quantiles.
#'
#' @inheritParams to_forecast_quantile
#' @return A `forecast_sample` object, unit `unit_cols`.
to_forecast_sample <- function(df, actual_col = "actual", log_scale = FALSE,
                               unit_cols = forecast_unit_cols) {
  long <- df %>% dplyr::rename(observed = dplyr::all_of(actual_col))
  if (isTRUE(log_scale)) {
    long <- long %>%
      dplyr::mutate(
        observed  = scoringutils::log_shift(observed, offset = 1),
        predicted = scoringutils::log_shift(predicted, offset = 1)
      )
  }
  scoringutils::as_forecast_sample(long, forecast_unit = unit_cols)
}

#' Does `df` look like a quantile forecast (has `forecast_output_cols`'
#' predictive columns) or a sample forecast (`sample_id` + `predicted`)?
#' @param df A data frame.
#' @return `"quantile"`, `"sample"`, or an error if neither shape matches.
forecast_shape <- function(df) {
  quantile_cols <- c(".pred", ".pred_lower50", ".pred_upper50", ".pred_lower90", ".pred_upper90")
  if (all(quantile_cols %in% names(df))) return("quantile")
  if (all(c("sample_id", "predicted") %in% names(df))) return("sample")
  cli::cli_abort(c(
    "`df` isn't a recognised forecast shape.",
    "i" = "Needs either {.field {quantile_cols}} (quantile) or {.field sample_id}/{.field predicted} (sample)."
  ))
}

# ---- CRPS approximation for quantile forecasts -----------------------------

#' Quantile (pinball) loss: `(actual - q) * (tau - 1(actual < q))`.
#' @param actual,q Numeric vectors (recycled). @param tau Probability, scalar
#'   or vector. @return Numeric vector.
pinball_loss <- function(actual, q, tau) {
  (actual - q) * (tau - as.numeric(actual < q))
}

#' Quantile-based CRPS approximation for a quantile forecast (Gneiting &
#' Raftery 2007): `2 * mean_tau[ pinball_loss(y, q_tau, tau) ]` over
#' `interval_probs`'s 5 levels.
#'
#' With exactly this quantile set (median + both interval bounds), this is
#' ALGEBRAICALLY IDENTICAL to what `scoringutils::score()` calls `wis` at this
#' resolution - verified numerically (max diff ~1e-13), which is why
#' `score_forecast()` reports only `crps`, not both. Computed explicitly here
#' (rather than reusing that internal `wis` value) so the calculation stays
#' auditable. A denser future quantile set, or a sample-based model (scored
#' via `to_forecast_sample()` instead), would make the two genuinely diverge -
#' see the file overview.
#'
#' @param actual Numeric vector. @param q05,q25,q50,q75,q95 Numeric vectors,
#'   same length as `actual`. @return Numeric vector.
crps_approx <- function(actual, q05, q25, q50, q75, q95) {
  qs   <- list(q05 = q05, q25 = q25, q50 = q50, q75 = q75, q95 = q95)
  taus <- interval_probs[names(qs)]
  losses <- Map(function(q, tau) pinball_loss(actual, q, tau), qs, taus)
  2 * Reduce(`+`, losses) / length(losses)
}

# ---- row-level scoring wrapper ---------------------------------------------

#' Score a quantile-shaped forecast: `crps` is `crps_approx()` above (its own
#' total, computed explicitly rather than reused from `scoringutils::score()`'s
#' `wis` - the two are identical here, see the file overview); `dispersion`/
#' `overprediction`/`underprediction` (raw & log) are that same `score()`
#' call's WIS decomposition, which sums to `crps`.
#' @inheritParams to_forecast_quantile
#' @keywords internal
score_quantile_forecast <- function(df, actual_col, unit_cols = forecast_unit_cols) {
  need <- c(unit_cols, ".pred_lower90", ".pred_lower50", ".pred",
           ".pred_upper50", ".pred_upper90", actual_col)
  miss <- setdiff(need, names(df))
  if (length(miss) > 0) cli::cli_abort("`df` is missing column{?s}: {.field {miss}}.")

  score_one <- function(log_scale) {
    as.data.frame(scoringutils::score(to_forecast_quantile(df, actual_col, log_scale, unit_cols)))
  }
  raw  <- score_one(FALSE)
  logs <- score_one(TRUE) %>%
    dplyr::select(dplyr::all_of(unit_cols), dplyr::all_of(log_metric_cols)) %>%
    dplyr::rename_with(~ paste0(.x, "_log"), dplyr::all_of(log_metric_cols))

  actual <- df[[actual_col]]
  crps_cols <- df %>%
    dplyr::transmute(
      dplyr::across(dplyr::all_of(unit_cols)),
      crps = crps_approx(actual, .pred_lower90, .pred_lower50, .pred, .pred_upper50, .pred_upper90),
      crps_log = crps_approx(log1p(actual), log1p(.pred_lower90), log1p(.pred_lower50),
                             log1p(.pred), log1p(.pred_upper50), log1p(.pred_upper90))
    )

  dplyr::distinct(df, dplyr::across(dplyr::all_of(unit_cols))) %>%
    dplyr::left_join(dplyr::select(raw, -wis),  by = unit_cols) %>%
    dplyr::left_join(logs, by = unit_cols) %>%
    dplyr::left_join(crps_cols, by = unit_cols)
}

#' Score a sample-shaped forecast: `crps` is the real
#' `scoringRules::crps_sample()` value (via `scoringutils`); `dispersion`/
#' `overprediction`/`underprediction` (raw & log) are its sample-analogue
#' decomposition, which sums to `crps` exactly (verified numerically).
#' @inheritParams to_forecast_quantile
#' @keywords internal
score_sample_forecast <- function(df, actual_col, unit_cols = forecast_unit_cols) {
  need <- c(unit_cols, "sample_id", "predicted", actual_col)
  miss <- setdiff(need, names(df))
  if (length(miss) > 0) cli::cli_abort("`df` is missing column{?s}: {.field {miss}}.")

  score_one <- function(log_scale) {
    as.data.frame(scoringutils::score(to_forecast_sample(df, actual_col, log_scale, unit_cols)))
  }
  raw  <- score_one(FALSE) %>%
    dplyr::select(dplyr::all_of(unit_cols), bias, crps, dplyr::all_of(log_metric_cols))
  logs <- score_one(TRUE) %>%
    dplyr::select(dplyr::all_of(unit_cols), crps, dplyr::all_of(log_metric_cols)) %>%
    dplyr::rename_with(~ paste0(.x, "_log"), c(crps, dplyr::all_of(log_metric_cols)))

  # interval_coverage_50/90 is a quantile-native concept (does actual fall in
  # the 50%/90% band?); a sample forecast has no such band without first
  # choosing quantiles from the sample, so left NA - the one field this
  # shape's schema doesn't (yet) share with the quantile path's.
  dplyr::distinct(df, dplyr::across(dplyr::all_of(unit_cols))) %>%
    dplyr::left_join(raw,  by = unit_cols) %>%
    dplyr::left_join(logs, by = unit_cols) %>%
    dplyr::mutate(interval_coverage_50 = NA, interval_coverage_90 = NA)
}

#' Score one (or several, stacked) model's forecasts against observed truth -
#' one row per forecast unit, dispatching on shape (`forecast_shape()`): real
#' CRPS for a sample forecast, approximate (algebraically identical) CRPS for
#' a quantile forecast (today's only model type) - one `crps` column either
#' way, so quantile- and sample-based models compare and merge directly. No
#' separate `wis` column: it would just be the same number as `crps` under a
#' different name (see the file overview). Preserves a row even where
#' `scoringutils::score()` would drop a unit entirely (every prediction `NA`,
#' i.e. the model "genuinely cannot forecast" that row) - left `NA` across
#' every metric instead, so row count always matches `df`'s.
#'
#' @param df A data frame, quantile- or sample-shaped (see `forecast_shape()`),
#'   plus an actual-value column (`actual_col`) and a `model` column (or pass
#'   `model_name` to stamp one on, for a single model's output).
#' @param actual_col Name of the truth column in `df`.
#' @param model_name Stamps a `model` column onto `df` if it lacks one.
#' @param unit_cols Columns that jointly identify one forecast. Defaults to
#'   `forecast_unit_cols` (iso3/origin_date/horizon/target_date/model) -
#'   correct as long as `df` covers only one value of every other grouping
#'   dimension. Widen it (e.g. `c(forecast_unit_cols, "window_type")` for a
#'   Stage 1 call stacking several windows, or `c(forecast_unit_cols,
#'   "champion_flag")` for a future Stage 2 call stacking champion + shadow
#'   candidates) whenever rows would otherwise collide as "the same forecast".
#' @return `unit_cols` plus: `crps` (PRIMARY), its decomposition
#'   `dispersion`/`overprediction`/`underprediction`, `bias`, `ae_median` (raw
#'   scale; `interval_coverage_50/90` for quantile forecasts only, `NA` for
#'   sample) and the `_log` (log1p scale) equivalents of `crps`/decomposition/
#'   `ae_median`. `bias` and the coverage indicators are scale-invariant (does
#'   `actual` fall inside `[lower, upper]`?), so not duplicated as `_log`.
score_forecast <- function(df, actual_col = "actual", model_name = NULL,
                           unit_cols = forecast_unit_cols) {
  if (!"model" %in% names(df)) {
    if (is.null(model_name)) {
      cli::cli_abort("`df` has no {.field model} column - pass `model_name`.")
    }
    df$model <- model_name
  }
  switch(forecast_shape(df),
    quantile = score_quantile_forecast(df, actual_col, unit_cols),
    sample   = score_sample_forecast(df, actual_col, unit_cols)
  )
}

# ---- calibration diagnostics ------------------------------------------------

#' PIT histogram (`scoringutils::get_pit_histogram()`) for a quantile
#' forecast - §5 of the evaluation plan's calibration plots.
#' @inheritParams score_forecast
#' @param by Grouping column(s), one histogram per (default `"model"`).
#' @param ... Passed to `scoringutils::get_pit_histogram()` (e.g. `num_bins`).
#' @return A data frame: `bin`, `mid`, `density`, one row per bin per group.
pit_histogram <- function(df, actual_col = "actual", model_name = NULL,
                          by = "model", unit_cols = forecast_unit_cols, ...) {
  if (!"model" %in% names(df)) {
    if (is.null(model_name)) cli::cli_abort("`df` has no {.field model} column - pass `model_name`.")
    df$model <- model_name
  }
  scoringutils::get_pit_histogram(to_forecast_quantile(df, actual_col, unit_cols = unit_cols), by = by, ...)
}

#' Empirical coverage per quantile level and nominal interval
#' (`scoringutils::get_coverage()`) for a quantile forecast - the
#' finer-grained sibling of `score_forecast()`'s `interval_coverage_50/90`.
#' @inheritParams pit_histogram
#' @return `model` (or `by`), `quantile_level`, `interval_range`,
#'   `interval_coverage(_deviation)`, `quantile_coverage(_deviation)`.
coverage_diagnostics <- function(df, actual_col = "actual", model_name = NULL,
                                 by = "model", unit_cols = forecast_unit_cols) {
  if (!"model" %in% names(df)) {
    if (is.null(model_name)) cli::cli_abort("`df` has no {.field model} column - pass `model_name`.")
    df$model <- model_name
  }
  scoringutils::get_coverage(to_forecast_quantile(df, actual_col, unit_cols = unit_cols), by = by)
}

# ---- burden normalisation (GDO's u* convention) ----------------------------

#' Burden-normalise an error metric (GDO's uMAE/uRMSE convention). `burden`
#' should be `mean_actual_by_prediction_month()`'s output
#' (`00_FUN_validation_metrics.R`) - a country's mean historical cases for the
#' target month, joined in by the caller.
#' @param x A row-level or aggregated error (e.g. `ae_median`, `mean(ae_median)`).
#' @param burden Numeric, same length as `x` or length 1.
#' @return `x / burden`, `NA` where `burden` is 0 or missing.
normalise_by_burden <- function(x, burden) {
  dplyr::if_else(!is.na(burden) & burden > 0, x / burden, NA_real_)
}

# ---- trajectory-level scorers -----------------------------------------------

#' Peak-timing difference, in months (Campbell et al. 2026's D-MOSS metric;
#' GDO-specific, no package covers it). Positive = model peaks AFTER the
#' observed peak; negative = too early. Restricted to rows where both are
#' observed; `NA` if fewer than `min_months` such rows remain.
#' @param target_date Date vector, one per group member (a month, within one
#'   trajectory; an origin date, across rolling origins at fixed lead).
#' @param actual,predicted Numeric, same length as `target_date`.
#' @param min_months Minimum complete-pair members required.
#' @return A single integer (months), or `NA`.
peak_timing_diff <- function(target_date, actual, predicted, min_months = 3L) {
  ok <- !is.na(actual) & !is.na(predicted)
  if (sum(ok) < min_months) return(NA_integer_)
  d <- target_date[ok]; a <- actual[ok]; p <- predicted[ok]
  peak_actual    <- d[which.max(a)]
  peak_predicted <- d[which.max(p)]
  (lubridate::year(peak_predicted)  - lubridate::year(peak_actual))  * 12L +
    (lubridate::month(peak_predicted) - lubridate::month(peak_actual))
}

#' Dynamic time warping distance (Campbell et al. 2026's D-MOSS metric:
#' trajectory shape, tolerant of a timing offset). Thin wrapper on `dtw::dtw()`
#' (Campbell et al.'s own package). Two uses: one forecast's own trajectory vs
#' truth; or one lead time's series across rolling origins vs the observed
#' series over that span.
#' @param actual,predicted Numeric (need not be equal length; `NA`s dropped
#'   independently before alignment).
#' @param ... Passed to `dtw::dtw()` (e.g. `step.pattern`, to match a specific
#'   published configuration).
#' @return `dtw::dtw()$distance` (cumulative, unnormalised), or `NA` if either
#'   input has 0 non-`NA` values.
dtw_distance <- function(actual, predicted, ...) {
  x <- actual[!is.na(actual)]
  y <- predicted[!is.na(predicted)]
  if (length(x) < 1L || length(y) < 1L) return(NA_real_)
  dtw::dtw(x, y, ...)$distance
}

# ---- relative comparison ---------------------------------------------------

#' Pairwise relative skill (relative WIS convention, Cramer et al. 2022 - the
#' COVID-19 Forecast Hub's primary cross-model metric). Thin wrapper on
#' `scoringutils::get_pairwise_comparisons()`; scores fresh from the wide
#' input rather than reusing `score_forecast()`'s output, which strips the
#' metadata that function needs. `< 1` means the compared model beats
#' `baseline`. Needs >= 2 models in `df$model`.
#' @param df Quantile-shaped, stacked for >= 2 models (`dplyr::bind_rows()`).
#' @param baseline Model name in `df$model` to compare every other model against.
#' @param actual_col Name of the truth column in `df`.
#' @param by Extra grouping (e.g. `"horizon"`) - `NULL` pools everything.
#' @param log_scale Score on the log1p scale (default `TRUE`, the plan's
#'   primary metric).
#' @inheritParams score_forecast
#' @return See `?scoringutils::get_pairwise_comparisons`; `wis_scaled_relative_skill`
#'   is the column of interest (named `wis` regardless of `log_scale`).
relative_skill <- function(df, baseline, actual_col = "actual", by = NULL,
                           log_scale = TRUE, unit_cols = forecast_unit_cols) {
  if (!"model" %in% names(df)) {
    cli::cli_abort("`df` needs a {.field model} column with >= 2 models to compare.")
  }
  sc <- scoringutils::score(to_forecast_quantile(df, actual_col, log_scale = log_scale, unit_cols = unit_cols))
  scoringutils::get_pairwise_comparisons(sc, by = by, baseline = baseline, metric = "wis")
}
