#' ---
#' title: "01_scoring"
#' author: "K M Susong"
#' ---
#'
#' Overview:
#' ========
#' The GDO forecasting objective's metric library - not Stage-1-specific.
#' DEFINITION ONLY - pure functions, no file I/O, no side effects on source.
#' Every scorer works directly off a model's `forecast_output_cols` (iso3,
#' origin_date, horizon, target_date, .pred, .pred_lower50, .pred_upper50,
#' .pred_lower90, .pred_upper90) joined to observed truth, so the Stage 1
#' hindcast runner, Stage 2's shadow-deployment logging (scored once outcomes
#' are known), and any ad hoc notebook use all call the same code.
#'
#' Quantile levels are NOT hard-coded here: the wide -> long adapter maps
#' `forecast_output_cols` onto whatever `00_config.R`'s `interval_probs` says,
#' the same single source of truth the models (models/00b_baseline_nowcast.R,
#' models/utils/glm_factory.R) already use. Currently 5 levels (50%/90%
#' central intervals + median) - kept as-is for now; likely to widen once
#' GAM/hierarchical models are compared and a denser posterior is available to
#' score against (see the CRPS note below).
#'
#' Built on `scoringutils` (Bosse, Gruson, Cori, van Leeuwen, Funk & Abbott
#' 2022; Sebastian Funk's epiforecasts.io group, a named GDO collaborator) and
#' `dtw`, rather than hand-rolled WIS/CRPS/PIT/DTW - the field-standard tools,
#' now added to renv.lock. Metrics, per the plan
#' (planning/2026_forecast_evaluation/03_Evaluation_Plan_DRAFT.md §3) and the
#' 2026-09-08 team meeting:
#'   - PRIMARY:   WIS (log scale primary, raw scale kept alongside), with its
#'                dispersion / overprediction / underprediction decomposition
#'                (Bracher, Ray, Gneiting & Reich 2021) - `score_forecast()`.
#'   - SECONDARY: PIT (`pit_histogram()`) and interval/quantile coverage
#'                (`coverage_diagnostics()`, plus `interval_coverage_50/90` in
#'                every `score_forecast()` row); `ae_median`/`ae_median_log`
#'                (median point-accuracy, raw and log scale - GDO's MAE
#'                building block; aggregate and burden-normalise with
#'                `normalise_by_burden()` for uMAE/uRMSE).
#'                [NOTE] No CRPS column: `scoringutils` only computes CRPS for
#'                SAMPLE-based forecasts, not quantile-based ones - with just 5
#'                quantile levels there is no way to get an independent CRPS
#'                anyway (a hand-checked identity: summing pinball loss at
#'                exactly the 5 levels WIS's intervals use reproduces WIS's own
#'                numerator exactly, to ~1e-13). WIS already is the CRPS
#'                approximation here; a real CRPS needs either a denser
#'                quantile set or a model that can generate posterior samples
#'                (the hierarchical/INLA model later in the roster is the
#'                obvious candidate - scoringutils' `crps_sample()` handles
#'                that format directly, no new code needed here when it lands).
#'   - OPERATIONAL (trajectory-level, one row per group): peak-timing
#'                difference (`peak_timing_diff()`, GDO-specific, no package
#'                covers it) and DTW distance (`dtw_distance()`, wrapping
#'                `dtw::dtw()` - Campbell et al. 2026's D-MOSS metric). Two
#'                distinct uses, both just a call on two numeric vectors:
#'                (a) one forecast's own 6-month trajectory vs what actually
#'                happened (group by iso3 x origin_date, vector = horizon 1-6);
#'                (b) Campbell's own axis - one lead time's forecast series
#'                across many rolling origins vs the observed series over that
#'                same calendar span (group by iso3 x horizon, vector = origin
#'                date) - only meaningful once Stage 1 has multiple origins
#'                per window, i.e. once the hindcast runner exists.
#'   - RELATIVE:  `relative_skill()` wraps `scoringutils::get_pairwise_comparisons()`
#'                - the pairwise geometric-mean relative-WIS convention (Cramer
#'                et al. 2022) - for "vs nowcast" / "vs GLM AR" / "vs prior best
#'                model". Takes a full multi-model `score_forecast()`-shaped
#'                table (needs >= 2 models present to compare); building that
#'                table is the caller's job once real multi-model Stage 1
#'                output exists.
#'
#' Timeline:
#' ========
#' 11-09-2026: Created (hand-rolled WIS/CRPS/PIT, no dtw dependency).
#' 11-09-2026: Rewritten on `scoringutils` + `dtw` (agreed additions to
#'   renv.lock) rather than reimplementing the same maths by hand.

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

# ---- wide (forecast_output_cols) -> long (scoringutils quantile format) ---

#' Reshape one model's wide forecast rows into a `scoringutils` quantile
#' forecast object.
#'
#' @param df A data frame with `forecast_output_cols` + a `model` column +
#'   an actual-value column (`actual_col`).
#' @param actual_col Name of the truth column in `df`.
#' @param log_scale If `TRUE`, both `predicted` and `observed` are
#'   `scoringutils::log_shift(offset = 1)`-transformed (= `log1p()`) before
#'   building the forecast object - the Bosse et al. 2023 log-scale scoring
#'   convention, built into the package itself.
#' @return A `scoringutils` `forecast_quantile` object, forecast unit
#'   `iso3, origin_date, horizon, target_date, model`.
to_forecast_quantile <- function(df, actual_col = "actual", log_scale = FALSE) {
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
  
  scoringutils::as_forecast_quantile(
    long,
    forecast_unit = c("iso3", "origin_date", "horizon", "target_date", "model")
  )
}

# ---- row-level scoring wrapper ---------------------------------------------

#' Score one (or several, already stacked) model's forecasts against observed
#' truth - one row per (iso3, origin_date, horizon, target_date, model), even
#' where `scoringutils::score()` would otherwise drop a unit entirely (every
#' quantile `NA`, i.e. the model "genuinely cannot forecast" that row): those
#' rows are kept and left `NA` across every metric, so a row count here always
#' matches the row count of the input `df`, not a silently-shrunk subset.
#'
#' @param df A data frame with the model contract's `forecast_output_cols`
#'   (`iso3`, `origin_date`, `horizon`, `target_date`, `.pred`,
#'   `.pred_lower50`, `.pred_upper50`, `.pred_lower90`, `.pred_upper90`) plus
#'   an actual-value column (`actual_col`, default `"actual"`) already joined
#'   in by the caller. A `model` column identifying which model produced each
#'   row, if scoring several models' output stacked together (use
#'   `model_name` instead for a single model's output with no such column).
#' @param actual_col Name of the truth column in `df`.
#' @param model_name Used to stamp a `model` column onto `df` if it doesn't
#'   already have one; required in that case.
#' @return `df`'s `iso3, origin_date, horizon, target_date, model` plus: `wis`,
#'   `dispersion`, `overprediction`, `underprediction`, `bias`, `ae_median`,
#'   `interval_coverage_50`, `interval_coverage_90` (raw scale); `wis_log`,
#'   `dispersion_log`, `overprediction_log`, `underprediction_log`,
#'   `ae_median_log` (log1p scale, PRIMARY per the plan). `bias` and the
#'   coverage indicators are scale-invariant under a monotone transform (does
#'   `actual` fall inside `[lower, upper]`?), so they aren't duplicated with a
#'   `_log` suffix.
score_forecast <- function(df, actual_col = "actual", model_name = NULL) {
  need <- c("iso3", "origin_date", "horizon", "target_date",
            ".pred", ".pred_lower50", ".pred_upper50",
            ".pred_lower90", ".pred_upper90", actual_col)
  miss <- setdiff(need, names(df))
  if (length(miss) > 0) {
    cli::cli_abort("`df` is missing column{?s}: {.field {miss}}.")
  }
  if (!"model" %in% names(df)) {
    if (is.null(model_name)) {
      cli::cli_abort("`df` has no {.field model} column - pass `model_name`.")
    }
    df$model <- model_name
  }
  
  log_cols <- c("wis", "dispersion", "overprediction", "underprediction", "ae_median")
  
  score_one <- function(log_scale) {
    as.data.frame(scoringutils::score(to_forecast_quantile(df, actual_col, log_scale)))
  }
  raw <- score_one(FALSE)
  logs <- score_one(TRUE) %>%
    dplyr::select(iso3, origin_date, horizon, target_date, model,
                  dplyr::all_of(log_cols)) %>%
    dplyr::rename_with(~ paste0(.x, "_log"), dplyr::all_of(log_cols))
  
  units <- dplyr::distinct(df, iso3, origin_date, horizon, target_date, model)
  
  units %>%
    dplyr::left_join(raw,  by = c("iso3", "origin_date", "horizon", "target_date", "model")) %>%
    dplyr::left_join(logs, by = c("iso3", "origin_date", "horizon", "target_date", "model"))
}

# ---- calibration diagnostics ------------------------------------------------

#' PIT histogram (`scoringutils::get_pit_histogram()`), for the calibration
#' plots in §5 of the evaluation plan.
#'
#' @inheritParams score_forecast
#' @param by Grouping column(s) to build a separate histogram per (default
#'   `"model"`).
#' @param ... Passed on to `scoringutils::get_pit_histogram()` (e.g.
#'   `num_bins`).
#' @return A data frame: `bin`, `mid`, `density`, one row per bin per group.
pit_histogram <- function(df, actual_col = "actual", model_name = NULL,
                          by = "model", ...) {
  if (!"model" %in% names(df)) {
    if (is.null(model_name)) {
      cli::cli_abort("`df` has no {.field model} column - pass `model_name`.")
    }
    df$model <- model_name
  }
  scoringutils::get_pit_histogram(to_forecast_quantile(df, actual_col), by = by, ...)
}

#' Empirical coverage per quantile level and nominal interval
#' (`scoringutils::get_coverage()`) - the finer-grained sibling of
#' `score_forecast()`'s `interval_coverage_50/90` columns, for a full
#' coverage-vs-nominal calibration plot.
#'
#' @inheritParams pit_histogram
#' @return A data frame: `model` (or whatever `by` names), `quantile_level`,
#'   `interval_range`, `interval_coverage`, `interval_coverage_deviation`,
#'   `quantile_coverage`, `quantile_coverage_deviation`.
coverage_diagnostics <- function(df, actual_col = "actual", model_name = NULL,
                                 by = "model") {
  if (!"model" %in% names(df)) {
    if (is.null(model_name)) {
      cli::cli_abort("`df` has no {.field model} column - pass `model_name`.")
    }
    df$model <- model_name
  }
  scoringutils::get_coverage(to_forecast_quantile(df, actual_col), by = by)
}

# ---- burden normalisation (GDO's u* convention) ----------------------------

#' Burden-normalise an error metric (GDO's u* convention: uMAE, uRMSE, ...).
#'
#' `burden` should be the same "mean_actual_predicted_month" quantity used
#' throughout `Scripts/validation/` (`mean_actual_by_prediction_month()` in
#' `00_FUN_validation_metrics.R`) - typically a country's mean historical
#' cases for the target calendar/season month, computed once by the caller and
#' joined in, not recomputed here.
#'
#' @param x A row-level or already-aggregated error (e.g. `ae_median`, or
#'   `mean(ae_median)`).
#' @param burden Numeric vector/scalar, same length as `x` or length 1.
#' @return `x / burden`, `NA` where `burden` is 0 or missing.
normalise_by_burden <- function(x, burden) {
  dplyr::if_else(!is.na(burden) & burden > 0, x / burden, NA_real_)
}

# ---- trajectory-level scorers (one group - e.g. iso3 x origin_date - at a time) --

#' Peak-timing difference, in months, between an observed and a predicted
#' trajectory (Campbell et al. 2026's D-MOSS metric). No package covers this -
#' GDO/D-MOSS-specific, not a general forecast-scoring quantity.
#'
#' Positive = the model's peak lands AFTER the observed peak; negative = the
#' model peaks too early. Restricted to rows where both `actual` and
#' `predicted` are observed, so the two peaks are found over the same window;
#' returns `NA` if fewer than `min_months` such rows remain (a peak read off
#' 1-2 months is not a meaningful comparison).
#'
#' @param target_date Date vector, one per group member (a month, for a
#'   within-trajectory call; an origin date, for a fixed-lead call across
#'   rolling origins - see the file overview).
#' @param actual,predicted Numeric vectors, same length as `target_date`.
#' @param min_months Minimum complete (actual & predicted both present)
#'   members required to compute a peak at all.
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
#' tolerant of a timing offset the pointwise metrics above penalise). Thin
#' wrapper on `dtw::dtw()` (the same package Campbell et al. used).
#'
#' Two distinct, equally valid uses - see the file overview for which grouping
#' each implies: (a) one forecast's own trajectory across horizons vs what
#' actually happened; (b) Campbell's own axis, one lead time's forecast series
#' across rolling origins vs the observed series over that span.
#'
#' @param actual,predicted Numeric vectors (need not be the same length; `NA`s
#'   are dropped from each independently before alignment).
#' @param ... Passed on to `dtw::dtw()` (e.g. `step.pattern`, `window.type`, to
#'   match a specific published configuration exactly).
#' @return A single numeric distance (`dtw::dtw()`'s cumulative, unnormalised
#'   `$distance`), or `NA` if either input has 0 non-`NA` values.
dtw_distance <- function(actual, predicted, ...) {
  x <- actual[!is.na(actual)]
  y <- predicted[!is.na(predicted)]
  if (length(x) < 1L || length(y) < 1L) return(NA_real_)
  dtw::dtw(x, y, ...)$distance
}

# ---- relative comparison ---------------------------------------------------

#' Pairwise relative skill (relative WIS convention: Cramer et al. 2022, PNAS
#' - the COVID-19 Forecast Hub's primary cross-model metric). Thin wrapper on
#' `scoringutils::get_pairwise_comparisons()`.
#'
#' Takes the WIDE, stacked multi-model input (same shape `score_forecast()`
#' takes) rather than `score_forecast()`'s own flattened output:
#' `get_pairwise_comparisons()` needs the metadata `scoringutils::score()`
#' attaches to its result, which plain data-frame reshaping (what
#' `score_forecast()` does to produce an easy-to-join table) strips - so this
#' scores fresh, once, and keeps that object intact.
#'
#' `< 1` means the compared model beats `baseline` (lower score = better).
#' Needs >= 2 models present (in `df$model`) to compare at all.
#'
#' @param df A data frame with `forecast_output_cols` + an actual-value column
#'   + a `model` column, for >= 2 models stacked (`dplyr::bind_rows()`).
#' @param baseline Model name in `df$model` to compare every other model
#'   against.
#' @param actual_col Name of the truth column in `df`.
#' @param by Extra grouping column(s) to compute relative skill within (e.g.
#'   `"horizon"`) - `NULL` (default) pools everything into one comparison.
#' @param log_scale Score on the log1p scale before comparing (default `TRUE`,
#'   the plan's primary metric) or the raw scale (`FALSE`).
#' @return A data frame - see `?scoringutils::get_pairwise_comparisons`;
#'   `wis_scaled_relative_skill` is the column of interest (still named `wis`
#'   regardless of `log_scale` - it is whichever scale was scored).
relative_skill <- function(df, baseline, actual_col = "actual", by = NULL,
                           log_scale = TRUE) {
  if (!"model" %in% names(df)) {
    cli::cli_abort("`df` needs a {.field model} column with >= 2 models to compare.")
  }
  sc <- scoringutils::score(to_forecast_quantile(df, actual_col, log_scale = log_scale))
  scoringutils::get_pairwise_comparisons(sc, by = by, baseline = baseline, metric = "wis")
}
