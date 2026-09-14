#' ---
#' title: "forecast_helpers"
#' author: "K M Susong"
#' ---
#'
#' Overview:
#' ========
#' Shared, contract-defining code for the forecasting models and the stage
#' runners. Kept deliberately small - only what is genuinely shared or what
#' pins the interface every model must honour.
#'
#' THE MODEL CONTRACT
#' =================
#' Every Scripts/forecasting/models/<slug>.R file defines exactly ONE model
#' object, built with new_forecast_model(). A model bundles three functions
#' with fixed signatures:
#'
#'   fit(train_df, spec)              -> fitted object
#'       Any shape. Opaque to callers - only the model's own predict()/diagnose()
#'       look inside it.
#'
#'   predict(fitted, targets, spec)   -> tibble with EXACTLY these columns,
#'       in this order (see `forecast_output_cols`):
#'         iso3, origin_date, horizon, target_date,
#'         .pred, .pred_lower50, .pred_upper50, .pred_lower90, .pred_upper90
#'       One row per input target row. .pred is the point forecast (predictive
#'       median); the four .pred_* columns are the 50% and 90% central
#'       predictive intervals - i.e. the 0.05 / 0.25 / 0.75 / 0.95 predictive
#'       quantiles. NA is allowed where the model genuinely cannot forecast.
#'
#'   diagnose(fitted, train_df, spec) -> named list
#'       Stage 0 checklist. Must contain a logical `pass`. Everything else is
#'       model-specific and shown as-is in the Stage 0 report.
#'
#' Inputs the runners pass in:
#'   train_df : long panel, one row per country-month, ordered/contiguous within
#'              a country. Columns include iso3, date, cases (gap months present
#'              as cases = NA rows), plus the feature columns from
#'              01_prepare_training_data.R.
#'   targets  : tibble(iso3, origin_date, horizon, target_date) - the rows to
#'              forecast. horizon is months ahead of origin_date; target_date is
#'              origin_date + horizon months.
#'   spec     : the model's own settings list; a runner may override fields
#'              (e.g. spec$max_horizon) before calling fit().
#'
#' Runners only ever touch model$name, model$spec and model$fit / $predict /
#' $diagnose, plus the columns above - never a model file's internal helpers.
#'
#' Timeline:
#' ========
#' 03-09-2026: Created alongside models/00a_baseline_naive.R.
#' 04-09-2026: Reviewed.
#' 10-09-2026: Interval schema 50%/95% -> 50%/90% for Stage 1 (.pred_lower90 /
#'   .pred_upper90). Quantile set is now 0.05 / 0.25 / 0.5 / 0.75 / 0.95.

# The exact, ordered output columns every model$predict() must return.
# The four .pred_* columns are the 50% and 90% central predictive intervals
# (0.05 / 0.25 / 0.75 / 0.95 predictive quantiles); .pred is the median.
forecast_output_cols <- c(
  "iso3", "origin_date", "horizon", "target_date",
  ".pred", ".pred_lower50", ".pred_upper50", ".pred_lower90", ".pred_upper90"
)

#' Build (and validate) a model object.
#'
#' Called once at the end of every models/<slug>.R file. Fails loudly at
#' source() time if a piece is missing or mis-wired, so a broken model never
#' reaches a backtest.
#'
#' @param name Short slug, also the value written in the output `model` column.
#' @param spec Named list of the model's settings.
#' @param fit,predict,diagnose The three interface functions.
#' @return A list of class "forecast_model".
new_forecast_model <- function(name, spec, fit, predict, diagnose) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    cli::cli_abort("`name` must be a single non-empty string.")
  }
  if (!is.list(spec)) {
    cli::cli_abort("Model {.val {name}}: `spec` must be a list.")
  }

  # Each interface function must exist and take the expected first argument -
  # a cheap guard against passing the pieces in the wrong order.
  expect_first_arg <- function(f, arg, what) {
    if (!is.function(f)) {
      cli::cli_abort("Model {.val {name}}: {what} is not a function.")
    }
    got <- names(formals(f))[1]
    if (!identical(got, arg)) {
      cli::cli_abort(
        "Model {.val {name}}: {what} first argument should be {.arg {arg}}, got {.arg {got %||% 'none'}}."
      )
    }
  }
  expect_first_arg(fit, "train_df", "fit()")
  expect_first_arg(predict, "fitted", "predict()")
  expect_first_arg(diagnose, "fitted", "diagnose()")

  structure(
    list(name = name, spec = spec, fit = fit, predict = predict, diagnose = diagnose),
    class = "forecast_model"
  )
}

#' Assert a predict() result matches the contract, and return it column-ordered.
#'
#' Runners call this on every model$predict() result so a model that returns the
#' wrong shape is caught immediately and in one place.
#'
#' @param df A model$predict() result.
#' @param model_name For the error message.
#' @return `df` with exactly `forecast_output_cols`, in order.
check_forecast_output <- function(df, model_name = "?") {
  if (!is.data.frame(df)) {
    cli::cli_abort("Model {.val {model_name}} predict() must return a data frame.")
  }
  missing_cols <- setdiff(forecast_output_cols, names(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Model {.val {model_name}} predict() is missing column{?s}: {.field {missing_cols}}."
    )
  }
  extra_cols <- setdiff(names(df), forecast_output_cols)
  if (length(extra_cols) > 0) {
    cli::cli_warn(
      "Model {.val {model_name}} predict() returned extra column{?s} (dropped): {.field {extra_cols}}."
    )
  }
  df[forecast_output_cols]
}

#' Slice a training panel to one rolling-origin cell's countries and window.
#'
#' `window` carries a per-country `train_start` (window length is clamped to
#' each country's own first observed month - see `03_check_window_coverage.R`),
#' so this is a join-then-filter, not one shared date range for every country.
#'
#' @param panel The full training panel.
#' @param window `tibble(iso3, train_start, train_end)` - one row per country
#'   for this (window_type, origin_date) cell, e.g. from
#'   `Output/forecasting/cv_splits/rolling_origin_country.csv`.
#' @return `panel` rows for those countries, within each one's own window.
slice_train <- function(panel, window) {
  panel %>%
    dplyr::inner_join(
      dplyr::select(window, iso3, train_start, train_end), by = "iso3"
    ) %>%
    dplyr::filter(date >= train_start, date <= train_end) %>%
    dplyr::select(-train_start, -train_end)
}

#' Build target rows (one per country x horizon) for one forecast origin.
#'
#' @param iso3s Countries to forecast for.
#' @param origin_date The origin (a single Date).
#' @param horizons Integer vector, months ahead of `origin_date`.
#' @return `tibble(iso3, origin_date, horizon, target_date)`.
build_targets <- function(iso3s, origin_date, horizons) {
  tidyr::crossing(iso3 = iso3s, horizon = horizons) %>%
    dplyr::mutate(
      origin_date = origin_date,
      target_date = origin_date %m+% months(horizon)
    ) %>%
    dplyr::select(iso3, origin_date, horizon, target_date)
}

#' Join observed truth onto forecast rows, by (iso3, target_date).
#'
#' Uses the FULL panel (not a training-window slice) - a target_date sits
#' beyond the training window by construction, and may sit beyond the whole
#' panel's coverage too (a lead into the genuine future), in which case the
#' join leaves `actual` `NA` - correctly: no truth exists yet.
#'
#' @param forecasts Contract-shaped forecast rows (`iso3`, `target_date`, ...).
#' @param panel The full training panel (`iso3`, `date`, `cases`).
#' @return `forecasts` with an `actual` column added.
attach_truth <- function(forecasts, panel) {
  truth <- dplyr::transmute(panel, iso3, date, actual = cases)
  dplyr::left_join(forecasts, truth, by = c("iso3", "target_date" = "date"))
}

#' Merge new rows into an existing CSV, write once. Existing rows sharing a
#' `key_cols` value with `new_rows` are dropped first, so re-running a subset
#' of cells replaces just those cells - the same accumulate-and-replace idiom
#' `run_stage0_fit.R` uses for `diagnostics.csv`, generalised for reuse.
#'
#' @param new_rows Rows to add or replace.
#' @param path Destination CSV path (parent dirs created if absent).
#' @param key_cols Column name(s) identifying a row uniquely.
#' @return `path`, invisibly.
write_forecast_csv <- function(new_rows, path, key_cols) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path)) {
    prior <- readr::read_csv(path, show_col_types = FALSE)
    # readr infers each column's type per-run from its own values, which can
    # drift between reads (e.g. an all-NA column reads back as logical, not
    # character) - coerce shared columns to new_rows' types before binding.
    for (col in intersect(names(prior), names(new_rows))) {
      caster <- switch(class(new_rows[[col]])[1],
        character = as.character, logical = as.logical,
        integer = as.integer, numeric = as.numeric, Date = as.Date,
        as.character
      )
      prior[[col]] <- caster(prior[[col]])
    }
    prior <- dplyr::anti_join(
      prior, dplyr::distinct(new_rows, dplyr::across(dplyr::all_of(key_cols))),
      by = key_cols
    )
    new_rows <- dplyr::bind_rows(prior, new_rows)
  }
  readr::write_csv(new_rows, path)
  invisible(path)
}

#' Build the `list(fitted, meta)` record `save_forecast_model()` writes to
#' disk - pure, no I/O, so a caller can bundle several before saving any of
#' them (see `write_fit_bundle()`). `meta` is merged over defaults (model name
#' pulled from the fit when present, a timestamp, and the R version).
#'
#' @param fitted A model$fit() result.
#' @param meta Named list of extra provenance to record (e.g. stage, snapshot,
#'   window_type, origin_date).
#' @return `list(fitted, meta)`.
forecast_model_record <- function(fitted, meta = list()) {
  default_meta <- list(
    model     = tryCatch(fitted$name, error = function(e) NA_character_),
    created   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    r_version = as.character(getRversion())
  )
  list(fitted = fitted, meta = utils::modifyList(default_meta, meta))
}

#' Save a fitted model object with provenance metadata - one fit, one file.
#'
#' @inheritParams forecast_model_record
#' @param path Destination .rds path (parent dirs created if absent).
#' @return `path`, invisibly.
save_forecast_model <- function(fitted, path, meta = list()) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(forecast_model_record(fitted, meta), path)
  invisible(path)
}

#' Merge new fit records into an existing bundle .rds and save once - several
#' fits, one file. Companion to `write_forecast_csv()` for `.rds` output: a
#' bundle keyed by origin (e.g. every rolling origin for one model x window
#' type) is far fewer files than one-per-origin, which matters in this repo
#' (Dropbox-synced - many small files sync worse than a few larger ones), at
#' the cost of rewriting the whole bundle on every save - same tradeoff
#' `write_forecast_csv()` already makes for the forecast CSVs.
#'
#' @param new_fits Named list, one element per origin (name = a unique key,
#'   e.g. `origin_date` as `"YYYY-MM-DD"`), each element a
#'   `forecast_model_record()` result. Names already present in the existing
#'   bundle are replaced; others are added; everything else in the bundle is
#'   left untouched.
#' @param path Destination bundle path (parent dirs created if absent).
#' @return `path`, invisibly.
write_fit_bundle <- function(new_fits, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  bundle <- if (file.exists(path)) readRDS(path) else list()
  bundle[names(new_fits)] <- new_fits
  saveRDS(bundle[sort(names(bundle))], path)
  invisible(path)
}
