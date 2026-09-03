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
#'         .pred, .pred_lower50, .pred_upper50, .pred_lower95, .pred_upper95
#'       One row per input target row. .pred is the point forecast; the four
#'       .pred_* columns are the 50% and 95% central predictive intervals
#'       (matching GDO's calibrated-interval files and its 95% whiskers).
#'       NA is allowed where the model genuinely cannot forecast.
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

# The exact, ordered output columns every model$predict() must return.
forecast_output_cols <- c(
  "iso3", "origin_date", "horizon", "target_date",
  ".pred", ".pred_lower50", ".pred_upper50", ".pred_lower95", ".pred_upper95"
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

# save_forecast_model() is added here alongside run_stage0_fit.R.
