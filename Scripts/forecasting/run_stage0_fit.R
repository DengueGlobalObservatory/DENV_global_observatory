#!/usr/bin/env Rscript

#' ---
#' title: "run_stage0_fit"
#' author: "K M Susong"
#' ---
#'
#' Overview:
#' ========
#' Stage 0 runner. For every forecast model:
#'   1. fit() once on the full pinned training panel (the post-nowcast "ground
#'      truth" of the evaluation plan, section 2.1) - no window slicing, no
#'      rolling origins
#'   2. diagnose() - the model's own sanity checklist
#'   3. a predict() contract check on the full panel (wiring, not skill): does
#'      predict() run, return the contract columns, and give monotone quantiles
#'   4. save the fitted object with provenance
#' Diagnostic only. No skill is claimed here.
#'
#' Model discovery: every Scripts/forecasting/models/NN_*.R file is sourced in
#' isolation and its single forecast_model object collected. Pass model slugs as
#' arguments to run a subset:
#'   Rscript Scripts/forecasting/run_stage0_fit.R nowcast
#'
#' Input : Output/forecasting/training_data/training_panel_<snapshot_date>.csv
#' Output: Output/forecasting/stage0_fit/<slug>/fit.rds
#'         Output/forecasting/stage0_fit/<slug>/fit_warnings.txt              (if any)
#'         Output/forecasting/stage0_fit/<slug>/wiring_check_forecasts.csv    (the
#'         predict() contract-check output itself - one origin, no truth, not a
#'         skill claim; read by the notebook's model-details appendix)
#'         Output/forecasting/stage0_fit/diagnostics.csv           (accumulates
#'         across partial runs - a re-run of a model replaces just its row,
#'         other models' rows are kept; each row carries its own `run_at`
#'         timestamp so a stale row is visible)
#'
#' Timeline:
#' ========
#' 03-09-2026: Created.
#' 04-09-2026: Reviewed. Persist the wiring-check forecasts for the notebook.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(tibble)
  library(lubridate)
  library(cli)
})

# ---- Locate project root ---------------------------------------
if (!dir.exists("Output") && dir.exists("../../Output")) setwd("../..")
if (!dir.exists("Output")) {
  cli::cli_abort("Run from the DENV_global_observatory repo root (cannot find {.path Output/}).")
}

source("Scripts/forecasting/00_config.R")
source("Scripts/forecasting/models/utils/forecast_helpers.R")

model_dir  <- "Scripts/forecasting/models"
stage0_dir <- file.path(forecast_out, "stage0_fit")
dir.create(stage0_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Load model definitions -----------------------------------

#' Source one models/NN_*.R file in isolation and pull out its model object.
#'
#' @param path Path to a model definition file.
#' @return The file's single `forecast_model` object.
load_model_file <- function(path) {
  env <- new.env(parent = globalenv())
  sys.source(path, envir = env)
  hits <- Filter(
    function(x) inherits(x, "forecast_model"),
    mget(ls(env), envir = env)
  )
  if (length(hits) != 1L) {
    cli::cli_abort(
      "{.path {path}} must define exactly one forecast_model object (found {length(hits)})."
    )
  }
  hits[[1]]
}

model_files <- sort(list.files(model_dir, pattern = "^[0-9].*\\.R$", full.names = TRUE))
if (length(model_files) == 0L) {
  cli::cli_abort("No model files matching {.code ^[0-9].*\\.R$} in {.path {model_dir}}.")
}

models <- lapply(model_files, load_model_file)
names(models) <- vapply(models, `[[`, character(1), "name")

wanted <- commandArgs(trailingOnly = TRUE)
if (length(wanted) > 0L) {
  unknown <- setdiff(wanted, names(models))
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "Unknown model slug{?s}: {.val {unknown}}.",
      "i" = "Available: {.val {names(models)}}"
    ))
  }
  models <- models[wanted]
}
cli::cli_inform("Stage 0 for: {.val {names(models)}}")

# ---- Training panel -----------------------------------------
panel_path <- file.path(forecast_out, "training_data",
                        paste0("training_panel_", snapshot_date, ".csv"))
if (!file.exists(panel_path)) {
  cli::cli_abort(c(
    "Training panel not found: {.path {panel_path}}.",
    "i" = "Run {.path Scripts/forecasting/01_prepare_training_data.R} first."
  ))
}
panel <- read_csv(panel_path, show_col_types = FALSE) %>%
  dplyr::mutate(date = as.Date(date))

# ---- Stage 0 per model -------------------------------------

#' Build the target rows for the predict() wiring check.
#'
#' Every country, every configured horizon, all from a single origin (the
#' panel's last month). No truth is needed - this only exercises `predict()`,
#' it makes no skill claim.
#'
#' @param panel The training panel.
#' @return `tibble(iso3, horizon, origin_date, target_date)`.
stage0_targets <- function(panel) {
  origin <- max(panel$date)
  panel %>%
    dplyr::distinct(iso3) %>%
    tidyr::crossing(horizon = forecast_horizons) %>%
    dplyr::mutate(
      origin_date = origin,
      target_date = origin %m+% months(horizon)
    )
}

#' Does this model's predict() honour the contract, on the full panel.
#'
#' Calls `model$predict()` on `stage0_targets(panel)`, runs the result through
#' `check_forecast_output()` (exact contract columns), and checks the four
#' `.pred_*` columns are monotone around `.pred`. Any error (missing columns,
#' predict() itself failing) is caught and reported rather than raised, so one
#' broken model doesn't stop Stage 0 for the others.
#'
#' @param model A `forecast_model`.
#' @param fitted That model's `fit()` result.
#' @param panel The training panel (only used to build the targets).
#' @return A list: `summary` (one-row tibble - `predict_ok`, `n_predictions`,
#'   `pct_pred_na`, `intervals_monotone`, `predict_error`) and `forecasts`
#'   (the full predict() result, contract columns only, or NULL on error).
predict_contract_check <- function(model, fitted, panel) {
  tryCatch({
    fc <- check_forecast_output(
      model$predict(fitted, stage0_targets(panel), model$spec),
      model$name
    )
    monotone <- with(fc, all(
      .pred_lower90 <= .pred_lower50 + 1e-6 &
        .pred_lower50 <= .pred + 1e-6 &
        .pred <= .pred_upper50 + 1e-6 &
        .pred_upper50 <= .pred_upper90 + 1e-6,
      na.rm = TRUE
    ))
    list(
      summary = tibble::tibble(
        predict_ok         = TRUE,
        n_predictions      = nrow(fc),
        pct_pred_na        = round(mean(is.na(fc$.pred)), 3),
        intervals_monotone = monotone,
        predict_error      = NA_character_
      ),
      forecasts = fc
    )
  }, error = function(e) {
    list(
      summary = tibble::tibble(
        predict_ok         = FALSE,
        n_predictions      = NA_integer_,
        pct_pred_na        = NA_real_,
        intervals_monotone = NA,
        predict_error      = conditionMessage(e)
      ),
      forecasts = NULL
    )
  })
}

#' Run Stage 0 (fit + diagnose + predict check + save) for one model.
#'
#' @param model A `forecast_model`.
#' @param panel The full training panel.
#' @return One-row tibble: the model's own `diagnose()` scalars, the
#'   `predict_contract_check()` columns, `n_fit_warnings`, `run_at`, and
#'   `fit_path` - this becomes one row of `diagnostics.csv`.
run_stage0_one <- function(model, panel) {
  cli::cli_h3(model$name)
  warns <- character(0)

  fitted <- withCallingHandlers(
    model$fit(panel, model$spec),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  diag <- model$diagnose(fitted, panel, model$spec)
  pchk <- predict_contract_check(model, fitted, panel)

  out_dir  <- file.path(stage0_dir, model$name)
  fit_path <- file.path(out_dir, "fit.rds")
  save_forecast_model(
    fitted, fit_path,
    meta = list(stage = "stage0", snapshot = snapshot_date)
  )
  if (length(warns) > 0L) {
    writeLines(warns, file.path(out_dir, "fit_warnings.txt"))
  }
  if (!is.null(pchk$forecasts)) {
    # The wiring-check output itself, kept for the notebook's model-details
    # appendix - a Stage 0 snapshot (one origin, no truth), not a skill claim.
    write_csv(pchk$forecasts, file.path(out_dir, "wiring_check_forecasts.csv"))
  }

  cli::cli_inform(c(
    ">" = "fit: {length(warns)} warning{?s}  |  diagnose pass: {diag$pass}",
    ">" = "predict: {if (isTRUE(pchk$summary$predict_ok)) 'ok' else 'FAILED'}  |  {pchk$summary$n_predictions} rows  |  monotone: {pchk$summary$intervals_monotone}",
    ">" = "saved: {.path {fit_path}}"
  ))
  if (!isTRUE(pchk$summary$predict_ok)) {
    cli::cli_warn("predict() check failed for {.val {model$name}}: {pchk$summary$predict_error}")
  }

  diag_scalars <- diag[vapply(diag, function(x) length(x) == 1L && !is.list(x), logical(1))]

  tibble::as_tibble(diag_scalars) %>%
    dplyr::bind_cols(pchk$summary) %>%
    dplyr::mutate(
      n_fit_warnings = length(warns),
      run_at         = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      fit_path       = fit_path
    )
}

results <- purrr::map_dfr(models, run_stage0_one, panel = panel)

# ---- Merge with earlier partial runs, write ----------------
diag_path <- file.path(stage0_dir, "diagnostics.csv")
if (file.exists(diag_path)) {
  prior <- read_csv(diag_path, show_col_types = FALSE) %>%
    dplyr::filter(!model %in% results$model)

  # readr guesses each column's type per-run from its own values (e.g. an
  # all-NA predict_error column reads back as logical, not character; a
  # timestamp-shaped run_at reads back as a datetime, not a string) - coerce
  # every shared column in `prior` to match the freshly-computed `results`,
  # which has the correct type, before binding.
  for (col in intersect(names(prior), names(results))) {
    caster <- switch(class(results[[col]])[1],
      character = as.character, logical = as.logical,
      integer = as.integer, numeric = as.numeric, as.character
    )
    prior[[col]] <- caster(prior[[col]])
  }
  results <- dplyr::bind_rows(prior, results)
}
results <- results %>%
  dplyr::relocate(
    model, run_at, pass, n_fit_warnings, predict_ok, intervals_monotone,
    n_predictions, pct_pred_na
  ) %>%
  dplyr::arrange(model)
write_csv(results, diag_path)

# ---- Console summary --------------------------------------
cli::cli_h2("Stage 0 complete")
cli::cli_inform("{.path {diag_path}}  ({nrow(results)} model{?s})")
results %>%
  dplyr::select(model, run_at, pass, n_fit_warnings, predict_ok, intervals_monotone, notes) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
