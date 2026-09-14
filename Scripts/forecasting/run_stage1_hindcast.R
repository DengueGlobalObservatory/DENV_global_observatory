#!/usr/bin/env Rscript

#' ---
#' title: "run_stage1_hindcast"
#' author: "K M Susong"
#' ---
#'
#' Overview:
#' ========
#' Stage 1 runner: rolling-origin hindcast. For every (model, window_type,
#' origin_date) cell in the precomputed grid (`03_check_window_coverage.R`'s
#' `rolling_origin_country.csv`):
#'   1. slice the training panel to that window (per-country train_start,
#'      country-clamped - see `slice_train()`)
#'   2. fit() on the slice, predict() 1..6 months ahead for every country
#'   3. attach truth (`attach_truth()`, NA where the future hasn't happened
#'      yet), n_train_months / n_gap_in_window per country, model, window_type
#'   4. save the fitted object (if `save_fit_objects`) and log the cell
#' No skill is scored here - that's `run_stage1_score.R` (Step 7). Every
#' window is run regardless of per-country `eligible` (a country a model
#' can't usefully fit just gets NA forecasts) - eligibility is a downstream
#' filter, not a gate here.
#'
#' Config block below is "edit freely"; CLI flags override it for one run
#' without touching the file. Positional args are model slugs (as
#' `run_stage0_fit.R`).
#'
#'   Rscript Scripts/forecasting/run_stage1_hindcast.R nowcast glm_ar1 \
#'     --windows=expanding --origins=2020-01-01,2022-01-01 \
#'     --origin-stride=6 --workers=4
#'
#' `--windows=` comma-separated window_type names; `--origins=` comma-separated
#' YYYY-MM-DD dates (overrides the full origin_start.. range); `--origin-stride=`
#' keep every Nth origin from whatever range is in play (cheap way to shrink a
#' test run); `--workers=` mclapply core count.
#'
#' Cells are parallelised (`parallel::mclapply`) within each model, one model
#' at a time. Workers never touch disk directly - each just returns its
#' forecast rows and (if `save_fit_objects`) its fitted model in memory; the
#' PARENT does every write, once, after a model's batch of cells finishes:
#' one `write_forecast_csv()` per (model, window_type) forecasts CSV, and one
#' `write_fit_bundle()` per (model, window_type) `.rds` - all that window's
#' origins bundled into a single file (not one file per origin: far fewer
#' files, which matters since this repo is Dropbox-synced) - both replacing
#' just the re-run origins/cells, leaving the rest of each file untouched.
#'
#' Input : Output/forecasting/training_data/training_panel_<snapshot_date>.csv
#'         Output/forecasting/cv_splits/rolling_origin_country.csv
#' Output: Output/forecasting/stage1_hindcast/forecasts/<model>__<window_type>.csv
#'         Output/forecasting/stage1_hindcast/fits/<model>__<window_type>.rds
#'           (a named list, one element per origin_date, each a
#'           `forecast_model_record()` - `readRDS(path)[["2020-01-01"]]$fitted`)
#'         Output/forecasting/stage1_hindcast/run_log.csv
#'
#' Timeline:
#' ========
#' 15-09-2026: Created.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(tibble)
  library(lubridate)
  library(parallel)
  library(cli)
})

# ---- Locate project root ---------------------------------------
if (!dir.exists("Output") && dir.exists("../../Output")) setwd("../..")
if (!dir.exists("Output")) {
  cli::cli_abort("Run from the DENV_global_observatory repo root (cannot find {.path Output/}).")
}

source("Scripts/forecasting/00_config.R")
source("Scripts/forecasting/models/utils/forecast_helpers.R")

# ---- Config (edit freely) ---------------------------------------
windows_to_run <- names(roll_windows)   # e.g. c("expanding") to run just one
origins_to_run <- NULL                  # NULL = every origin in the grid
origin_stride  <- 1L                    # keep every Nth origin (>= 1)
models_to_run  <- NULL                  # NULL = every models/NN_*.R file
n_workers      <- 4L

# ---- CLI overrides ------------------------------------------------
cli_args  <- commandArgs(trailingOnly = TRUE)
flag_args <- grep("^--", cli_args, value = TRUE)
slug_args <- setdiff(cli_args, flag_args)

parse_flag <- function(flag) {
  hit <- grep(paste0("^--", flag, "="), flag_args, value = TRUE)
  if (length(hit) == 0L) return(NULL)
  sub(paste0("^--", flag, "="), "", hit[[1]])
}

if (!is.null(v <- parse_flag("windows")))       windows_to_run <- strsplit(v, ",")[[1]]
if (!is.null(v <- parse_flag("origins")))       origins_to_run <- as.Date(strsplit(v, ",")[[1]])
if (!is.null(v <- parse_flag("origin-stride"))) origin_stride  <- as.integer(v)
if (!is.null(v <- parse_flag("workers")))       n_workers      <- as.integer(v)
if (length(slug_args) > 0L)                     models_to_run  <- slug_args

#---check: every requested window_type is a real one (roll_windows' names)
unknown_windows <- setdiff(windows_to_run, names(roll_windows))
if (length(unknown_windows) > 0L) {
  cli::cli_abort("Unknown window_type{?s}: {.val {unknown_windows}}. Available: {.val {names(roll_windows)}}.")
}

model_dir     <- "Scripts/forecasting/models"
hindcast_dir  <- file.path(forecast_out, "stage1_hindcast")
forecasts_dir <- file.path(hindcast_dir, "forecasts")
fits_dir      <- file.path(hindcast_dir, "fits")
run_log_path  <- file.path(hindcast_dir, "run_log.csv")
dir.create(forecasts_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Load model definitions (mirrors run_stage0_fit.R) ------------
load_model_file <- function(path) {
  env <- new.env(parent = globalenv())
  sys.source(path, envir = env)
  hits <- Filter(function(x) inherits(x, "forecast_model"), mget(ls(env), envir = env))
  if (length(hits) != 1L) {
    cli::cli_abort("{.path {path}} must define exactly one forecast_model object (found {length(hits)}).")
  }
  hits[[1]]
}

#---check: at least one model file was found to discover
model_files <- sort(list.files(model_dir, pattern = "^[0-9].*\\.R$", full.names = TRUE))
if (length(model_files) == 0L) {
  cli::cli_abort("No model files matching {.code ^[0-9].*\\.R$} in {.path {model_dir}}.")
}
models <- lapply(model_files, load_model_file)
names(models) <- vapply(models, `[[`, character(1), "name")

#---check: every positional slug arg names a model that was actually discovered
if (!is.null(models_to_run)) {
  unknown <- setdiff(models_to_run, names(models))
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "Unknown model slug{?s}: {.val {unknown}}.",
      "i" = "Available: {.val {names(models)}}"
    ))
  }
  models <- models[models_to_run]
}
#---log: which models/windows this run will cover
cli::cli_inform("Stage 1 hindcast for: {.val {names(models)}}  |  windows: {.val {windows_to_run}}")

# ---- Training panel & rolling-origin grid --------------------------
#---check: the pinned training panel exists (run 01_prepare_training_data.R if not)
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

#---check: the rolling-origin grid exists (run 03_check_window_coverage.R if not)
grid_path <- file.path(forecast_out, "cv_splits", "rolling_origin_country.csv")
if (!file.exists(grid_path)) {
  cli::cli_abort(c(
    "Rolling-origin grid not found: {.path {grid_path}}.",
    "i" = "Run {.path Scripts/forecasting/03_check_window_coverage.R} first."
  ))
}
grid <- read_csv(grid_path, show_col_types = FALSE) %>%
  dplyr::mutate(
    origin_date = as.Date(origin_date),
    train_start = as.Date(train_start),
    train_end   = as.Date(train_end)
  ) %>%
  dplyr::filter(window_type %in% windows_to_run)

if (!is.null(origins_to_run)) {
  grid <- dplyr::filter(grid, origin_date %in% origins_to_run)
}
if (origin_stride > 1L) {
  keep <- sort(unique(grid$origin_date))
  keep <- keep[seq(1L, length(keep), by = origin_stride)]
  grid <- dplyr::filter(grid, origin_date %in% keep)
}
#---check: the window/origin filters above didn't filter the grid down to nothing
if (nrow(grid) == 0L) {
  cli::cli_abort("No rows left in the rolling-origin grid after filtering - check --windows/--origins.")
}

cells <- dplyr::distinct(grid, window_type, origin_date) %>%
  dplyr::arrange(window_type, origin_date)
#---log: size of the grid this run is about to work through
cli::cli_inform("{nrow(cells)} (window_type, origin_date) cell{?s} per model.")

# ---- One cell: fit, predict, attach truth & metadata, save fit --
#' Run one (model, window_type, origin_date) cell.
#'
#' @param mdl A `forecast_model`.
#' @param window_type,origin_date This cell's window and origin.
#' @param cell_rows This cell's per-country grid rows (`iso3`, `train_start`,
#'   `train_end`, `n_train_months`, `n_gap_in_window`).
#' @param panel The full training panel.
#' @return `list(forecasts, fit_record, log)` - `forecasts`/`fit_record` are
#'   `NULL` on a caught error (`fit_record` is also `NULL` whenever
#'   `save_fit_objects` is `FALSE`); `log` is always a one-row tibble
#'   (`run_log.csv`'s row for this cell). Nothing is written to disk here -
#'   the parent bundles and writes after a whole model's cells finish.
run_one_cell <- function(mdl, window_type, origin_date, cell_rows, panel) {
  t0 <- Sys.time()
  warns <- character(0)
  log_row <- function(n_forecasts, fit_error) {
    tibble::tibble(
      model = mdl$name, window_type = window_type, origin_date = origin_date,
      n_countries = dplyr::n_distinct(cell_rows$iso3), n_forecasts = n_forecasts,
      n_fit_warnings = length(warns), fit_error = fit_error,
      seconds = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
    )
  }

  tryCatch({
    train_slice <- slice_train(panel, cell_rows)
    fitted <- withCallingHandlers(
      mdl$fit(train_slice, mdl$spec),
      warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }
    )
    targets <- build_targets(cell_rows$iso3, origin_date, forecast_horizons)
    fc <- check_forecast_output(mdl$predict(fitted, targets, mdl$spec), mdl$name) %>%
      dplyr::mutate(model = mdl$name, window_type = window_type) %>%
      dplyr::left_join(
        dplyr::select(cell_rows, iso3, n_train_months, n_gap_in_window), by = "iso3"
      ) %>%
      attach_truth(panel)

    fit_record <- if (isTRUE(save_fit_objects)) {
      forecast_model_record(fitted, meta = list(
        stage = "stage1_hindcast", snapshot = snapshot_date,
        window_type = window_type, origin_date = format(origin_date)
      ))
    } else {
      NULL
    }

    list(forecasts = fc, fit_record = fit_record, log = log_row(nrow(fc), NA_character_))
  }, error = function(e) {
    list(forecasts = NULL, fit_record = NULL, log = log_row(0L, conditionMessage(e)))
  })
}

# ---- Run: one model at a time, cells within a model in parallel --
all_logs <- vector("list", length(models))
names(all_logs) <- names(models)

for (mdl_name in names(models)) {
  mdl <- models[[mdl_name]]
  #---log: which model this batch of cells belongs to
  cli::cli_h3(mdl_name)

  cell_list <- purrr::pmap(cells, function(window_type, origin_date) {
    list(
      window_type = window_type,
      origin_date = origin_date,
      cell_rows   = dplyr::filter(grid, window_type == !!window_type, origin_date == !!origin_date)
    )
  })

  results <- parallel::mclapply(cell_list, function(cell) {
    run_one_cell(mdl, cell$window_type, cell$origin_date, cell$cell_rows, panel)
  }, mc.cores = n_workers)

  logs        <- purrr::map_dfr(results, "log")
  forecasts   <- purrr::map(results, "forecasts")
  fit_records <- purrr::map(results, "fit_record")
  # cells (and so results/logs/forecasts/fit_records) all share one index -
  # subset every one of them by the SAME logical vector, rather than compact()
  # then reconstruct alignment separately, so a forecasts/fit_records mismatch
  # can't silently happen.
  window_of <- logs$window_type
  origin_of <- format(logs$origin_date)

  n_ok <- sum(!vapply(forecasts, is.null, logical(1)))
  #---log: this model's batch results - how many cells succeeded, warned, errored
  cli::cli_inform(c(
    ">" = "{n_ok}/{length(results)} cells ok  |  {sum(logs$n_fit_warnings)} fit warnings  |  {sum(!is.na(logs$fit_error))} errors",
    ">" = "{round(sum(logs$seconds), 1)} cell-seconds total"
  ))

  # One CSV write per (model, window_type) group, after all its cells finish.
  has_forecast <- !vapply(forecasts, is.null, logical(1))
  by_window <- split(forecasts[has_forecast], window_of[has_forecast])
  for (wt in names(by_window)) {
    out_path <- file.path(forecasts_dir, paste0(mdl_name, "__", wt, ".csv"))
    write_forecast_csv(dplyr::bind_rows(by_window[[wt]]), out_path, key_cols = "origin_date")
    #---log: confirm each forecasts CSV was written, and where
    cli::cli_inform(c(">" = "wrote {.path {out_path}}"))
  }

  # One bundled .rds per (model, window_type) - every origin in this batch
  # together, not one file per origin (see the file overview).
  has_fit <- !vapply(fit_records, is.null, logical(1))
  by_window_fits     <- split(fit_records[has_fit], window_of[has_fit])
  origins_by_window  <- split(origin_of[has_fit], window_of[has_fit])
  for (wt in names(by_window_fits)) {
    new_fits <- stats::setNames(by_window_fits[[wt]], origins_by_window[[wt]])
    fit_path <- file.path(fits_dir, paste0(mdl_name, "__", wt, ".rds"))
    write_fit_bundle(new_fits, fit_path)
    #---log: confirm each fit bundle was written, and where
    cli::cli_inform(c(">" = "wrote {.path {fit_path}}"))
  }

  all_logs[[mdl_name]] <- logs
}

# ---- run_log.csv ---------------------------------------------------
write_forecast_csv(
  dplyr::bind_rows(all_logs),
  run_log_path,
  key_cols = c("model", "window_type", "origin_date")
)

# ---- Console summary ------------------------------------------------
#---log: whole-run wrap-up, read back from run_log.csv (not the in-memory
# logs) so it reflects what actually landed on disk, including prior runs'
# untouched cells.
cli::cli_h2("Stage 1 hindcast complete")
final_log <- read_csv(run_log_path, show_col_types = FALSE)
cli::cli_inform("{.path {run_log_path}}  ({nrow(final_log)} cell{?s} logged)")
final_log %>%
  dplyr::group_by(model, window_type) %>%
  dplyr::summarise(
    n_cells = dplyr::n(), n_errors = sum(!is.na(fit_error)),
    n_warnings = sum(n_fit_warnings), .groups = "drop"
  ) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
