#!/usr/bin/env Rscript

#' ---
#' title: "03_check_window_coverage"
#' author: "K M Susong"
#' ---
#'
#' Overview:
#' ========
#' Pre-Stage-1 gate. Before any model is built, confirm the pinned panel can
#' actually support the rolling-origin backtest under each candidate training
#' window (fixed 5 / 7 / 10 years and expanding), and show exactly where it
#' cannot.
#'
#' For every monthly origin from `origin_start` to the last origin that still
#' leaves `max(forecast_horizons)` months of outcome, and every window length:
#'   - `n_train_months`  : valued (non-gap) months a country has inside the
#'                         training window
#'   - `n_gap_in_window`  : gap months inside the window
#'   - `n_targets_with_truth` : how many of the h = 1..H target months the
#'                              country has a value for
#'   - `eligible`        : n_train_months >= min_train_months AND at least one
#'                         target has truth
#'
#' No model fitting. Outputs feed the Stage 1 runner and a human-read report.
#'
#' Input : Output/forecasting/training_data/training_panel_<snapshot_date>.csv
#' Output: Output/forecasting/cv_splits/rolling_origins.csv         (grid level)
#'         Output/forecasting/cv_splits/rolling_origin_country.csv  (per country)
#'         Output/forecasting/window_coverage_report.txt            (human read)
#'
#' Timeline:
#' ========
#' 03-09-2026: Created.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(cli)
})

# ---- Define project configs ----------------------------------------
source("Scripts/forecasting/00_config.R")

panel_path <- file.path(forecast_out, "training_data",
                        paste0("training_panel_", snapshot_date, ".csv"))
if (!file.exists(panel_path)) {
  cli::cli_abort(c(
    "Training panel not found: {.path {panel_path}}.",
    "i" = "Run {.path Scripts/forecasting/01_prepare_training_data.R} first."
  ))
}

cv_dir <- file.path(forecast_out, "cv_splits")
dir.create(cv_dir, recursive = TRUE, showWarnings = FALSE)
report_path <- file.path(forecast_out, "window_coverage_report.txt")

max_horizon <- max(forecast_horizons)

# ---- Panel: spans and valued months --------------------------------
panel <- read_csv(panel_path, show_col_types = FALSE) %>%
  dplyr::mutate(
    date        = as.Date(date),
    region      = factor(region, levels = region_levels),
    is_valued   = data_status != "gap"
  )

country_span <- panel %>%
  dplyr::group_by(iso3, region) %>%
  dplyr::summarise(
    first_date = min(date),
    last_date  = max(date),
    .groups    = "drop"
  )

valued_months <- panel %>%
  dplyr::filter(is_valued) %>%
  dplyr::select(iso3, date)

# ---- Rolling origins ---------------------------------------------
last_month <- max(panel$date)
max_origin <- last_month %m-% months(max_horizon)
origins    <- seq(origin_start, max_origin, by = "month")

# training months at each window
win_tbl <- tibble::tibble(
  window_type = names(roll_windows),
  window_months = as.integer(roll_windows)
)

# ALL window x origin x country combination
grid <- tidyr::crossing(origin_date = origins, win_tbl) %>%
  tidyr::crossing(country_span) %>%
  dplyr::mutate(
    grid_id     = dplyr::row_number(),
    train_end   = origin_date,
    train_start = dplyr::if_else(
      is.na(window_months),
      first_date,
      pmax(origin_date %m-% months(window_months - 1L), first_date)
    )
  )

# ---- Training months in window (range join) ----------------------
train_counts <- grid %>%
  dplyr::select(grid_id, iso3, train_start, train_end) %>%
  dplyr::left_join(
    valued_months,
    by = dplyr::join_by(iso3, x$train_start <= y$date, y$date <= x$train_end)
  ) %>%
  dplyr::group_by(grid_id) %>%
  dplyr::summarise(n_train_months = sum(!is.na(date)), .groups = "drop")

# ---- Target months with truth ----------------------------------
target_truth <- grid %>%
  dplyr::select(grid_id, iso3, origin_date) %>%
  tidyr::crossing(h = forecast_horizons) %>%
  dplyr::mutate(target_date = origin_date %m+% months(h)) %>%
  dplyr::left_join(
    valued_months %>% dplyr::mutate(has_truth = TRUE),
    by = dplyr::join_by(iso3, target_date == date)
  ) %>%
  dplyr::group_by(grid_id) %>%
  dplyr::summarise(n_targets_with_truth = sum(!is.na(has_truth)), .groups = "drop")

# ---- Assemble per-country eligibility --------------------------
per_country <- grid %>%
  dplyr::left_join(train_counts, by = "grid_id") %>%
  dplyr::left_join(target_truth, by = "grid_id") %>%
  dplyr::mutate(
    n_train_months       = tidyr::replace_na(n_train_months, 0L),
    n_targets_with_truth = tidyr::replace_na(n_targets_with_truth, 0L),
    eff_start            = pmax(train_start, first_date),
    eff_end              = pmin(train_end, last_date),
    window_span_months   = pmax(
      0L,
      as.integer(lubridate::interval(eff_start, eff_end) %/% months(1)) + 1L
    ),
    n_gap_in_window = pmax(0L, window_span_months - n_train_months),
    eligible = n_train_months >= min_train_months & n_targets_with_truth > 0
  ) %>%
  dplyr::select(
    window_type, origin_date, iso3, region, train_start, train_end,
    n_train_months, n_gap_in_window, n_targets_with_truth, eligible
  ) %>%
  dplyr::arrange(window_type, origin_date, region, iso3)

# ---- Grid-level summary --------------------------------------
rolling_origins <- per_country %>%
  dplyr::group_by(window_type, origin_date) %>%
  dplyr::summarise(
    train_end            = dplyr::first(train_end),
    earliest_train_start = min(train_start),
    n_countries_total    = dplyr::n(),
    n_countries_eligible = sum(eligible),
    .groups              = "drop"
  ) %>%
  dplyr::left_join(win_tbl, by = "window_type") %>%
  dplyr::mutate(
    nominal_train_start = dplyr::if_else(
      is.na(window_months),
      as.Date(NA),
      origin_date %m-% months(window_months - 1L)
    )
  ) %>%
  dplyr::select(
    window_type, origin_date, nominal_train_start, earliest_train_start,
    train_end, n_countries_total, n_countries_eligible
  ) %>%
  dplyr::arrange(window_type, origin_date)

# ---- Write machine-readable outputs --------------------------
write_csv(rolling_origins, file.path(cv_dir, "rolling_origins.csv"))
write_csv(per_country, file.path(cv_dir, "rolling_origin_country.csv"))

# ---- Human-readable report ----------------------------------
window_order <- c("fixed_5y", "fixed_7y", "fixed_10y", "expanding")
window_order <- window_order[window_order %in% unique(per_country$window_type)]

rl <- character(0)
add <- function(...) rl <<- c(rl, sprintf(...))

add("GDO forecast backtest -- window coverage check")
add("Generated %s", format(Sys.time(), "%Y-%m-%d %H:%M"))
add("")
add("Data snapshot        : %s", snapshot_date)
add("Panel span           : %s to %s", format(min(panel$date)), format(last_month))
add("Countries            : %d", dplyr::n_distinct(panel$iso3))
add("Forecast horizons    : %s months", paste(range(forecast_horizons), collapse = "-"))
add("Min training months  : %d", min_train_months)
add("Rolling origins       : %s to %s  (%d monthly origins)",
    format(min(origins)), format(max(origins)), length(origins))
add("")

add("== Eligible countries per window ==")
add("%-11s %8s %8s %8s %10s %10s", "window", "first", "median", "last",
    ">= 40 by", ">= 60 by")
for (w in window_order) {
  d <- rolling_origins %>% dplyr::filter(window_type == w) %>% dplyr::arrange(origin_date)
  reach <- function(n) {
    hit <- d %>% dplyr::filter(n_countries_eligible >= n)
    if (nrow(hit) == 0) "never" else format(min(hit$origin_date))
  }
  add("%-11s %8d %8.0f %8d %10s %10s",
      w,
      d$n_countries_eligible[1],
      stats::median(d$n_countries_eligible),
      d$n_countries_eligible[nrow(d)],
      reach(40), reach(60))
}
add("")

add("== Countries never eligible (any origin) ==")
for (w in window_order) {
  ever <- per_country %>%
    dplyr::filter(window_type == w, eligible) %>%
    dplyr::distinct(iso3) %>%
    dplyr::pull(iso3)
  never <- sort(setdiff(unique(per_country$iso3), ever))
  add("%-11s : %d", w, length(never))
  if (length(never) > 0) {
    add("   %s", paste(never, collapse = ", "))
  }
}
add("")

add("== First origin each country becomes eligible ==")
first_elig <- per_country %>%
  dplyr::filter(eligible) %>%
  dplyr::group_by(window_type, iso3) %>%
  dplyr::summarise(first_origin = min(origin_date), .groups = "drop")

for (w in window_order) {
  d <- first_elig %>% dplyr::filter(window_type == w)
  brk <- d %>%
    dplyr::mutate(yr = lubridate::year(first_origin)) %>%
    dplyr::count(yr, name = "n_countries")
  add("%s (n eligible somewhere = %d):", w, nrow(d))
  add("   %s", paste(sprintf("%d:%d", brk$yr, brk$n_countries), collapse = "  "))
}
add("")

no_2026 <- setdiff(
  unique(panel$iso3),
  panel %>% dplyr::filter(is_valued, lubridate::year(date) == 2026) %>%
    dplyr::distinct(iso3) %>% dplyr::pull(iso3)
)
add("== Note ==")
add("%d countries have no valued month in 2026, so they drop out of the",
    length(no_2026))
add("latest origins regardless of window:")
add("   %s", paste(sort(no_2026), collapse = ", "))

writeLines(rl, report_path)

# ---- Console summary ---------------------------------------
cli::cli_h2("Window coverage check")
cli::cli_inform(c(
  "*" = "{.path {file.path(cv_dir, 'rolling_origins.csv')}}  ({nrow(rolling_origins)} rows)",
  "*" = "{.path {file.path(cv_dir, 'rolling_origin_country.csv')}}  ({nrow(per_country)} rows)",
  "*" = "{.path {report_path}}"
))

cli::cli_h3("eligible countries per window (first / median / last origin)")
rolling_origins %>%
  dplyr::group_by(window_type) %>%
  dplyr::arrange(origin_date, .by_group = TRUE) %>%
  dplyr::summarise(
    first  = dplyr::first(n_countries_eligible),
    median = round(stats::median(n_countries_eligible)),
    last   = dplyr::last(n_countries_eligible),
    .groups = "drop"
  ) %>%
  dplyr::arrange(match(window_type, window_order)) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

