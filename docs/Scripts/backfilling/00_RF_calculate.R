#' Overview
#' ========
#'
#' Monthly RF refresh script.
#'
#' This script recalculates empirical RF tables from crawler snapshots and writes
#' a single combined RF artifact (all sources) as monthly dated plus latest files
#' under Output/.
#'
#' Optional environment variables (narrow crawler snapshot window, faster tests):
#'   RF_DATE_START — YYYY-MM-DD or YYYYMMDD
#'   RF_DATE_END   — YYYY-MM-DD or YYYYMMDD

# Quiet progress before any readr/httr-heavy code loads (Rscript has no TTY).
Sys.setenv(VROOM_SHOW_PROGRESS = "false")
options(
  cli.progress_bar = FALSE,
  cli.spinner = FALSE,
  readr.show_progress = FALSE,
  dplyr.show_progress = FALSE
)

source("Scripts/backfilling/FUNCTIONS/00_FUN_dengue_rf_pipeline.R")

if (!exists("log_message")) {
  source("Scripts/utils/logging.R")
  ensure_logger(console = TRUE)
}

log_message("Running 00_RF_calculate.R")

.parse_rf_env_date <- function(nm) {
  v <- Sys.getenv(nm, unset = "")
  if (!nzchar(v)) {
    return(NULL)
  }
  d <- suppressWarnings(as.Date(v))
  if (!is.na(d)) {
    return(d)
  }
  suppressWarnings(as.Date(v, format = "%Y%m%d"))
}

rf_date_start <- .parse_rf_env_date("RF_DATE_START")
rf_date_end <- .parse_rf_env_date("RF_DATE_END")
if (!is.null(rf_date_start) || !is.null(rf_date_end)) {
  log_message(sprintf(
    "RF snapshot window: start=%s end=%s",
    if (is.null(rf_date_start)) "NULL" else as.character(rf_date_start),
    if (is.null(rf_date_end)) "NULL" else as.character(rf_date_end)
  ))
}

# Store RF artifacts under Output/RF
rf_output_dir <- file.path("Output", "RF")

t0 <- Sys.time()
rf_results <- run_monthly_rf_refresh(
  output_dir = rf_output_dir,
  run_date = Sys.Date(),
  date_start = rf_date_start,
  date_end = rf_date_end
)
log_message(sprintf("RF refresh elapsed: %.1f s", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

log_message(sprintf("Combined RF rows: %d", nrow(rf_results$combined_rf)))
log_message(sprintf("PAHO rows in combined RF: %d", nrow(rf_results$paho_rf)))
log_message(sprintf("SEARO rows in combined RF: %d", nrow(rf_results$searo_rf)))
log_message(sprintf("WHO_Global rows in combined RF: %d", nrow(rf_results$who_global_rf)))
log_message(paste("Combined dated RF:", rf_results$combined_dated_path))
log_message(paste("Combined latest RF:", rf_results$combined_latest_path))
