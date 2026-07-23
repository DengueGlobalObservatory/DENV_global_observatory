# =============================================================================
# 03_GAMnowcast_validation_comparison.R
# Step-4-style comparison: empirical proportion baseline vs. GAM prototype(s)
# =============================================================================
# Joins the row-level outputs from the empirical baseline and one or more GAM
# variants on (iso3, season, cutoff_month, prediction_month, Month) so each
# comparison row represents the same held-out (country, season, cutoff k,
# prediction month m) observation evaluated under each method.
#
# Inputs:
#   Output/validation/validation_detail.csv                   (empirical baseline)
#   Output/validation/gam_prototype/validation_detail_gam_<variant>.csv
#       — one or more GAM variants (e.g. _base, _iso3_re).
#   For backward compatibility a legacy `validation_detail_gam.csv` is also
#   accepted (treated as variant "loco").
#
# Outputs (under Output/validation/gam_prototype/):
#   gam_vs_empirical_long_paired.csv     — joined per-row table, long format
#   gam_vs_empirical_overall.csv         — global metrics, all methods
#   gam_vs_empirical_by_country.csv      — per-country metrics, all methods
#   gam_vs_empirical_by_pair.csv         — per-(cutoff, prediction_month)
#   gam_vs_empirical_by_lead_time.csv    — per-lead-time
#   gam_vs_empirical_lowcount_overpred.csv — overprediction rate vs low actuals
#
# Metrics are MAPE-first. APE outliers are explicitly summarised via the
# 95th and 99th percentiles to surface the long right tail that drives the
# mean. RMSE_scaled is RMSE / mean_actual_cases within the relevant stratum.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

emp_path <- "Output/validation/validation_detail.csv"
gam_dir  <- "Output/validation/gam_prototype"
out_dir  <- gam_dir

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

if (!file.exists(emp_path)) {
  stop(
    "Missing empirical detail file: ", emp_path,
    "\nRun Scripts/validation/03_nowcast_validation_ind.R first."
  )
}

# Discover GAM variant files by suffix.
gam_files <- list.files(
  gam_dir,
  pattern = "^validation_detail_gam(_[A-Za-z0-9_]+)?\\.csv$",
  full.names = TRUE
)
if (length(gam_files) == 0) {
  stop(
    "No validation_detail_gam*.csv found under ", gam_dir,
    "\nRun Scripts/validation/03_GAMnowcast_validation_ind.R first."
  )
}

variant_from_path <- function(p) {
  stem <- tools::file_path_sans_ext(basename(p))
  v <- sub("^validation_detail_gam_?", "", stem)
  if (v == "") "legacy" else v
}

gam_variants <- vapply(gam_files, variant_from_path, character(1))
names(gam_files) <- gam_variants

message("Found GAM variants:")
for (v in names(gam_files)) {
  message("  ", v, "  -> ", gam_files[[v]])
}

# --- Load empirical -----------------------------------------------------------
emp <- read_csv(emp_path, show_col_types = FALSE) %>%
  dplyr::transmute(
    iso3, country, Region, season, cutoff_month, prediction_month, Month,
    actual_cases,
    method = "empirical",
    predicted_cases
  )

# --- Load all GAM variants in long format ------------------------------------
gam_long <- purrr::imap_dfr(gam_files, function(path, variant) {
  read_csv(path, show_col_types = FALSE) %>%
    dplyr::transmute(
      iso3, country, Region, season, cutoff_month, prediction_month, Month,
      actual_cases,
      method = paste0("gam_", variant),
      predicted_cases
    )
})

# Errors are recomputed from predicted_cases / actual_cases directly so all
# methods are on identical units regardless of how each input file stored its
# absolute_percent_error column (the empirical file stores percent, the GAM
# files store decimal). All percent quantities below are explicitly in
# percent (× 100), matching the convention used by summary_country.csv.
long_all <- dplyr::bind_rows(emp, gam_long) %>%
  dplyr::mutate(
    abs_err = abs(predicted_cases - actual_cases),
    sq_err  = (predicted_cases - actual_cases)^2,
    rel_err_pct = dplyr::if_else(
      !is.na(actual_cases) & actual_cases > 0,
      100 * (predicted_cases - actual_cases) / actual_cases,
      NA_real_
    ),
    ape_pct = dplyr::if_else(
      !is.na(actual_cases) & actual_cases > 0,
      100 * abs(actual_cases - predicted_cases) / actual_cases,
      NA_real_
    )
  )

# --- Restrict to rows where ALL methods produced a prediction ----------------
# A row is comparable only if every method (empirical + each GAM variant)
# produced a non-NA predicted_cases. We pivot wide on `method` for the join,
# then drop incomplete rows and pivot back.
fold_keys <- c("iso3", "season", "cutoff_month", "prediction_month", "Month")
context_cols <- c("country", "Region", "actual_cases")

wide <- long_all %>%
  dplyr::select(dplyr::all_of(c(fold_keys, context_cols, "method", "predicted_cases"))) %>%
  tidyr::pivot_wider(names_from = method, values_from = predicted_cases)

method_cols <- setdiff(colnames(wide), c(fold_keys, context_cols))

complete_mask <- rowSums(!is.na(wide[, method_cols, drop = FALSE])) == length(method_cols)

n_total <- nrow(wide)
n_complete <- sum(complete_mask)
message(sprintf(
  "Matched %d rows; %d have non-NA predictions for every method (%d dropped).",
  n_total, n_complete, n_total - n_complete
))

if (n_complete == 0) {
  stop("No rows with complete predictions across all methods.")
}

complete_keys <- wide[complete_mask, fold_keys, drop = FALSE]

paired_long <- long_all %>%
  dplyr::semi_join(complete_keys, by = fold_keys)

write_csv(paired_long, file.path(out_dir, "gam_vs_empirical_long_paired.csv"))

# Sort method levels so empirical comes first, GAM variants after.
method_levels <- c("empirical", sort(grep("^gam_", unique(paired_long$method), value = TRUE)))
paired_long <- paired_long %>%
  dplyr::mutate(method = factor(method, levels = method_levels))

# ----------------------------------------------------------------------------
# Helper: long-format metric block for any grouping
# ----------------------------------------------------------------------------
metric_block_long <- function(df, ...) {
  groups <- rlang::enquos(...)
  df %>%
    dplyr::group_by(!!!groups, method) %>%
    dplyr::summarise(
      n_predictions = dplyr::n(),
      n_actual_positive = sum(actual_cases > 0, na.rm = TRUE),
      mean_actual = mean(actual_cases, na.rm = TRUE),
      MAE = mean(abs_err, na.rm = TRUE),
      RMSE = sqrt(mean(sq_err, na.rm = TRUE)),
      MAPE_pct = mean(ape_pct, na.rm = TRUE),
      median_APE_pct = median(ape_pct, na.rm = TRUE),
      p95_APE_pct = quantile(ape_pct, 0.95, na.rm = TRUE),
      p99_APE_pct = quantile(ape_pct, 0.99, na.rm = TRUE),
      MRE_signed_pct = mean(rel_err_pct, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(RMSE_scaled = RMSE / mean_actual)
}

# ----------------------------------------------------------------------------
# Overall (one row per method)
# ----------------------------------------------------------------------------
overall <- metric_block_long(paired_long)
write_csv(overall, file.path(out_dir, "gam_vs_empirical_overall.csv"))

# ----------------------------------------------------------------------------
# Per-country
# ----------------------------------------------------------------------------
by_country <- metric_block_long(paired_long, iso3, country, Region) %>%
  dplyr::arrange(iso3, country, Region, method)
write_csv(by_country, file.path(out_dir, "gam_vs_empirical_by_country.csv"))

# ----------------------------------------------------------------------------
# Per-(cutoff_month, prediction_month) operational pair
# ----------------------------------------------------------------------------
by_pair <- metric_block_long(paired_long, cutoff_month, prediction_month) %>%
  dplyr::arrange(cutoff_month, prediction_month, method)
write_csv(by_pair, file.path(out_dir, "gam_vs_empirical_by_pair.csv"))

# ----------------------------------------------------------------------------
# Per-lead-time
# ----------------------------------------------------------------------------
by_lead_time <- paired_long %>%
  dplyr::mutate(lead_time = prediction_month - cutoff_month) %>%
  metric_block_long(lead_time) %>%
  dplyr::arrange(lead_time, method)
write_csv(by_lead_time, file.path(out_dir, "gam_vs_empirical_by_lead_time.csv"))

# ----------------------------------------------------------------------------
# Overprediction rate at low observed counts
# Buckets observed cases into 0, 1-4, 5-19, 20-99, 100+, summarises per method
# how often each predicts > 5 * max(actual, 1) and shows median / p95 APE.
# ----------------------------------------------------------------------------
lowcount <- paired_long %>%
  dplyr::mutate(
    actual_bucket = dplyr::case_when(
      actual_cases == 0 ~ "0",
      actual_cases <= 4 ~ "1-4",
      actual_cases <= 19 ~ "5-19",
      actual_cases <= 99 ~ "20-99",
      TRUE ~ "100+"
    ),
    actual_bucket = factor(
      actual_bucket,
      levels = c("0", "1-4", "5-19", "20-99", "100+")
    ),
    overshoot = predicted_cases > pmax(actual_cases * 5, 5)
  ) %>%
  dplyr::group_by(actual_bucket, method) %>%
  dplyr::summarise(
    n_predictions = dplyr::n(),
    overshoot_rate = mean(overshoot, na.rm = TRUE),
    median_APE_pct = median(ape_pct, na.rm = TRUE),
    p95_APE_pct = quantile(ape_pct, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(actual_bucket, method)
write_csv(lowcount, file.path(out_dir, "gam_vs_empirical_lowcount_overpred.csv"))

# ----------------------------------------------------------------------------
# Console summary (compact diagnostic for go/no-go decisions)
# ----------------------------------------------------------------------------
fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits, big.mark = ",")
message("\n--- GAM prototype variants vs empirical baseline (matched rows) ---")
message("All percent quantities are in percent units (e.g. 50 = 50% APE).")
overall_print <- overall %>%
  dplyr::transmute(
    method,
    n = n_predictions,
    mean_actual = round(mean_actual, 1),
    MAPE_pct = round(MAPE_pct, 2),
    median_APE_pct = round(median_APE_pct, 2),
    p95_APE_pct = round(p95_APE_pct, 2),
    p99_APE_pct = round(p99_APE_pct, 2),
    MAE = round(MAE, 1),
    RMSE_scaled = round(RMSE_scaled, 2),
    MRE_signed_pct = round(MRE_signed_pct, 2)
  )
print(as.data.frame(overall_print), row.names = FALSE)

message("\nWrote comparison tables under ", out_dir)
