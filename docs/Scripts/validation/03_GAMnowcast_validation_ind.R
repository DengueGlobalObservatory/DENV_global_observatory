# =============================================================================
# 03_GAMnowcast_validation_ind.R
# Prototype: direct negative-binomial GAM nowcast — row-level LOSO validation
# =============================================================================
# Reads the same `full_data_season_monthly_proportions.csv` used by
# 03_nowcast_validation_ind.R (latest dated `Output/YYYY_MM_DD/` written by
# Scripts/V1_Pipeline.R Step 8). Builds leave-one-season-out (LOSO) prediction
# rows where, for each (country, held-out season, cutoff k, prediction month
# m > k), the row carries:
#
#   - actual_cases               : observed cases at month m of held-out season
#   - observed_cumulative_cases_at_cutoff (C_{c,s,k})
#   - last_observed_cases (L_{c,s,k})
#   - Ave_cum_monthly_proportion_at_cutoff (P_{c,-s,k})
#   - Ave_monthly_proportion_missing (p_{c,-s,m})
#   - lead_time = m - k
#   - season_month = m (cyclic 1..12)
#   - prediction_calendar_month = calendar Month at month m (cyclic 1..12)
#   - cutoff_month_num = k (cyclic 1..12)
#   - Region (factor), iso3 (factor)
#
# Validation strategies (set STRATEGY below):
#   - "loso" (default; required for iso3 random-effect variant)
#       For each held-out (country c, season s) fold, fit one bam() on rows
#       from all OTHER (c', s') pairs (including c's other seasons), then
#       predict the (c, s) test rows. Country c's iso3 random intercept is
#       therefore informed by c's other seasons.
#   - "loco"
#       For each held-out country c, fit one bam() on rows from countries
#       other than c. Faster (~80 fits) but invalid for iso3 RE (held-out
#       country has no estimated random intercept).
#
# Model variants (set MODEL_VARIANT below):
#   - "base"         smooths over predictors only; no country term.
#   - "iso3_re"      adds s(iso3_factor, bs = "re") — country random intercepts.
#   - "iso3_smooth"  replaces the global s(season_month, bs="cc") with a
#                    factor-smooth interaction
#                      s(season_month, iso3_factor, bs="fs", xt=list(bs="cc"))
#                    so each country gets its own cyclic seasonal shape, all
#                    shrunk toward a shared shape (single smoothing parameter).
#                    The factor-smooth absorbs the per-country intercept.
#
# Output: Output/validation/gam_prototype/validation_detail_gam_<variant>.csv
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(mgcv)
  library(parallel)
})

source("Scripts/validation/FUNCTIONS/00_FUN_validation_metrics.R")

# --- Constants ---------------------------------------------------------------
MIN_SEASONS <- 3L
MIN_TRAIN_SEASONS <- 2L

# CLI / env override; defaults to leave-one-season-out + base model.
STRATEGY <- Sys.getenv("GAM_STRATEGY", unset = "loso")
MODEL_VARIANT <- Sys.getenv("GAM_VARIANT", unset = "base")

if (!STRATEGY %in% c("loso", "loco")) {
  stop("STRATEGY must be one of 'loso', 'loco' (got: ", STRATEGY, ")")
}
if (!MODEL_VARIANT %in% c("base", "iso3_re", "iso3_smooth")) {
  stop("MODEL_VARIANT must be one of 'base', 'iso3_re', 'iso3_smooth' (got: ", MODEL_VARIANT, ")")
}
if (MODEL_VARIANT %in% c("iso3_re", "iso3_smooth") && STRATEGY != "loso") {
  stop(
    "MODEL_VARIANT '", MODEL_VARIANT,
    "' requires STRATEGY 'loso' (under LOCO the held-out country contributes no rows ",
    "to estimate its random intercept / country-specific seasonal smooth)."
  )
}

iso3_term <- switch(
  MODEL_VARIANT,
  base        = "none",
  iso3_re     = "re",
  iso3_smooth = "smooth"
)

# Parallel fits across folds. Allow override via env var. Each worker uses 1
# bam thread to avoid oversubscription. Defaults to detectCores - 2 (cap 8).
n_cores_env <- suppressWarnings(as.integer(Sys.getenv("GAM_CORES", unset = "")))
n_cores <- if (!is.na(n_cores_env) && n_cores_env > 0) {
  n_cores_env
} else {
  max(1L, min(8L, parallel::detectCores() - 2L))
}

out_dir <- "Output/validation/gam_prototype"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

# --- Locate latest pipeline run that saved the proportions table ------------
run_dirs <- list.dirs("Output", recursive = FALSE) %>%
  keep(~ str_detect(basename(.x), "^\\d{4}_\\d{2}_\\d{2}$")) %>%
  sort()

prop_paths <- file.path(run_dirs, "full_data_season_monthly_proportions.csv")
prop_existing <- prop_paths[file.exists(prop_paths)]

if (length(prop_existing) == 0) {
  stop(
    "No full_data_season_monthly_proportions.csv found under Output/YYYY_MM_DD/. ",
    "Run Scripts/V1_Pipeline.R first so Step 8 writes this file."
  )
}

prop_csv <- prop_existing[length(prop_existing)]
message("Using proportions file: ", prop_csv)
message("STRATEGY = ", STRATEGY, "  |  MODEL_VARIANT = ", MODEL_VARIANT,
        "  |  cores = ", n_cores)

# --- Load season-monthly proportions + attach Region ------------------------
validation_data <- read_csv(prop_csv, show_col_types = FALSE) %>%
  dplyr::select(
    country, iso3, Year, season, season_nMonth, Month, cases,
    Actual_monthly_proportion, Actual_cum_monthly_proportion
  )

region_lookup <- read_csv(
  "Assets/Stable/OD_maps/pred_downscale_with_ci_V3.csv",
  show_col_types = FALSE
) %>%
  transmute(
    iso3 = ISO_A0,
    Region = od_region
  ) %>%
  distinct(iso3, .keep_all = TRUE)

validation_data <- validation_data %>%
  left_join(region_lookup, by = "iso3")

# --- Build LOSO prediction rows for all eligible country-seasons ------------
# Each row has predictors and `actual_cases`. P_k and p_m are LOSO (computed
# from the country's other seasons), so no held-out-season information leaks
# into a row's seasonal-profile predictors.
message("Building LOSO prediction rows ...")
all_rows <- build_gam_rows_all(
  validation_data,
  min_seasons = MIN_SEASONS,
  min_train_seasons = MIN_TRAIN_SEASONS
) %>%
  add_gam_features()

if (nrow(all_rows) == 0) {
  stop("No GAM prediction rows constructed; check input data and filters.")
}

message(
  "Constructed ", nrow(all_rows), " LOSO rows across ",
  dplyr::n_distinct(all_rows$iso3), " countries and ",
  dplyr::n_distinct(paste(all_rows$iso3, all_rows$season)), " country-season folds."
)

# Keep iso3 factor levels stable across folds — needed for iso3 RE so that
# predict() at fold time recognises the held-out country's level even though
# none of the test rows are in the training fold.
iso3_levels <- sort(unique(all_rows$iso3))
all_rows$iso3_factor <- factor(all_rows$iso3, levels = iso3_levels)

# --- Fold execution ----------------------------------------------------------
fit_and_predict <- function(train_df, test_df) {
  fit <- fit_gam_nowcast(
    train_df,
    iso3_term = iso3_term,
    nthreads = 1L
  )
  predict_gam_nowcast(fit, test_df)
}

predict_one_country_loco <- function(c_iso3) {
  train_df <- all_rows %>% dplyr::filter(iso3 != c_iso3)
  test_df  <- all_rows %>% dplyr::filter(iso3 == c_iso3)
  fit_and_predict(train_df, test_df)
}

predict_one_fold_loso <- function(c_iso3, s_holdout) {
  train_df <- all_rows %>% dplyr::filter(!(iso3 == c_iso3 & season == s_holdout))
  test_df  <- all_rows %>% dplyr::filter(iso3 == c_iso3, season == s_holdout)
  fit_and_predict(train_df, test_df)
}

run_in_parallel <- function(fold_args, fold_fn) {
  use_parallel <- n_cores > 1L && .Platform$OS.type == "unix"
  if (use_parallel) {
    parallel::mclapply(
      seq_along(fold_args[[1]]),
      function(i) {
        do.call(fold_fn, lapply(fold_args, `[`, i))
      },
      mc.cores = n_cores,
      mc.preschedule = FALSE
    )
  } else {
    lapply(seq_along(fold_args[[1]]), function(i) {
      do.call(fold_fn, lapply(fold_args, `[`, i))
    })
  }
}

t0 <- Sys.time()

if (STRATEGY == "loco") {
  countries <- sort(unique(all_rows$iso3))
  message("Fitting ", length(countries), " leave-one-country-out GAMs ...")
  results <- run_in_parallel(list(c_iso3 = countries), predict_one_country_loco)
} else {
  folds <- all_rows %>%
    dplyr::distinct(iso3, season) %>%
    dplyr::arrange(iso3, season)
  message("Fitting ", nrow(folds), " leave-one-(country, season)-out GAMs ...")
  results <- run_in_parallel(
    list(c_iso3 = folds$iso3, s_holdout = folds$season),
    predict_one_fold_loso
  )
}

predictions <- dplyr::bind_rows(results)

elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
message(sprintf("Fold loop done in %.1f s (%.1f min)", elapsed, elapsed / 60))

# --- Errors (mirror columns / definitions of empirical detail file) ----------
validation_detail_gam <- predictions %>%
  dplyr::mutate(
    absolute_error = predicted_cases - actual_cases,
    squared_error = absolute_error^2,
    relative_error = dplyr::if_else(
      !is.na(actual_cases) & actual_cases > 0,
      (predicted_cases - actual_cases) / actual_cases,
      NA_real_
    ),
    absolute_percent_error = dplyr::if_else(
      !is.na(actual_cases) & actual_cases > 0,
      abs(actual_cases - predicted_cases) / actual_cases,
      NA_real_
    )
  ) %>%
  dplyr::select(
    iso3, country, Region, season, cutoff_month, prediction_month, Month,
    actual_cases, predicted_cases,
    absolute_error, squared_error, relative_error, absolute_percent_error,
    lead_time,
    observed_cumulative_cases_at_cutoff, last_observed_cases,
    Ave_cum_monthly_proportion_at_cutoff, Ave_monthly_proportion_missing
  )

out_path <- file.path(
  out_dir,
  paste0("validation_detail_gam_", MODEL_VARIANT, ".csv")
)
write_csv(validation_detail_gam, out_path)
message(
  "Wrote ", out_path,
  " (nrow = ", nrow(validation_detail_gam),
  ", strategy = ", STRATEGY,
  ", variant = ", MODEL_VARIANT, ")"
)
