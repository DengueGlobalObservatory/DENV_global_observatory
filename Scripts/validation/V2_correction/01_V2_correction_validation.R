# =============================================================================
# 01_V2_correction_validation.R
# Compare V1 vs V2 pipeline outputs for the same run date (Step 4 correction).
# =============================================================================
# Reads:
#   Output/<date>_V1/  — production backfill path (PAHO-only correction)
#   Output/<date>_V2/  — V2 delay correction (PAHO + WHO)
# Writes: Output/validation/V2_correction/*.csv
# Run before knitting 02_V2_correction_validation.Rmd
# =============================================================================

suppressPackageStartupMessages(library(tidyverse))

# Walk up to repo root when sourced from Scripts/validation/V2_correction/
if (!dir.exists("Output") || !dir.exists("Scripts")) {
  dir <- getwd()
  for (i in seq_len(10)) {
    if (dir.exists(file.path(dir, "Output")) &&
        dir.exists(file.path(dir, "Scripts"))) {
      setwd(dir)
      break
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) break
    dir <- parent
  }
}
if (!dir.exists("Output")) {
  stop("Cannot find project root (expected Output/ and Scripts/).")
}

# --- Config ------------------------------------------------------------------
run_date <- "2026_06_11"
v1_dir   <- file.path("Output", paste0(run_date, "_V1"))
v2_dir   <- file.path("Output", paste0(run_date, "_V2"))
out_dir  <- "Output/validation/V2_correction"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Join keys — omit `source` (many historical rows have NA source; NA never matches in joins)
backfill_key <- c("iso3", "Year", "Month", "date")
nowcast_key  <- c("iso3", "Year", "Month", "date", "Data_status")

# --- Helpers -----------------------------------------------------------------
read_pipeline_csv <- function(dir, filename) {
  path <- file.path(dir, filename)
  if (!file.exists(path)) {
    stop("Missing file: ", path)
  }
  read_csv(path, show_col_types = FALSE)
}

compare_monthly_cases <- function(v1, v2, join_key, label) {
  join_key <- intersect(join_key, union(names(v1), names(v2)))

  v1_slim <- v1 %>%
    transmute(
      across(all_of(join_key)),
      source_v1 = source,
      cases_v1 = cases
    )

  v2_extra <- intersect(
    c("raw_cases", "corrected_cases", "d", "rf",
      "correction_applied", "correction_reason", "missing_reason"),
    names(v2)
  )

  v2_slim <- v2 %>%
    transmute(
      across(all_of(join_key)),
      source_v2 = source,
      cases_v2 = cases,
      across(all_of(v2_extra))
    )

  joined <- inner_join(v1_slim, v2_slim, by = join_key) %>%
    mutate(
      dataset = label,
      delta = cases_v2 - cases_v1,
      abs_delta = abs(delta),
      cases_match = cases_v1 == cases_v2,
      source = coalesce(source_v2, source_v1)
    )

  case_diffs <- joined %>%
    filter(!cases_match | is.na(cases_v1) | is.na(cases_v2))

  # Winning source changed for the same country–month
  source_switches <- joined %>%
    filter(!is.na(source_v1), !is.na(source_v2), source_v1 != source_v2)

  only_v1 <- anti_join(v1_slim, v2_slim, by = join_key)
  only_v2 <- anti_join(v2_slim, v1_slim, by = join_key)

  summary_tbl <- tibble(
    dataset = label,
    n_v1 = nrow(v1),
    n_v2 = nrow(v2),
    n_joined = nrow(joined),
    n_only_v1 = nrow(only_v1),
    n_only_v2 = nrow(only_v2),
    n_case_diffs = sum(!joined$cases_match, na.rm = TRUE),
    n_source_switches = nrow(source_switches),
    n_identical = sum(joined$cases_match, na.rm = TRUE),
    median_abs_delta = median(case_diffs$abs_delta, na.rm = TRUE),
    max_abs_delta = max(case_diffs$abs_delta, na.rm = TRUE)
  )

  list(
    joined = joined,
    case_diffs = case_diffs,
    source_switches = source_switches,
    summary = summary_tbl
  )
}

summarise_correction_audit <- function(df, source_label) {
  cases_col <- if ("total_den" %in% names(df)) "total_den" else "cases"
  corrected_col <- if ("total_corrected_cases" %in% names(df)) {
    "total_corrected_cases"
  } else {
    "cases_corrected"
  }
  applied_col <- if ("total_applied_cases" %in% names(df)) {
    "total_applied_cases"
  } else {
    "cases_applied"
  }

  df %>%
    mutate(
      source = source_label,
      raw = .data[[cases_col]],
      corrected = .data[[corrected_col]],
      applied = .data[[applied_col]],
      impact = applied - raw
    ) %>%
    group_by(source, correction_applied, correction_excluded, correction_reason) %>%
    summarise(
      n_rows = n(),
      n_countries = n_distinct(iso3),
      total_raw = sum(raw, na.rm = TRUE),
      total_applied = sum(applied, na.rm = TRUE),
      total_impact = sum(impact, na.rm = TRUE),
      .groups = "drop"
    )
}

# --- Row counts (unchanged downstream of correction) -------------------------
unchanged_files <- c(
  "DENV_average_season.csv",
  "full_data_season_monthly_proportions.csv"
)

unchanged_summary <- map_dfr(unchanged_files, function(f) {
  v1 <- read_pipeline_csv(v1_dir, f)
  v2 <- read_pipeline_csv(v2_dir, f)
  tibble(
    file = f,
    n_rows_v1 = nrow(v1),
    n_rows_v2 = nrow(v2),
    identical_row_count = nrow(v1) == nrow(v2)
  )
})

# --- Backfill ----------------------------------------------------------------
backfill_v1 <- read_pipeline_csv(v1_dir, "DENV_cases_backfill_output.csv")
backfill_v2 <- read_pipeline_csv(v2_dir, "DENV_cases_backfill_output.csv")
backfill_cmp <- compare_monthly_cases(backfill_v1, backfill_v2, backfill_key, "backfill")

# --- Nowcast -----------------------------------------------------------------
nowcast_v1 <- read_pipeline_csv(v1_dir, "DENV_cases_nowcast_output.csv")
nowcast_v2 <- read_pipeline_csv(v2_dir, "DENV_cases_nowcast_output.csv")
nowcast_cmp <- compare_monthly_cases(nowcast_v1, nowcast_v2, nowcast_key, "nowcast")

# --- Country tracking --------------------------------------------------------
ct_v1 <- read_pipeline_csv(v1_dir, "country_tracking.csv")
ct_v2 <- read_pipeline_csv(v2_dir, "country_tracking.csv")

step_cols <- grep("^step_", names(ct_v1), value = TRUE)

country_tracking_long <- map_dfr(step_cols, function(col) {
  ct_v1 %>%
    select(iso3, country, value_v1 = all_of(col)) %>%
    inner_join(
      ct_v2 %>% select(iso3, value_v2 = all_of(col)),
      by = "iso3"
    ) %>%
    mutate(step = col, differs = value_v1 != value_v2)
})

country_tracking_diffs <- country_tracking_long %>%
  filter(differs) %>%
  left_join(
    ct_v1 %>% select(iso3, final_status_v1 = final_status),
    by = "iso3"
  ) %>%
  left_join(
    ct_v2 %>% select(iso3, final_status_v2 = final_status),
    by = "iso3"
  )

country_tracking_summary <- country_tracking_long %>%
  group_by(step) %>%
  summarise(n_countries_differ = sum(differs), .groups = "drop") %>%
  arrange(desc(n_countries_differ))

cts_v1 <- read_pipeline_csv(v1_dir, "country_tracking_summary.csv")
cts_v2 <- read_pipeline_csv(v2_dir, "country_tracking_summary.csv")

# --- V2 correction audit (V2 only) -------------------------------------------
audit_dir <- file.path(v2_dir, "inital_rf_correction")
audit_files <- c(
  paho_weekly = "correction_paho_weekly.csv",
  who_monthly = "correction_who_monthly.csv"
)

audit_summary <- if (dir.exists(audit_dir)) {
  imap_dfr(audit_files, function(fname, label) {
    path <- file.path(audit_dir, fname)
    if (!file.exists(path)) {
      return(tibble())
    }
    df <- read_csv(path, show_col_types = FALSE)
  summarise_correction_audit(df, label)
  }) %>%
    bind_rows()
} else {
  tibble()
}

# --- Belize spot-check -------------------------------------------------------
belize_backfill <- backfill_cmp$joined %>%
  filter(iso3 == "BLZ") %>%
  mutate(
    belize_expected = cases_v1 == cases_v2 & cases_v2 == raw_cases
  )

# --- Aggregate summaries -----------------------------------------------------
diff_by_source <- bind_rows(
  backfill_cmp$case_diffs %>% mutate(dataset = "backfill"),
  nowcast_cmp$case_diffs %>% mutate(dataset = "nowcast")
) %>%
  filter(!is.na(cases_v1) & !is.na(cases_v2)) %>%
  group_by(dataset, source) %>%
  summarise(
    n_diffs = n(),
    median_abs_delta = median(abs_delta, na.rm = TRUE),
    sum_abs_delta = sum(abs_delta, na.rm = TRUE),
    .groups = "drop"
  )

diff_by_country <- bind_rows(
  backfill_cmp$case_diffs %>% mutate(dataset = "backfill"),
  nowcast_cmp$case_diffs %>% mutate(dataset = "nowcast")
) %>%
  filter(!is.na(cases_v1) & !is.na(cases_v2)) %>%
  group_by(dataset, iso3, source) %>%
  summarise(
    n_diffs = n(),
    sum_abs_delta = sum(abs_delta, na.rm = TRUE),
    max_abs_delta = max(abs_delta, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(sum_abs_delta))

run_metadata <- bind_rows(
  backfill_cmp$summary,
  nowcast_cmp$summary
) %>%
  mutate(
    run_date = run_date,
    v1_dir = v1_dir,
    v2_dir = v2_dir,
    run_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

# --- Write outputs -----------------------------------------------------------
write_csv(run_metadata, file.path(out_dir, "run_metadata.csv"))
write_csv(unchanged_summary, file.path(out_dir, "unchanged_files_summary.csv"))
write_csv(backfill_cmp$joined, file.path(out_dir, "backfill_comparison.csv"))
write_csv(backfill_cmp$case_diffs, file.path(out_dir, "backfill_case_diffs.csv"))
write_csv(backfill_cmp$source_switches, file.path(out_dir, "backfill_source_switches.csv"))
write_csv(nowcast_cmp$joined, file.path(out_dir, "nowcast_comparison.csv"))
write_csv(nowcast_cmp$case_diffs, file.path(out_dir, "nowcast_case_diffs.csv"))
write_csv(nowcast_cmp$source_switches, file.path(out_dir, "nowcast_source_switches.csv"))
write_csv(country_tracking_diffs, file.path(out_dir, "country_tracking_diffs.csv"))
write_csv(country_tracking_summary, file.path(out_dir, "country_tracking_step_summary.csv"))
write_csv(cts_v1, file.path(out_dir, "country_tracking_summary_v1.csv"))
write_csv(cts_v2, file.path(out_dir, "country_tracking_summary_v2.csv"))
write_csv(audit_summary, file.path(out_dir, "correction_audit_summary.csv"))
write_csv(belize_backfill, file.path(out_dir, "belize_backfill_check.csv"))
write_csv(diff_by_source, file.path(out_dir, "case_diffs_by_source.csv"))
write_csv(diff_by_country, file.path(out_dir, "case_diffs_by_country.csv"))

message("V1 vs V2 validation complete.")
message("Backfill case diffs: ", backfill_cmp$summary$n_case_diffs)
message("Nowcast case diffs: ", nowcast_cmp$summary$n_case_diffs)
message("Source switches (backfill): ", backfill_cmp$summary$n_source_switches)
message("Outputs written to: ", out_dir)
