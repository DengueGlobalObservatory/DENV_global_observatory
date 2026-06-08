#!/usr/bin/env Rscript

# Paper missing-data summary for the GDO descriptive manuscript.
#
# Fills the "missing in the data" prose with values computed from the most
# recent local Output/<date>/DENV_cases_nowcast_output.csv (loaded via the
# same V1_Dashboard_setup.R that local Quarto preview uses) and prints two
# manuscript tables to the console. No CSV files are written.
#
# Universe: countries with a radial plot in the live dashboard
# (`names(all_country_plots)`), restricted to current-year months 1..recent_month.
#
# "Missing" definitions reported in parallel (per the plan):
#   - Primary  : source == "Estimates"     (nowcast-filled months)
#   - Secondary: Data_status == "Unobserved" (broader; printed for comparison)
#
# Backfill is intentionally out of scope.
#
# Usage (from project root):
#   Rscript Scripts/nowcasting/04_gdo_paper_missing_data_summary.R
#   Rscript Scripts/nowcasting/04_gdo_paper_missing_data_summary.R Output/2026_06_01

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# ---- Locate project root ---------------------------------------------------
if (!file.exists("Scripts/V1_Dashboard_setup.R")) {
  # Allow running from Scripts/nowcasting/
  if (file.exists("../../Scripts/V1_Dashboard_setup.R")) {
    setwd("../..")
  }
}
if (!file.exists("Scripts/V1_Dashboard_setup.R")) {
  stop("Run from project root (cannot find Scripts/V1_Dashboard_setup.R).")
}

# ---- Optional CLI: pin a specific Output snapshot --------------------------
cli_args <- commandArgs(trailingOnly = TRUE)
pinned_dir <- if (length(cli_args) >= 1 && nzchar(cli_args[1])) cli_args[1] else NA_character_

if (!is.na(pinned_dir)) {
  pinned_file <- file.path(pinned_dir, "DENV_cases_nowcast_output.csv")
  if (!file.exists(pinned_file)) {
    stop("Pinned snapshot not found: ", pinned_file)
  }
  message("Pinning snapshot: ", pinned_file)
}

# ---- Load dashboard data (matches local Quarto preview) --------------------
# The setup script prints plot-construction debug messages via cat(); silence
# them so the summary output is readable.
invisible(capture.output(
  suppressMessages(suppressWarnings(source("Scripts/V1_Dashboard_setup.R"))),
  type = "output"
))

if (!exists("data") || !exists("all_country_plots") ||
    !exists("current_year") || !exists("recent_month")) {
  stop(
    "V1_Dashboard_setup.R did not produce expected objects ",
    "(data, all_country_plots, current_year, recent_month)."
  )
}

if (!is.na(pinned_dir)) {
  data <- read.csv(file.path(pinned_dir, "DENV_cases_nowcast_output.csv"),
                   check.names = FALSE)
  col_names <- names(data)
  if (length(col_names) > 0 &&
      (col_names[1] == "" || col_names[1] == "X" || col_names[1] == "X.")) {
    data <- data %>% dplyr::select(-1)
  }
  # Apply the same future-month masking as the dashboard setup.
  data <- data %>%
    dplyr::mutate(
      is_future = (Year > current_year) | (Year == current_year & Month > recent_month),
      cases = dplyr::if_else(is_future, NA_real_, cases)
    ) %>%
    dplyr::select(-is_future)
}


# ---- Pick country/region columns -------------------------------------------
country_col <- if ("country" %in% names(data)) "country" else "Country"
region_col  <- "Region"

# ---- Country universe: radial plot list (live, ~88) ------------------------
radial_countries <- names(all_country_plots)
radial_countries <- radial_countries[!is.na(radial_countries) & nzchar(radial_countries)]

n_radial_countries <- length(radial_countries)

# Also compute the country-config "complete data" tier (~81) for the prose note.
country_cfg_path <- "pages/country/country-config.csv"
n_complete_data <- if (file.exists(country_cfg_path)) {
  cfg <- read.csv(country_cfg_path, check.names = FALSE, stringsAsFactors = FALSE)
  sum(tolower(as.character(cfg$enabled)) == "true", na.rm = TRUE)
} else {
  NA_integer_
}

# Paper table universe: the radial-plot country list (83 in your example).
# This ensures the manuscript denominator matches the dashboard “All Countries”
# panel and the expected 83 × 5 = 415 country-months layout.
paper_countries <- radial_countries
n_paper_countries <- length(paper_countries)

# ---- Build country-month fact table ----------------------------------------
# One row per radial country x current-year month (1..recent_month).
ytd <- data %>%
  dplyr::filter(Year == current_year, Month >= 1, Month <= recent_month) %>%
  dplyr::filter(.data[[country_col]] %in% radial_countries) %>%
  dplyr::mutate(
    country = .data[[country_col]],
    region  = .data[[region_col]],
    is_missing_estimates  = !is.na(source) & source == "Estimates",
    is_missing_unobserved = !is.na(Data_status) & Data_status == "Unobserved"
  ) %>%
  dplyr::select(country, region, iso3, Year, Month, cases, source, Data_status,
                is_missing_estimates, is_missing_unobserved)

# Sanity check on shape
n_country_months <- nrow(ytd)
expected_n <- n_radial_countries * recent_month

# Country-level aggregates
country_totals <- ytd %>%
  dplyr::group_by(country, region) %>%
  dplyr::summarise(
    n_missing_months = sum(is_missing_estimates),
    last_observed_month = suppressWarnings(max(Month[!is_missing_estimates], na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    last_observed_month = dplyr::if_else(
      is.finite(last_observed_month),
      suppressWarnings(as.integer(last_observed_month)),
      NA_integer_
    ),
    last_observed_label = dplyr::if_else(
      is.na(last_observed_month), NA_character_,
      month.abb[dplyr::coalesce(last_observed_month, 1L)]
    )
  )

# Attach delay_months (months since last observed) to missing rows.
# - Trailing delay (Month > last_observed)            -> Month - last_observed
# - No observed month yet                             -> Month (distance from Jan)
# - Internal gap (Month <= last_observed)             -> NA, flagged separately
ytd <- ytd %>%
  dplyr::left_join(country_totals %>% dplyr::select(country, last_observed_month),
                   by = "country") %>%
  dplyr::mutate(
    delay_type = dplyr::case_when(
      !is_missing_estimates ~ NA_character_,
      is.na(last_observed_month) ~ "no_observed",
      Month > last_observed_month ~ "trailing",
      TRUE ~ "internal_gap"
    ),
    delay_months = dplyr::case_when(
      delay_type == "trailing"    ~ as.integer(Month - last_observed_month),
      delay_type == "no_observed" ~ as.integer(Month),
      TRUE ~ NA_integer_
    )
  )

# ---- Headline counts -------------------------------------------------------
n_missing_est   <- sum(ytd$is_missing_estimates)
n_missing_unobs <- sum(ytd$is_missing_unobserved)

countries_with_missing <- country_totals %>%
  dplyr::filter(n_missing_months > 0)
n_countries_with_missing <- nrow(countries_with_missing)

regions_with_missing <- countries_with_missing %>%
  dplyr::filter(!is.na(region) & nzchar(region)) %>%
  dplyr::distinct(region)
n_regions_total <- ytd %>%
  dplyr::filter(!is.na(region) & nzchar(region)) %>%
  dplyr::distinct(region) %>% nrow()
n_regions_with_missing <- nrow(regions_with_missing)

# Persistent missing: countries with >= 4 missing months
persistent <- country_totals %>%
  dplyr::filter(n_missing_months >= 4) %>%
  dplyr::arrange(dplyr::desc(n_missing_months), country)

n_persistent <- nrow(persistent)

# ---- Table 1: missing country-months by region -----------------------------
table_missing_by_region <- ytd %>%
  dplyr::filter(!is.na(region) & nzchar(region)) %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(
    n_country_months = dplyr::n(),
    n_missing = sum(is_missing_estimates),
    pct_missing = round(100 * n_missing / dplyr::n(), 1),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(pct_missing))

# Append an "All radial countries" total row for convenience.
table_missing_by_region <- dplyr::bind_rows(
  table_missing_by_region,
  tibble::tibble(
    region = "All (radial)",
    n_country_months = n_country_months,
    n_missing = n_missing_est,
    pct_missing = round(100 * n_missing_est / n_country_months, 1)
  )
)

# ---- Table 2: delay distribution among missing country-months --------------
# Trailing + no_observed only; internal-gap missing rows reported separately.
delay_rows <- ytd %>%
  dplyr::filter(is_missing_estimates,
                delay_type %in% c("trailing", "no_observed"),
                !is.na(delay_months))

n_internal_gap <- sum(ytd$delay_type == "internal_gap", na.rm = TRUE)
n_delay_denom  <- nrow(delay_rows)

delay_buckets <- delay_rows %>%
  dplyr::mutate(
    bucket = dplyr::if_else(delay_months >= 4, "4+", as.character(delay_months)),
    bucket = factor(bucket, levels = c("1", "2", "3", "4+"))
  ) %>%
  dplyr::count(bucket, name = "n", .drop = FALSE) %>%
  dplyr::mutate(pct = if (n_delay_denom > 0) round(100 * n / n_delay_denom, 1) else 0) %>%
  dplyr::arrange(bucket)

# ---- Table 2b: region × delay-bucket (for manuscript table) ----------------
# This uses the paper universe (radial plots) and a full country×month grid so
# total country-months = n_countries * recent_month.

# Region lookup for paper countries (stable, one per country in most cases).
country_region_lookup <- data %>%
  dplyr::mutate(country = .data[[country_col]], region = .data[[region_col]]) %>%
  dplyr::filter(!is.na(country) & nzchar(country)) %>%
  dplyr::distinct(country, region)

facts_grid <- tidyr::expand_grid(
  country = paper_countries,
  Month = seq_len(recent_month)
) %>%
  dplyr::left_join(country_region_lookup, by = "country") %>%
  dplyr::left_join(
    ytd %>%
      dplyr::select(country, Month, source, is_missing_estimates, delay_type, delay_months),
    by = c("country", "Month")
  ) %>%
  dplyr::mutate(
    is_missing_estimates = dplyr::coalesce(is_missing_estimates, FALSE),
    delay_bucket = dplyr::case_when(
      !is_missing_estimates ~ NA_character_,
      delay_type %in% c("trailing", "no_observed") & !is.na(delay_months) & delay_months >= 4 ~ "4+",
      delay_type %in% c("trailing", "no_observed") & !is.na(delay_months) ~ as.character(delay_months),
      TRUE ~ NA_character_
    ),
    delay_bucket = factor(delay_bucket, levels = c("1", "2", "3", "4+"))
  )

table_missing_by_region_delay <- facts_grid %>%
  dplyr::filter(!is.na(region) & nzchar(region)) %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(
    n_countries = dplyr::n_distinct(country),
    total_country_month = dplyr::n(),
    `1_month_out_of_date`  = sum(delay_bucket == "1", na.rm = TRUE),
    `2_month_out_of_date`  = sum(delay_bucket == "2", na.rm = TRUE),
    `3_month_out_of_date`  = sum(delay_bucket == "3", na.rm = TRUE),
    `4plus_month_out_of_date` = sum(delay_bucket == "4+", na.rm = TRUE),
    total_missing = sum(is_missing_estimates, na.rm = TRUE),
    pct_missing = round(100 * total_missing / dplyr::n(), 1),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(pct_missing))

# All row (paper universe)
table_missing_by_region_delay <- dplyr::bind_rows(
  table_missing_by_region_delay,
  tibble::tibble(
    region = "All",
    n_countries = n_paper_countries,
    total_country_month = n_paper_countries * recent_month,
    `1_month_out_of_date`  = sum(facts_grid$delay_bucket == "1", na.rm = TRUE),
    `2_month_out_of_date`  = sum(facts_grid$delay_bucket == "2", na.rm = TRUE),
    `3_month_out_of_date`  = sum(facts_grid$delay_bucket == "3", na.rm = TRUE),
    `4plus_month_out_of_date` = sum(facts_grid$delay_bucket == "4+", na.rm = TRUE),
    total_missing = sum(facts_grid$is_missing_estimates, na.rm = TRUE),
    pct_missing = round(100 * sum(facts_grid$is_missing_estimates, na.rm = TRUE) /
                          (n_paper_countries * recent_month), 1)
  )
)

# Also: country-level missing-months distribution (1, 2, 3, 4+).
country_missing_distribution <- country_totals %>%
  dplyr::filter(n_missing_months > 0) %>%
  dplyr::mutate(
    bucket = dplyr::case_when(
      n_missing_months >= 4 ~ "4+",
      TRUE                  ~ as.character(n_missing_months)
    ),
    bucket = factor(bucket, levels = c("1", "2", "3", "4+"))
  ) %>%
  dplyr::count(bucket, name = "n_countries") %>%
  dplyr::mutate(pct_of_radial = round(100 * n_countries / n_radial_countries, 1)) %>%
  dplyr::arrange(bucket)

# ---- Print results ---------------------------------------------------------
as_of_label <- format(Sys.Date(), "%d-%B-%Y")
folder_label <- if (!is.na(pinned_dir)) pinned_dir else
  tryCatch(get_latest_dataset_info()$dir, error = function(e) NA_character_)

cat("\n")
cat("==============================================================\n")
cat(" GDO paper missing-data summary\n")
cat("==============================================================\n")
cat(sprintf(" As of            : %s\n", as_of_label))

cat(sprintf(" Snapshot folder  : %s\n",
            if (is.null(folder_label) || is.na(folder_label)) "NA" else folder_label))
cat(sprintf(" Current year     : %d\n", current_year))
cat(sprintf(" Months included  : 1..%d (recent_month)\n", recent_month))
cat(sprintf(" Radial countries : %d (denominator)\n", n_radial_countries))
cat(sprintf(" Paper countries (radial): %d\n", n_paper_countries))
cat(sprintf(" Complete-data tier (country-config, for note): %s\n",
            ifelse(is.na(n_complete_data), "NA", as.character(n_complete_data))))
cat(sprintf(" Country-months   : %d (observed shape vs expected %d)\n",
            n_country_months, expected_n))
cat("\n")
cat(" Missing definition tally (current-year YTD, radial universe):\n")
cat(sprintf("   source == 'Estimates'         : %d / %d  (%.1f%%)  [PRIMARY]\n",
            n_missing_est, n_country_months,
            100 * n_missing_est / n_country_months))
cat(sprintf("   Data_status == 'Unobserved'   : %d / %d  (%.1f%%)\n",
            n_missing_unobs, n_country_months,
            100 * n_missing_unobs / n_country_months))
cat("\n")

# ---- Narrative block -------------------------------------------------------
pct <- function(num, den) {
  if (is.na(den) || den == 0) return("NA")
  sprintf("%.1f%%", 100 * num / den)
}

persistent_list <- if (nrow(persistent) > 0) {
  paste(persistent$country, collapse = ", ")
} else {
  "(none)"
}

# delay bucket helpers (defensive: factor comparisons with NA buckets can leak
# NA positions when subsetting, so use which()).
delay_n <- function(b) {
  v <- delay_buckets$n[which(as.character(delay_buckets$bucket) == b)]
  if (length(v) == 0) 0L else as.integer(v[1])
}

bucket_phrase <- function(b) {
  n <- delay_n(b)
  sprintf("%s (n=%d)", pct(n, n_delay_denom), n)
}

narrative <- sprintf(
"As of %s, the GDO includes %d months for these %d countries, resulting in %d
individual country-month observations. Of the %d country-months, %d (%s) had
missing data (see table by region below). Country-months with missing data were
found in %d (%s) countries and %d (%s) regions. Among the %d trailing missing
country-months (i.e. months after each country's last observed month),
%s were a single month out-of-date, %s were 2 months out-of-date, %s were
3 months out-of-date, and %s were 4 or more months out-of-date in [%s]. This is
%s of countries in the observatory with persistent (>=4 month) missing data.
(An additional %d missing country-months are internal gaps before the country's
last observed month and are excluded from the delay distribution.)",
  as_of_label,
  recent_month, n_radial_countries, n_country_months,
  n_country_months, n_missing_est, pct(n_missing_est, n_country_months),
  n_countries_with_missing, pct(n_countries_with_missing, n_radial_countries),
  n_regions_with_missing,   pct(n_regions_with_missing, n_regions_total),
  n_delay_denom,
  bucket_phrase("1"),
  bucket_phrase("2"),
  bucket_phrase("3"),
  bucket_phrase("4+"),
  persistent_list,
  pct(n_persistent, n_radial_countries),
  n_internal_gap
)

cat("---- Narrative (fill-ins) ------------------------------------\n")
cat(narrative, "\n\n")

cat("---- Table 1: missing country-months by region ----------------\n")
print(as.data.frame(table_missing_by_region), row.names = FALSE)
cat("\n")

cat("---- Table 2: delay distribution among missing country-months -\n")
cat(sprintf(
  "(trailing + no-observed only; denominator = %d of %d missing country-months;\n internal gaps excluded = %d)\n",
  n_delay_denom, n_missing_est, n_internal_gap))
print(as.data.frame(delay_buckets), row.names = FALSE)
cat("\n")

cat("---- Table 2b: missing by region with delay buckets (paper) ---\n")
cat(sprintf(
  "(denominator = radial countries; total country-months = n_countries * %d)\n",
  recent_month
))
print(as.data.frame(table_missing_by_region_delay), row.names = FALSE)
cat("\n")

cat("---- Table 3: countries by number of missing months -----------\n")
cat(sprintf("(denominator = %d radial countries)\n", n_radial_countries))
print(as.data.frame(country_missing_distribution), row.names = FALSE)
cat("\n")

cat("---- Countries with >=4 missing months (persistent) -----------\n")
if (nrow(persistent) > 0) {
  print(
    as.data.frame(
      persistent %>%
        dplyr::mutate(
          last_observed = dplyr::if_else(is.na(last_observed_label),
                                         "(none in current year)",
                                         last_observed_label)
        ) %>%
        dplyr::select(country, region, n_missing_months, last_observed)
    ),
    row.names = FALSE
  )
} else {
  cat("(none)\n")
}
cat("\n")

# Make the key tables available in the interactive workspace too.
invisible(list(
  table_missing_by_region = table_missing_by_region,
  delay_buckets = delay_buckets,
  country_missing_distribution = country_missing_distribution,
  persistent = persistent,
  country_totals = country_totals,
  ytd = ytd
))
