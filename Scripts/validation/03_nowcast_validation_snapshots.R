# =============================================================================
# 03_nowcast_validation_snapshots.R
# Estimate vs observed — pipeline snapshots when counts later appear
# =============================================================================
# Uses ~2 Output/YYYY_MM_DD runs per calendar month (week 1 and week 3-ish),
# keeps country–calendar-month rows that were Estimates in those runs, and
# matches each to the first Observed count in a later snapshot.
# Output: Output/validation/estimate_vs_observed.csv
# =============================================================================

library(tidyverse)

out_dir <- "Output/validation"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

out_path <- file.path(out_dir, "estimate_vs_observed.csv")
read_cols <- c("iso3", "country", "Country", "Year", "Month", "cases", "source", "Data_status")

# --- Discover dated pipeline output folders ----------------------------------
snap_meta <- list.dirs("Output", recursive = FALSE) %>%
  keep(~ grepl("^[0-9]{4}_[0-9]{2}_[0-9]{2}$", basename(.x))) %>%
  tibble(snapshot_dir = .) %>%
  mutate(
    snapshot_date = basename(snapshot_dir),
    snapshot_dt = as.Date(str_replace_all(snapshot_date, "_", "-")),
    run_month = format(snapshot_dt, "%Y-%m"),
    run_day = day(snapshot_dt)
  ) %>%
  arrange(snapshot_dt)

if (nrow(snap_meta) == 0) {
  warning("No dated Output folders found; writing empty estimate_vs_observed.csv.")
  write_csv(tibble(), out_path)
} else {

pick_week_snap <- function(days) {
  w1 <- days[days <= 7]
  w3 <- days[days >= 15 & days <= 21]
  list(
    week1 = if (length(w1)) min(w1) else min(days),
    week3 = if (length(w3)) min(w3) else days[which.min(abs(days - 18))]
  )
}

selected_snaps <- snap_meta %>%
  group_by(run_month) %>%
  group_modify(~ {
    picked <- pick_week_snap(.x$run_day)
    bind_rows(
      .x %>% filter(run_day == picked$week1) %>% slice_head(n = 1) %>% mutate(estimate_timing = "week1"),
      .x %>% filter(run_day == picked$week3) %>% slice_head(n = 1) %>% mutate(estimate_timing = "week3")
    )
  }) %>%
  ungroup() %>%
  distinct(snapshot_dir, snapshot_date, snapshot_dt, run_month, estimate_timing)

load_nowcast_rows <- function(d, keep) {
  f <- file.path(d, "DENV_cases_nowcast_output.csv")
  if (!file.exists(f)) {
    return(tibble())
  }
  raw <- read_csv(f, show_col_types = FALSE, col_select = any_of(read_cols))
  country_col <- if ("country" %in% names(raw)) "country" else "Country"
  raw %>%
    filter(
      if (keep == "estimate") {
        source == "Estimates"
      } else {
        Data_status == "Observed"
      }
    ) %>%
    transmute(
      snapshot_dir = d,
      snapshot_date = basename(d),
      snapshot_dt = as.Date(str_replace_all(snapshot_date, "_", "-")),
      iso3,
      country = .data[[country_col]],
      Year,
      Month,
      cases = cases,
      source = source
    )
}

# Estimates from ~2 pipeline runs per calendar month
estimates <- map_dfr(
  selected_snaps$snapshot_dir,
  load_nowcast_rows,
  keep = "estimate"
) %>%
  mutate(
    estimate_snapshot_date = snapshot_date,
    estimate_snapshot_dt = snapshot_dt,
    cases_estimate = cases
  ) %>%
  inner_join(
    selected_snaps %>% dplyr::select(snapshot_dir, run_month, estimate_timing),
    by = "snapshot_dir"
  ) %>%
  dplyr::select(-snapshot_dir, -source, -snapshot_date, -snapshot_dt, -cases)

if (nrow(estimates) == 0) {
  warning("No estimate rows in selected snapshots; writing empty CSV.")
  write_csv(tibble(), out_path)
} else {

# Observed rows from all snapshots (needed to find first report after each estimate)
observed_all <- map_dfr(snap_meta$snapshot_dir, load_nowcast_rows, keep = "observed") %>%
  filter(!is.na(cases)) %>%
  dplyr::select(-country)

if (nrow(observed_all) == 0) {
  warning("No observed rows in any snapshot; writing empty CSV.")
  write_csv(tibble(), out_path)
} else {

estimate_vs_observed <- estimates %>%
  inner_join(observed_all, by = c("iso3", "Year", "Month"), relationship = "many-to-many") %>%
  filter(snapshot_dt > estimate_snapshot_dt) %>%
  group_by(
    iso3, country, Year, Month,
    estimate_snapshot_date, estimate_snapshot_dt,
    run_month, estimate_timing, cases_estimate
  ) %>%
  dplyr::arrange(snapshot_dt, .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  dplyr::rename(
    first_observed_snapshot_date = snapshot_date,
    first_observed_snapshot_dt = snapshot_dt,
    cases_observed = cases,
    observed_source = source
  ) %>%
  dplyr::select(-snapshot_dir)

# Latest observed count (any snapshot) as a reference if values revise
latest_observed <- observed_all %>%
  group_by(iso3, Year, Month) %>%
  dplyr::arrange(desc(snapshot_dt), .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  transmute(
    iso3,
    Year,
    Month,
    latest_observed_snapshot_date = snapshot_date,
    cases_observed_latest = cases
  )

estimate_vs_observed <- estimate_vs_observed %>%
  left_join(latest_observed, by = c("iso3", "Year", "Month")) %>%
  mutate(
    target_ym = sprintf("%04d-%02d", Year, Month),
    error_signed = cases_observed - cases_estimate,
    error_abs = abs(error_signed),
    error_rel = if_else(
      is.finite(cases_observed) & cases_observed != 0,
      error_signed / cases_observed,
      NA_real_
    ),
    days_estimate_to_observed = as.integer(
      first_observed_snapshot_dt - estimate_snapshot_dt
    )
  ) %>%
  dplyr::arrange(country, target_ym, estimate_snapshot_dt, estimate_timing)

write_csv(estimate_vs_observed, out_path)
n_country_months <- estimate_vs_observed %>%
  distinct(iso3, Year, Month) %>%
  nrow()
message(
  "Wrote ", nrow(estimate_vs_observed),
  " estimate-vs-observed rows (",
  n_country_months,
  " country–months) to ", out_path
)

}

}

}
