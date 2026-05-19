# =============================================================================
# 03_nowcast_validation_snapshots.R
# Snapshot convergence — how “final” nowcasts differ across pipeline runs
# =============================================================================
# Scans Output/YYYY_MM_DD/ for DENV_cases_nowcast_output.csv, keeps rows that
# represent estimated (nowcast) unobserved months, and compares each snapshot
# to the latest available snapshot for the same country–calendar month.
# Outputs:
#   snapshot_convergence_detail.csv
#   snapshot_convergence_summary.csv
# =============================================================================

library(tidyverse)

out_dir <- "Output/validation"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

# --- Discover dated pipeline output folders ----------------------------------
snap_dirs <- list.dirs("Output", recursive = FALSE) %>%
  keep(~ str_detect(basename(.x), "^\\d{4}_\\d{2}_\\d{2}$")) %>%
  sort()

# --- Load one snapshot file (nowcast estimates only) -------------------------
load_snap <- function(d) {
  f <- file.path(d, "DENV_cases_nowcast_output.csv")
  if (!file.exists(f)) {
    return(tibble())
  }
  read_csv(f, show_col_types = FALSE) %>%
    filter(Data_status == "Unobserved", source == "Estimates") %>%
    dplyr::transmute(
      snapshot_date = basename(d),
      iso3,
      country,
      Year,
      Month,
      cases_nowcast = cases
    )
}

# --- Stack all snapshots ------------------------------------------------------
snap <- map_dfr(snap_dirs, load_snap)

if (nrow(snap) == 0) {
  warning("No snapshot nowcast rows found; writing empty detail/summary CSVs.")
  write_csv(tibble(), file.path(out_dir, "snapshot_convergence_detail.csv"))
  write_csv(tibble(), file.path(out_dir, "snapshot_convergence_summary.csv"))
} else {

# --- “Final” reference = latest snapshot per country–month -------------------
# snapshot_date strings sort lexicographically in chronological order for YYYY_MM_DD.
final <- snap %>%
  group_by(iso3, Year, Month) %>%
  dplyr::arrange(desc(snapshot_date), .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  dplyr::select(iso3, Year, Month, final_nowcast = cases_nowcast)

# --- Per snapshot: absolute distance to final ---------------------------------
detail <- snap %>%
  left_join(final, by = c("iso3", "Year", "Month")) %>%
  mutate(abs_diff_to_final = abs(cases_nowcast - final_nowcast))

# --- Per country–month: revision path + stabilisation heuristic --------------
# stabilised_snapshot = first snapshot date where |nowcast - final| <= 1 case
summary <- detail %>%
  group_by(iso3, country, Year, Month) %>%
  dplyr::arrange(snapshot_date, .by_group = TRUE) %>%
  dplyr::summarise(
    n_snapshots = n(),
    first_snapshot = dplyr::first(snapshot_date),
    last_snapshot = dplyr::last(snapshot_date),
    final_nowcast = dplyr::last(final_nowcast),
    first_abs_diff = dplyr::first(abs_diff_to_final),
    last_abs_diff = dplyr::last(abs_diff_to_final),
    stabilised_snapshot = dplyr::first(snapshot_date[abs_diff_to_final <= 1]),
    .groups = "drop"
  )

write_csv(detail, file.path(out_dir, "snapshot_convergence_detail.csv"))
write_csv(summary, file.path(out_dir, "snapshot_convergence_summary.csv"))

message("Wrote snapshot convergence CSVs to ", out_dir)
}
