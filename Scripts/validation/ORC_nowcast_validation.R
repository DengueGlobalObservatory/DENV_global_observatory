# =============================================================================
# ORC_nowcast_validation.R
# Orchestrator — run the full retrospective validation workflow in order
# =============================================================================
# From the repository root:
#   Rscript Scripts/validation/ORC_nowcast_validation.R
#
# Requires `full_data_season_monthly_proportions.csv` in the latest dated
# `Output/YYYY_MM_DD/` folder (produced by Scripts/V1_Pipeline.R Step 8).
# =============================================================================

message("--- 1/4 Individual validation (LOSO detail) ---")
source("Scripts/validation/03_nowcast_validation_ind.R")

message("--- 2/4 Summary tables, quantiles, calibrated lookup, coverage ---")
source("Scripts/validation/03_nowcast_validation_summary.R")

message("--- 3/4 Snapshot convergence ---")
source("Scripts/validation/03_nowcast_validation_snapshots.R")

message("--- 4/4 Figures ---")
source("Scripts/validation/04_nowcast_validation_FIG.R")

message("ORC_nowcast_validation.R finished.")
