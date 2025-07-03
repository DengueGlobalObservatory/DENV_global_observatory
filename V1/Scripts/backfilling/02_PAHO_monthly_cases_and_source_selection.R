#' ---
#' title: "02_PAHO_monthly_cases_and_source_selection"
#' author: "K M Susong"
#' 
#' ---
#'
#' Overview
#' ========
#' 
#' Note: This script is dependent on 01_dengue_data_update.R

# Functions: 
source("V1/Scripts/backfilling/FUNCTIONS/00_FUN_paho_data_process.R")

# ----- PAHO backfilling and monthly case calculation 

# apply backfilling and define monthly cumulative cases 
paho_monthly_cumm <- PAHO_cumm_monthly(paho)

# Calculate monthly cases:
paho_month <- PAHO_incid_monthly(paho_monthly_cumm)


# ----- Selection of data sources for each country


# ------ Save output df 

