#' ---
#' title: "V1_Pipeline"
#' author: "K M Susong"
#' 
#' ---
#'

library(dplyr)
library(lubridate)
library(purrr)

#------ Step 1: start log

# create run directory 
run_dir <- paste0("V1/Output/",format(Sys.Date(), "%Y_%m_%d"))
                  
# Create run directory safely
if (!dir.exists(run_dir)) {
  dir.create(run_dir, recursive = TRUE)
  cat("Created run directory:", run_dir, "\n")
} else {
  cat("Run directory already exists:", run_dir, "\n")
}


# Define log file path
log_file <- file.path(run_dir, paste0("pipeline_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))

# ---- Start Log Process ----
# Open a single connection
tryCatch({
  log_con <- file(log_file, open = "wt")
  sink(log_con, type = "output", split = TRUE)
  sink(log_con, type = "message")
}, error = function(e) {
  stop("Failed to create or open log file: ", conditionMessage(e))
})

# logging helper functions ------
log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s: %s\n", timestamp, level, msg))
}

`%+%` <- function(a, b) paste0(a, b)


options(
  warning.expression = quote(log_message(geterrmessage(), level = "WARNING")),
  error = function(e) {
    log_message(paste("ERROR:", conditionMessage(e)), level = "ERROR")
    stop(e)
  }
)

# -------


cat("=============================================\n")
log_message("Pipeline initialized.")
log_message("Run directory: " %+% run_dir)
log_message("R version: " %+% R.version.string)
log_message("Working directory: " %+% getwd())
cat("=============================================\n\n")

#------ Step 2: Open Data

log_message("Step 2: Opening dengue-related data sources...")

## this opens all opendengue, paho, searo and who data
source("V1/Scripts/data_sourcing/01_dengue_data.R")

# Check that expected objects were created (adjust names as needed)
expected_objects <- c("OD_national", "paho", "searo", "who")
missing_objs <- expected_objects[!expected_objects %in% ls()]

if (length(missing_objs) > 0) {
  log_message(paste("Missing expected data objects:", paste(missing_objs, collapse = ", ")), level = "ERROR")
  stop("Critical data objects missing: ", paste(missing_objs, collapse = ", "))
} else {
  log_message("All expected data objects are present.")
}

# # log the countries in each df
# country_log <- tibble(country = character())
# # add existing dfs
# country_log <- country_log |>
#   add_country_step(OD_national, "OD_national") |>
#   add_country_step(paho,       "paho") |>
#   add_country_step(searo,      "searo") |>
#   add_country_step(who,        "who")
# 
# country_log <- add_country_step(country_log, OD_national, "OD_national")


# ----- Step 3: determine historic average and baseline seasonality

log_message( "Step 3: selected historic data")

# Select the source of historic data
source("V1/Scripts/data_sourcing/01_Choosing_WHO_or_OpenDengue_data.R")

log_message( "Step 3: combine data sources")

# Define the combination of WHO and Opendengue data for historical data
source("V1/Scripts/data_sourcing/02_Combining_WHO_and_OpenDengue_data.R")

log_message( "Step 3: define average season")

# calculate average season 
source("V1/Scripts/seasonal_baseline/02_identify_seasonal_baseline.R") 


#------ Step 4: Data selection and Backfilling  
log_message( "Step 4: select current season data and backfill")


source("V1/Scripts/backfilling/02_PAHO_monthly_cases_and_source_selection.R")

#------ Step 5: Nowcasting to Date 
log_message( "Step 5: Nowcast missing recent months")


source("V1/Scripts/nowcasting/03_proportion_nowcast.R")


#------ Step 6: Post-processing 
log_message( "Step 6: post processing")


# ---- Regional monthly summary ----

# add region back in:

region_map <- OD_national %>%
  mutate(
    country = str_to_title(country) # fix ALL CAP
  ) %>%
  dplyr::select(country, Region) %>%
  unique()

data <- merge(data,region_map)

region_levels <- c(
  "South America",
  "Caribbean",
  "Pacific Islands",
  "South Asia",
  "Central America & Mexico",
  "Sub-Saharan Africa",
  "East & Southeast Asia",
  "Europe, Middle East & North Africa"
)

data <- data %>%
  mutate(Region = factor(Region, levels = region_levels))


#  Regional Summary 

region_summary <- data %>%
  group_by(Region, Year, Month) %>%
  reframe(
    cases = sum(cases, na.rm = TRUE),
    Ave_season_monthly_cases = sum(Ave_season_monthly_cases, na.rm = TRUE),
    Ave_season_monthly_cum_cases = sum(Ave_season_monthly_cum_cases, na.rm = TRUE),
    Predicted_total_seasonal_cases = sum(Predicted_total_seasonal_cases, na.rm = TRUE),
    # Optionally include averages of proportions or percentiles if needed
    Ave_cum_monthly_proportion = mean(Ave_cum_monthly_proportion, na.rm = TRUE),
    Ave_monthly_proportion = mean(Ave_monthly_proportion, na.rm = TRUE),
    percentile_most_recent = mean(percentile_most_recent, na.rm = TRUE),
    n_countries = n_distinct(iso3)
  ) %>%
  ungroup() %>%
  mutate(
    date = as.Date(paste0(Year, "-", Month, "-01")))

#----- Step 7: Save outputs 
# ------ Save output df 


write_csv(current_data, file = paste0("V1/Output/", format(Sys.Date(), "%Y_%m_%d"),"/DENV_cases_backfill_output.csv"))
write.csv(data, file = paste0("V1/Output/", format(Sys.Date(), "%Y_%m_%d"),"/DENV_cases_nowcast_output.csv"))
write.csv(full_data_average_season, file = paste0("V1/Output/", format(Sys.Date(), "%Y_%m_%d"),"/DENV_average_season.csv"))


# ----- close log ----

on.exit({
  log_message("Pipeline run completed.")
  sink(type = "message")
  sink(type = "output")
  close(log_con)
})