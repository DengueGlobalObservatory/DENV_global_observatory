#' Overview
#' ========
#' 
#' V2 PAHO monthly values
#' 
#' Note: This script is dependent on 01_this_season_dengue_data.R
#' 

library(ggplot2)
library(countrycode)
# Functions: 
source("Scripts/backfilling/FUNCTIONS/00_FUN_paho_data_process.R")

if (!exists("log_message")) {
  source("Scripts/utils/logging.R")
  ensure_logger(console = TRUE)
}

if (!exists("record_countries_at_step")) {
  source("Scripts/utils/country_tracking.R")
}

log_message("Running 02_V2_PAHO_monthly_cases.R")


# move to monthly cases 
# weekly cumm -> month cum -> monthly
paho_month_cumm  <- compute_monthcumm_cases(df = paho)
# Calculate monthly cases:
paho_monthly <-  PAHO_incid_monthly(paho_month_cumm)
log_message("PAHO monthly rows after correction: " %+% nrow(paho_monthly))

# handle negative values 
## -- room for improvement in future

paho_monthly <- paho_monthly %>%
  mutate(
    missing_reason = case_when(computed_monthly_cases_corr < 0 ~ "revised_down", TRUE ~ missing_reason))



#--------------
# Log countries without data for current year
if (exists("log_message")) {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # Get all unique countries in PAHO data
  all_paho_countries <- paho_monthly %>%
    dplyr::select(country) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      iso3 = countrycode::countrycode(country, "country.name", "iso3c")
    ) %>%
    dplyr::filter(!is.na(iso3))
  
  # Get countries with data for current year
  countries_with_current_year <- paho_monthly %>%
    dplyr::filter(year == current_year) %>%
    dplyr::select(country) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      iso3 = countrycode::countrycode(country, "country.name", "iso3c")
    ) %>%
    dplyr::filter(!is.na(iso3))
  
  # Identify countries without current year data
  countries_without_current_year <- all_paho_countries %>%
    dplyr::filter(!iso3 %in% countries_with_current_year$iso3)
  
  log_message("PAHO countries with data for " %+% current_year %+% ": " %+% 
                nrow(countries_with_current_year) %+% "/" %+% nrow(all_paho_countries))
  
  if (nrow(countries_without_current_year) > 0) {
    log_message("Warning: " %+% nrow(countries_without_current_year) %+% 
                  " PAHO countries without data for current year (" %+% current_year %+% "): " %+% 
                  paste(countries_without_current_year$iso3, collapse = ", "), level = "WARNING")
  } else {
    log_message("All PAHO countries have data for current year (" %+% current_year %+% ")")
  }}

# Record countries after PAHO negative value handling (Step 4b: PAHO After Negative Handling)
if (exists("record_countries_at_step")) {
  tryCatch({
    paho_monthly_countries <- paho_monthly %>%
      dplyr::select(country) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        iso3 = countrycode::countrycode(country, "country.name", "iso3c")
      ) %>%
      dplyr::select(country, iso3) %>%
      dplyr::filter(!is.na(iso3))
    
    record_countries_at_step(paho_monthly_countries, "Step_4b_PAHO_After_Negative_Handling")
    log_message("Recorded " %+% nrow(paho_monthly_countries) %+% " countries after PAHO negative value handling")
  }, error = function(e) {
    if (exists("log_message")) {
      log_message("Warning: Country tracking failed at Step 4b PAHO Negative Handling: " %+% conditionMessage(e), level = "WARNING")
    }
  })
}


