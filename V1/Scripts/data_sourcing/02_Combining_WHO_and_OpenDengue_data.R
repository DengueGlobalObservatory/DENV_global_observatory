#' ---
#' title: "02_Combining_WHO_and_OpenDengue_data"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
#' Combine WHO and OpenDengue data using years selected in 01_Choosing_WHO_or_OpenDengue_data script. 
#' 
#' Timeline:
#' =========
#' 
#' 24-10-2024: Loaded data. Filtered OD and WHO data for country years from each. Removed countries with only one week of data from OD (2013 - PAHO launch).
#' 24-10-2024: Interpolated OD data, split weeks spanning months then aggregated to monthly. Interpolated WHO data. Combined OD and WHO. 
#' 22-04-2025: Reran script with corrected years to be drawn from WHO and OD datasets for overlapping countries. Corrected filepath to write csv. 
#' 18-05-2025: Updated code with code defined in separate function files. Wrote results to CSV.
#' 19-05-2025: Changed to a 01 script.
#' 19-06-2025: Changed input OpenDengue data to V1.3.
#' 19-07-2025: Added status update.
#' 16-09-2025: Changed function source to a single file for WHO and OD.
#' 23-09-2025: add WHO API

library(httr)
library(jsonlite)
library(dplyr)
library(readxl)



#--------------- Load functions
source("V1/Scripts/data_sourcing/FUNCTIONS/00_OpenDengue_national_data_processing_functions.R")
source("V1/Scripts/data_sourcing/FUNCTIONS/00_WHO_data_processing_functions.R")

if (!exists("log_message")) {
  source("V1/Scripts/utils/logging.R")
  ensure_logger(console = TRUE)
}

if (!exists("record_countries_at_step")) {
  source("V1/Scripts/utils/country_tracking.R")
}

log_message("Running 02_Combining_WHO_and_OpenDengue_data.")

#
#--------------- Filtering OD and WHO datasets for chosen years 

# OpenDengue 
OD_data <- extract_desired_OD_data(OD_national_clean, WHO_OD_coverage)
log_message("Selected OpenDengue records: " %+% nrow(OD_data))

# WHO
WHO_data <- extract_desired_WHO_data(WHO_clean, WHO_OD_coverage)
log_message("Selected WHO records: " %+% nrow(WHO_data))

#--------------- Prepare OD data to join to WHO 

# Deduplicate 
# OD_dedup <- deduplicate_OD_data(OD_data)

# # Interpolate
# OD_interpolated <- OD_dedup %>% 
#   # Remove locations with only one row
#   add_count(ISO_A0, adm_0_name, T_res, name = "Counts") %>% 
#   dplyr::filter(Counts > 1) %>% 
#   dplyr::select(-Counts) %>%
#   interpolate_missing_national_OD_data(.) 

# if(any(grepl("^interpolation", colnames(OD_interpolated)))){
#     stop("Error in OpenDengue interpolation")}

#----- Disaggregating weeks crossing months 
# Allocate cases to respective months, assume equal numbers per day in weeks crossing months.
# OD_weekly_disaggregated <- disaggregate_OD_cases_weeks_crossing_months(OD_data)

# if(any(grepl("^disaggregation", colnames(OD_weekly_disaggregated)))){
#   stop("Error in OpenDengue weekly disaggregation")}

#----- Aggregate weekly --> monthly 
# OD_monthly <- aggregate_weekly_to_monthly_OD_cases(OD_weekly_disaggregated) 

# if(any(grepl("^aggregation", colnames(OD_weekly_disaggregated)))){
  # stop("Error in OpenDengue monthly aggregation")}

#----- Prepare OpenDengue data to combine with WHO data 
OD_monthly_final <- OD_data %>%
  ungroup() %>%
  dplyr::mutate(
    Month = month
    # monthly_cases = dengue_total) 
    )%>%
  arrange(iso3, Year, Month) %>%
  mutate(
    date = lubridate::make_date(year = Year, month = Month, day = 1),
    # cases = monthly_cases,
    country = str_to_title(country), # fix ALL CAP
    Year = year(date)
    ) %>%
  filter(Year > 2009) %>%
  dplyr::select(
    date,
    country,
    iso3,
    cases) %>%
  distinct() 

#--------------- Preparing WHO data to join to OpenDengue 

# #----- Interpolate 
# WHO_interpolated <- interpolate_missing_WHO_data(WHO_clean)
# 
# if(any(grepl("error", colnames(WHO_interpolated)))){
#   stop("Error in WHO interpolation")}

#----- Prepare WHO data to combine with OpenDengue data 
WHO_final <- WHO_interpolated %>% 
  dplyr::select(
    iso3, country, Month, Year, interpolated_cases) %>%
  dplyr::rename(cases = interpolated_cases) %>%
  dplyr::mutate(
    date = lubridate::make_date(year = Year, month = Month, day = 1),
    cases = as.numeric(cases),
    cases = ifelse(cases < 0, NA, cases)) %>%
  dplyr::select(date, country, iso3, cases) 

#--------------- Combine WH0 and OpenDengue data
WHO_OD_combined <- rbind(OD_monthly_final, WHO_final) 
log_message("Combined dataset rows: " %+% nrow(WHO_OD_combined))

#----- Clean combined data 
WHO_OD_combined_clean <- WHO_OD_combined %>% 
  group_by(country, date) %>% 
  
  # Where duplicate entries exist for a single month select the higher value  
  dplyr::mutate(
    Number_of_obs = n(),
    Max_val = (cases == max(cases)) ,
    To_keep = case_when(Number_of_obs == 1 ~ "Keep",
                        Number_of_obs != 1 & Max_val == 1 ~ "Keep",
                        Number_of_obs != 1 & Max_val == 0 ~ "Remove")) %>% 
  dplyr::filter(To_keep == "Keep") %>% 
  dplyr::select(
    !To_keep & !Number_of_obs & !Max_val) %>%
  ungroup()

# Record countries before filtering (Step 3b: Combined Before Filter)
if (exists("record_countries_at_step")) {
  tryCatch({
    record_countries_at_step(WHO_OD_combined_clean, "Step_3b_Combined_Before_Filter")
  }, error = function(e) {
    if (exists("log_message")) {
      log_message("Warning: Country tracking failed at Step 3b Before Filter: " %+% conditionMessage(e), level = "WARNING")
    }
  })
}

#--------------- Filter combined data for complete years, all zero years, and countries with at least 3 years of data.

# Prepare data with Year and Month columns
WHO_OD_combined_clean <- WHO_OD_combined_clean %>%
  ungroup() %>%
  dplyr::mutate(Year = year(date),
         Month = month(date))

# Step 1: Filter for complete years (12 months per year)
full_data_complete_years <- WHO_OD_combined_clean %>%
  group_by(country, Year) %>%
  dplyr::mutate(Number_of_months_in_year = n()) %>%
  dplyr::filter(Number_of_months_in_year == 12) %>%
  dplyr::select(!Number_of_months_in_year) %>%
  ungroup()

# Step 2: Filter for all zero years
full_data_nonzero_years <- full_data_complete_years %>%
  group_by(country, Year) %>%
  dplyr::mutate(All_zeroes = ifelse(sum(cases, na.rm = TRUE) == 0, "Yes", "No")) %>%
  dplyr::filter(All_zeroes == "No") %>%
  dplyr::select(!All_zeroes) %>%
  ungroup()

# Step 3: Filter for locations with at least 3 years of data
full_data <- full_data_nonzero_years %>%
  group_by(country, Month) %>%
  dplyr::mutate(Number_of_years = n()) %>%
  dplyr::filter(Number_of_years >= 3) %>%
  dplyr::select(!Number_of_years) %>%
  ungroup()

#--------------------------- Print status update 

log_message("Filtered combined dataset rows: " %+% nrow(full_data))
log_message("Finished combining OpenDengue and WHO data in 02_Combining_WHO_and_OpenDengue_data script.")

# Determine specific drop reasons for countries that were in before_filter but not after
if (exists("record_countries_at_step")) {
  tryCatch({
    # Get countries before and after each filtering step
    countries_before <- WHO_OD_combined_clean %>%
      dplyr::select(country, iso3) %>%
      dplyr::distinct()
    
    countries_after_complete <- full_data_complete_years %>%
      dplyr::select(country, iso3) %>%
      dplyr::distinct()
    
    countries_after_nonzero <- full_data_nonzero_years %>%
      dplyr::select(country, iso3) %>%
      dplyr::distinct()
    
    countries_after_final <- full_data %>%
      dplyr::select(country, iso3) %>%
      dplyr::distinct()
    
    # Create drop reason mapping - check in order of filtering steps
    # Only include countries that were dropped (not in final)
    dropped_countries <- countries_before %>%
      dplyr::filter(!iso3 %in% countries_after_final$iso3)
    
    if (nrow(dropped_countries) > 0) {
      drop_reasons_df <- dropped_countries %>%
        dplyr::mutate(
          drop_reason = dplyr::case_when(
            # Dropped at complete years step (first filter)
            !iso3 %in% countries_after_complete$iso3 ~ "Filtered: incomplete years (<12 months per year)",
            # Dropped at nonzero years step (second filter) - but passed complete years
            !iso3 %in% countries_after_nonzero$iso3 ~ "Filtered: all-zero years",
            # Dropped at minimum years step (third filter) - but passed previous two
            !iso3 %in% countries_after_final$iso3 ~ "Filtered: <3 years of data",
            # Should not happen, but just in case
            TRUE ~ "Filtered: unknown reason"
          )
        ) %>%
        dplyr::select(iso3, drop_reason)  # Ensure only iso3 and drop_reason columns
      
      # Store for use in tracking
      assign("step3b_drop_reasons", drop_reasons_df, envir = .GlobalEnv)
      
      if (exists("log_message")) {
        log_message("Created drop reasons for " %+% nrow(drop_reasons_df) %+% " countries dropped at step 3b filtering")
      }
    } else {
      # No countries dropped, create empty data frame with correct structure
      assign("step3b_drop_reasons", 
             data.frame(iso3 = character(), drop_reason = character(), stringsAsFactors = FALSE),
             envir = .GlobalEnv)
      
      if (exists("log_message")) {
        log_message("No countries dropped at step 3b filtering")
      }
    }
  }, error = function(e) {
    if (exists("log_message")) {
      log_message("Warning: Could not determine specific drop reasons for step 3b: " %+% conditionMessage(e), level = "WARNING")
    }
    # Create empty data frame on error
    assign("step3b_drop_reasons", 
           data.frame(iso3 = character(), drop_reason = character(), stringsAsFactors = FALSE),
           envir = .GlobalEnv)
  })
}

# Record countries after filtering (Step 3b: Combined After Filter)
if (exists("record_countries_at_step")) {
  tryCatch({
    # Use country-specific drop reasons if available
    if (exists("step3b_drop_reasons") && is.data.frame(step3b_drop_reasons) && nrow(step3b_drop_reasons) > 0) {
      if (exists("log_message")) {
        log_message("Using country-specific drop reasons for " %+% nrow(step3b_drop_reasons) %+% " countries at step 3b")
      }
      record_countries_at_step(full_data, "Step_3b_Combined_After_Filter",
                               drop_reason = step3b_drop_reasons)
      # Clean up
      rm(step3b_drop_reasons, envir = .GlobalEnv)
    } else {
      if (exists("log_message")) {
        log_message("Warning: No step3b_drop_reasons found, using generic drop reason", level = "WARNING")
      }
      # Fallback to generic reason
      record_countries_at_step(full_data, "Step_3b_Combined_After_Filter",
                               drop_reason = "Filtered: incomplete years, all-zero years, or <3 years of data")
    }
  }, error = function(e) {
    if (exists("log_message")) {
      log_message("Warning: Country tracking failed at Step 3b After Filter: " %+% conditionMessage(e), level = "WARNING")
    }
  })
}

#---------------------- Saving 
# write_csv(WHO_OD_combined_final,
#           "Results/02_Combining_WHO_and_OpenDengue_data/OD_WHO_National_data_combined.csv")
