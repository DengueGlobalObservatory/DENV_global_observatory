# ----- Step 3: determine historic average and baseline seasonality

log_message("Step 3: selecting historic data (simplified: OD_national preferred, WHO as fallback)")

# Load required functions for data processing
source("Scripts/data_sourcing/FUNCTIONS/00_WHO_data_processing_functions.R")

if (!exists("log_message")) {
  source("Scripts/utils/logging.R")
  ensure_logger(console = TRUE)
}

if (!exists("record_countries_at_step")) {
  source("Scripts/utils/country_tracking.R")
}

# Prepare OpenDengue data
OD_national_clean <- OD_national %>%
  dplyr::mutate(
    iso3 = ISO_A0,
    country = stringr::str_to_title(adm_0_name),
    cases = dengue_total_scaled,
    Year = Year,
    Month = month
  ) %>%
  dplyr::filter(Year > 2009) %>%
  dplyr::mutate(
    date = lubridate::make_date(year = Year, month = Month, day = 1)
  ) %>%
  dplyr::select(date, country, iso3, cases, Year, Month) %>%
  dplyr::distinct()

log_message("OpenDengue records prepared: " %+% nrow(OD_national_clean))

# Prepare WHO data
WHO_clean <- who %>%
  dplyr::select(date, country, iso3, cases) %>%
  dplyr::mutate(
    date = as.Date(date),
    country = countrycode::countrycode(iso3, "iso3c", "country.name"),
    country = ifelse(iso3 == "MDR", "Autonomous Region of Madeira", country),
    country = ifelse(iso3 == "MAF", "Saint Martin", country),
    cases = as.numeric(cases),
    Year = year(date),
    Month = month(date),
    cases = dplyr::if_else(cases < 0, NA_real_, cases)
  ) %>%
  dplyr::filter(Year > 2009)

if (any(is.na(WHO_clean$country))) {
  log_message("Warning: Not all ISO3 codes in WHO database matched to countries", level = "WARNING")
}

# Record countries after cleaning (Step 2: Cleaned Data)
if (exists("record_countries_at_step") && exists("WHO_clean") && exists("OD_national_clean")) {
  tryCatch({
    # Combine cleaned countries
    cleaned_countries <- dplyr::bind_rows(
      WHO_clean %>% dplyr::select(country, iso3) %>% dplyr::distinct(),
      OD_national_clean %>% dplyr::select(country, iso3) %>% dplyr::distinct()
    ) %>% dplyr::distinct()
    
    record_countries_at_step(cleaned_countries, "Step_2_Cleaned_Data")
  }, error = function(e) {
    # Silently fail - tracking should not stop pipeline
    if (exists("log_message")) {
      log_message("Warning: Country tracking failed at Step 3a Clean: " %+% conditionMessage(e), level = "WARNING")
    }
  })
}

# Interpolate WHO data
WHO_interpolated <- interpolate_missing_WHO_data(WHO_clean)

WHO_clean_final <- WHO_interpolated %>%
  dplyr::mutate(
    cases = interpolated_cases,
    date = lubridate::make_date(year = Year, month = Month, day = 1)
  ) %>%
  dplyr::select(date, country, iso3, cases, Year, Month) %>%
  dplyr::distinct()

log_message("WHO records prepared: " %+% nrow(WHO_clean_final))

# Combine: prefer OD_national, use WHO when OD is missing
# Add source indicator to track data origin
OD_national_with_source <- OD_national_clean %>%
  dplyr::mutate(source = "OD_national")

WHO_clean_with_source <- WHO_clean_final %>%
  dplyr::mutate(source = "WHO")

WHO_OD_combined <- dplyr::bind_rows(OD_national_with_source, WHO_clean_with_source)

log_message("Combined dataset rows: " %+% nrow(WHO_OD_combined))

# Normalize country names to a single canonical name per iso3
# This prevents name mismatches between OD and WHO from causing duplicates downstream
WHO_OD_combined <- WHO_OD_combined %>%
  dplyr::mutate(
    country_canonical = countrycode::countrycode(iso3, "iso3c", "country.name",
                                                  custom_match = c("MDR" = "Autonomous Region of Madeira",
                                                                   "MAF" = "Saint Martin")),
    country = dplyr::if_else(is.na(country_canonical),
                             country,  # keep original if countrycode fails
                             country_canonical)
  ) %>%
  dplyr::select(-country_canonical)

log_message("Country names normalized to canonical names per iso3")

# Clean combined data - prefer OD_national when duplicates exist
# For each country-month combination, keep OD_national if available, otherwise WHO
# Use iso3, date as the deduplication key (not country, date) to handle name variants
WHO_OD_combined_clean <- WHO_OD_combined %>%
  dplyr::group_by(iso3, date) %>%
  dplyr::mutate(
    Number_of_obs = n(),
    # Prefer OD_national: if OD_national exists, keep it; otherwise keep WHO
    To_keep = dplyr::case_when(
      Number_of_obs == 1 ~ "Keep",
      Number_of_obs > 1 & any(source == "OD_national") ~ ifelse(source == "OD_national", "Keep", "Remove"),
      Number_of_obs > 1 & !any(source == "OD_national") ~ "Keep"  # Only WHO available
    )
  ) %>%
  dplyr::filter(To_keep == "Keep") %>%
  dplyr::select(!To_keep & !Number_of_obs) %>%
  dplyr::ungroup()

#--------------- Filter combined data - only remove all-zero years
# Let seasonal filtering handle completeness requirements

# Prepare data with Year and Month columns (if not already present)
WHO_OD_combined_clean <- WHO_OD_combined_clean %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Year = year(date),
                Month = month(date))

# Filter for all-zero years only (let seasonal filtering handle completeness)
full_data_nonzero_years <- WHO_OD_combined_clean %>%
  dplyr::group_by(country, Year) %>%
  dplyr::mutate(All_zeroes = ifelse(sum(cases, na.rm = TRUE) == 0, "Yes", "No")) %>%
  dplyr::filter(All_zeroes == "No") %>%
  dplyr::select(!All_zeroes) %>%
  dplyr::ungroup()

# Final data - no additional filtering (seasonal script will handle completeness)
full_data <- full_data_nonzero_years

#--------------------------- Print status update 

log_message("Filtered combined dataset rows: " %+% nrow(full_data))
log_message("Removed all-zero years only - seasonal filtering will handle completeness requirements.")
log_message("Finished combining OpenDengue and WHO data in 01_select_historic_data.R script.")

# Determine specific drop reasons for countries that were in before_filter but not after
if (exists("record_countries_at_step")) {
  tryCatch({
    # Get countries before and after filtering
    countries_before <- WHO_OD_combined_clean %>%
      dplyr::select(country, iso3) %>%
      dplyr::distinct()
    
    countries_after_nonzero <- full_data_nonzero_years %>%
      dplyr::select(country, iso3) %>%
      dplyr::distinct()
    
    countries_after_final <- full_data %>%
      dplyr::select(country, iso3) %>%
      dplyr::distinct()
    
    # Create drop reason mapping - only tracking all-zero years filter
    # Only include countries that were dropped (not in final)
    dropped_countries <- countries_before %>%
      dplyr::filter(!iso3 %in% countries_after_final$iso3)
    
    if (nrow(dropped_countries) > 0) {
      drop_reasons_df <- dropped_countries %>%
        dplyr::mutate(
          drop_reason = dplyr::case_when(
            # Dropped at nonzero years step (only filter applied)
            !iso3 %in% countries_after_nonzero$iso3 ~ "Filtered: all-zero years",
            # Should not happen, but just in case
            TRUE ~ "Filtered: unknown reason"
          )
        ) %>%
        dplyr::select(iso3, drop_reason)  # Ensure only iso3 and drop_reason columns
      
      # Store for use in tracking
      assign("step3b_drop_reasons", drop_reasons_df, envir = .GlobalEnv)
      
      if (exists("log_message")) {
        log_message("Created drop reasons for " %+% nrow(drop_reasons_df) %+% " countries dropped at step 3b filtering (all-zero years only)")
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

# Record countries after filtering (Step 3a: After Case Filter)
if (exists("record_countries_at_step")) {
  tryCatch({
    # Use country-specific drop reasons if available
    if (exists("step3b_drop_reasons") && is.data.frame(step3b_drop_reasons) && nrow(step3b_drop_reasons) > 0) {
      if (exists("log_message")) {
        log_message("Using country-specific drop reasons for " %+% nrow(step3b_drop_reasons) %+% " countries at step 3b")
      }
      record_countries_at_step(full_data, "Step_3a_After_Case_Filter",
                               drop_reason = step3b_drop_reasons)
      # Clean up
      rm(step3b_drop_reasons, envir = .GlobalEnv)
    } else {
      if (exists("log_message")) {
        log_message("No countries dropped at step 3b filtering")
      }
      # Record final countries without drop reasons (no countries were dropped)
      record_countries_at_step(full_data, "Step_3a_After_Case_Filter")
    }
  }, error = function(e) {
    if (exists("log_message")) {
      log_message("Warning: Country tracking failed at Step 3b After Filter: " %+% conditionMessage(e), level = "WARNING")
    }
  })
}