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

#--------------- Loading data
# opens in current version from the github
OD_national <- read_data(extract = "national", as_data_frame = TRUE, showProgress = FALSE)
#!!!# this is currently hard coded, this will need to be updated to an API pull of the most recent data
WHO <- read_excel("Data/WHO/dengue-global-data-2025-08-16.xlsx")

# Results
Choosing_WHO_or_OpenDengue_data <- new.env()
source("V1/Scripts/data_sourcing/01_Choosing_WHO_or_OpenDengue_data.R", 
       local = Choosing_WHO_or_OpenDengue_data,
       echo = FALSE)
WHO_OD_coverage <- Choosing_WHO_or_OpenDengue_data$WHO_OD_coverage

#--------------- Load functions
source("V1/Scripts/data_sourcing/FUNCTIONS/00_OpenDengue_national_data_processing_functions.R")
source("V1/Scripts/data_sourcing/FUNCTIONS/00_WHO_data_processing_functions.R")

#---------------------- Clean data 

# WHO
WHO_clean <- WHO %>% 
  dplyr::select(date, country, iso3, cases) %>% 
  dplyr::mutate(
    date = as.Date(date),
    country = countrycode(iso3, "iso3c", "country.name"),
    cases = as.numeric(cases),
    Year = year(date),
    cases = ifelse(cases < 0, NA, cases)
  ) 

# OpenDengue
OD_national_clean <- OD_national %>%
  dplyr::filter(T_res != "Year") %>% 
  dplyr::mutate(
    adm_0_name = countrycode(ISO_A0, "iso3c", "country.name")
  )

#--------------- Filtering OD and WHO datasets for chosen years 

# OpenDengue 
OD_data <- extract_desired_OD_data(OD_national_clean, WHO_OD_coverage)

# WHO
WHO_data <- extract_desired_WHO_data(WHO_clean, WHO_OD_coverage)

#--------------- Prepare OD data to join to WHO 

# Deduplicate 
OD_dedup <- deduplicate_OD_data(OD_data)

# Interpolate
OD_interpolated <- OD_dedup %>% 
  # Remove locations with only one row
  add_count(ISO_A0, adm_0_name, name = "Counts") %>% 
  dplyr::filter(Counts > 1) %>% 
  dplyr::select(-Counts) %>%
  interpolate_missing_national_OD_data(.) # BUGGING HERE, FIX!!!!!!

# if(any(grepl("^interpolation", colnames(OD_interpolated)))){
#    stop("Error in OpenDengue interpolation")}

#----- Disaggregating weeks crossing months 
# Allocate cases to respective months, assume equal numbers per day in weeks crossing months.
OD_weekly_disaggregated <- disaggregate_OD_cases_weeks_crossing_months(OD_interpolated)

# if(any(grepl("^disaggregation", colnames(OD_weekly_disaggregated)))){
#   stop("Error in OpenDengue weekly disaggregation")}

#----- Aggregate weekly --> monthly 
OD_monthly <- aggregate_weekly_to_monthly_OD_cases(OD_weekly_disaggregated) 

if(any(grepl("^aggregation", colnames(OD_weekly_disaggregated)))){
  stop("Error in OpenDengue monthly aggregation")}

#----- Prepare OpenDengue data to combine with WHO data 
OD_monthly_final <- OD_monthly %>%
  ungroup() %>%
  arrange(ISO_A0, Year, Month) %>%
  mutate(
    date = lubridate::make_date(year = Year, month = Month, day = 1),
    iso3 = ISO_A0,
    cases = monthly_cases,
    country = str_to_title(adm_0_name), # fix ALL CAP
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

#----- Interpolate 
WHO_interpolated <- interpolate_missing_WHO_data(WHO_clean)

if(any(grepl("error", colnames(WHO_interpolated)))){
  stop("Error in WHO interpolation")}

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

#--------------- Filter combined data for complete years, all zero years, and countries with at least 3 years of data.

# Filter for coverage: remove locations with < 12 data points in a year. 
WHO_OD_combined_final <- WHO_OD_combined_clean %>% 
  ungroup() %>%
  dplyr::mutate(Year = year(date),
         Month = month(date)) %>%
  group_by(country, Year) %>%  
  
  # Filter for coverage: remove locations with < 12 data points in a year. 
  dplyr::mutate(Number_of_months_in_year = n()) %>% 
  dplyr::filter(Number_of_months_in_year == 12) %>% 
  dplyr::select(!Number_of_months_in_year) %>%
  
  # Filter for all zero years
  dplyr::mutate(All_zeroes = ifelse(sum(cases) == 0, "Yes", "No")) %>%  
  dplyr::filter(All_zeroes == "No") %>%
  dplyr::select(!All_zeroes) %>%
  ungroup() %>% 
  
  # Filter for locations with less than three years of data.
  group_by(country, Month) %>% 
  dplyr::mutate(Number_of_years = n()) %>% 
  dplyr::filter(Number_of_years >= 3) %>% 
  dplyr::select(!Number_of_years) %>%
  ungroup() %>% 
  dplyr::select(-Year, -Month)

#--------------------------- Print status update 

print("Finished combining OpenDengue and WHO data in 02_Combining_WHO_and_OpenDengue_data script.")

#---------------------- Saving 
# write_csv(WHO_OD_combined_final,
#           "Results/02_Combining_WHO_and_OpenDengue_data/OD_WHO_National_data_combined.csv")
