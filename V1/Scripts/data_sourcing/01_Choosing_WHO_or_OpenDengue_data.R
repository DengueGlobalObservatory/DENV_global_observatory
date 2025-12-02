#' ---
#' title: "01_Choosing_WHO_or_OpenDengue_data"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
#' Compare data coverage by year between WHO and OpenDengue datasets. Select years from source with higher coverage.
#' 
#' Timeline:
#' =========
#' 

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

log_message("Running 01_Choosing_WHO_or_OpenDengue_data.")

#--------------- Clean data

WHO_clean <- who %>% 
  dplyr::select(date, country, iso3, cases) %>% 
  dplyr::mutate(
    date = as.Date(date),
    country = countrycode(iso3, "iso3c", "country.name"),
    country = ifelse(iso3 == "MDR", "Autonomous Region of Madeira", country),
    cases = as.numeric(cases),
    Year = year(date),
    cases = dplyr::if_else(cases < 0, NA_real_, cases)
  ) 

if(any(is.na(WHO_clean$country))){message("Not all ISO3 codes in WHO database matched to countries - investigate.")}

OD_national_clean <- OD_national %>%
  mutate(
    iso3 = ISO_A0,
    country = str_to_sentence(adm_0_name),
    cases = dengue_total_scaled
  )
# %>%
#   dplyr::filter(T_res != "Year") %>% 
#   dplyr::mutate(adm_0_name = countrycode(ISO_A0, "iso3c", "country.name"))

#---------------------- Listing countries unique to one source + in both  

# Countries only in OpenDengue 
Countries_only_in_OD_national <- OD_national_clean$country[!OD_national_clean$iso3 %in% WHO_clean$iso3] %>% 
  unique()

# Countries only in WHO 
Countries_only_in_WHO_national <- WHO_clean$country[!WHO_clean$iso3 %in% OD_national_clean$iso3] %>% 
  unique()

# Countries in both 
countries_vec <- unique(OD_national_clean$country[OD_national_clean$iso3 %in% WHO_clean$iso3])
Countries_in_both_df <- tibble(
  Country = as.character(countries_vec)
) %>%
  mutate(
    iso3 = countrycode(Country, "country.name", "iso3c"),
    iso3 = dplyr::if_else(Country == "SAINT MARTIN", "MAF", iso3)
  )

log_message("Countries in both WHO and OpenDengue databases: " %+% paste(as.character(Countries_in_both_df$Country), collapse = ", "))
log_message("Count - Countries in both WHO and OpenDengue databases: " %+% length(Countries_in_both_df$Country))
log_message("Countries only included in OpenDengue: " %+% paste(as.character(Countries_only_in_OD_national), collapse = ", "))
log_message("Count - Countries only included in OpenDengue: " %+% length(Countries_only_in_OD_national))
log_message("Countries only included in WHO database: " %+% paste(as.character(Countries_only_in_WHO_national), collapse = ", "))
log_message("Count - Countries only included in WHO database: " %+% length(Countries_only_in_WHO_national))

# Record countries after cleaning (Step 3a: WHO/OD Clean)
if (exists("record_countries_at_step") && exists("WHO_clean") && exists("OD_national_clean")) {
  tryCatch({
    # Combine cleaned countries
    cleaned_countries <- dplyr::bind_rows(
      WHO_clean %>% dplyr::select(country, iso3) %>% dplyr::distinct(),
      OD_national_clean %>% dplyr::select(country, iso3) %>% dplyr::distinct()
    ) %>% dplyr::distinct()
    
    record_countries_at_step(cleaned_countries, "Step_3a_WHO_OD_Clean")
  }, error = function(e) {
    # Silently fail - tracking should not stop pipeline
    if (exists("log_message")) {
      log_message("Warning: Country tracking failed at Step 3a Clean: " %+% conditionMessage(e), level = "WARNING")
    }
  })
}

#---------------------- Data coverage for countries in both (OpenDengue)

#---- Extract data for countries in both
OD_data_for_countries_in_both <- OD_national_clean %>% 
  dplyr::filter(iso3 %in% Countries_in_both_df$iso3)

# # Deduplicate 
# OD_dedup <- deduplicate_OD_data(OD_data_for_countries_in_both)

# # Interpolate
# OD_interpolated <- OD_dedup %>% 
#   # Remove locations with only one row
#   add_count(ISO_A0, adm_0_name, name = "Counts") %>% 
#   dplyr::filter(Counts > 1) %>% 
#   dplyr::select(-Counts) %>%
#   interpolate_missing_national_OD_data()

# if(any(grepl("^interpolation", colnames(OD_interpolated)))){
#    stop("Error in OpenDengue interpolation")}

# Data coverage of OD countries in both
OD_coverage <- OD_data_for_countries_in_both %>%
  group_by(iso3) %>%
  # group_modify(~ assess_OD_national_interpolated_data_coverage(.x)) %>%
  ungroup() %>%
  dplyr::mutate(
    iso3 = countrycode(country, "country.name", "iso3c"),
    iso3 = dplyr::if_else(country == "SAINT MARTIN", "MAF", iso3)
  )%>%
  group_by(iso3, Year) %>%
  mutate(
    OD_annual_counts = sum(cases)
  ) %>%
  ungroup()

if(any(grepl("^error", colnames(OD_coverage)))){
  stop("Error in OpenDengue coverage assessment")}

#---------------------- Data coverage for countries in both (WHO)

# Extract data for countries in both
WHO_countries_in_both <- WHO_clean %>% 
  dplyr::filter(
    iso3 %in% Countries_in_both_df$iso3
  )

# Interpolate 
WHO_interpolated <- interpolate_missing_WHO_data(WHO_countries_in_both) 
if(any(grepl("error", colnames(WHO_interpolated)))){
  stop("Error in WHO interpolation")}

# Data coverage of WHO countries in both 
WHO_coverage <- assess_WHO_interpolated_data_coverage(WHO_interpolated)
if(any(grepl("error", colnames(WHO_coverage)))){
  stop("Error in WHO coverage assessment")}

#--------------------------- Compare coverage between countries in both databases 

identify_which_years_to_keep_OD_vs_WHO <- function(OD_coverage, 
                                                   WHO_coverage){
  
  OD_WHO_coverage <- full_join(
    OD_coverage, WHO_coverage, 
    by = c("Year", "iso3"), 
    suffix = c(".OD", ".WHO")
  )
  
  OD_WHO_coverage_comp <- OD_WHO_coverage %>% 
    dplyr::mutate(
      # Weekly vs monthly comparison - equal coverage
      Which_to_keep = case_when(
        is.na(WHO_annual_counts) ~ "OpenDengue",
        is.na(OD_annual_counts) ~ "WHO",
        OD_annual_counts %in% c(51, 52, 53) & WHO_annual_counts == 12 ~ "Either",
        TRUE ~ NA),
      
      # # Scaling weekly --> monthly
      # OD_annual_counts_scaled = ifelse(
      #   T_res == "Month", OD_annual_counts, OD_annual_counts * (12/52)
      # ),
      
      # Weekly vs monthly comparison - different coverage
      Which_to_keep = dplyr::case_when(
        is.na(Which_to_keep) & OD_annual_counts > WHO_annual_counts ~ "OpenDengue",
        is.na(Which_to_keep) & OD_annual_counts < WHO_annual_counts ~ "WHO",
        is.na(Which_to_keep) & OD_annual_counts == WHO_annual_counts ~ "Either",
        TRUE ~ Which_to_keep
      )
    ) 
  
  OD_WHO_coverage_comp_final <- OD_WHO_coverage_comp %>%
    group_by(iso3) %>%
    dplyr::mutate(
      # Count number of observations coming from OpenDengue and WHO by country
      OD_total = sum(Which_to_keep == "OpenDengue", na.rm = TRUE),
      WHO_total = sum(Which_to_keep == "WHO", na.rm = TRUE), 
      
      # Select high coverage database, where equal number of obs from each select WHO
      Which_to_keep_clean = case_when(
        Which_to_keep == "Either" & OD_total > WHO_total ~ "OpenDengue",
        Which_to_keep == "Either" & OD_total < WHO_total ~ "WHO",
        Which_to_keep == "Either" & OD_total == WHO_total ~ "WHO",
        TRUE ~ Which_to_keep)
    ) 
  
  return(OD_WHO_coverage_comp_final)
}

WHO_OD_coverage <- identify_which_years_to_keep_OD_vs_WHO(
  OD_coverage,
  WHO_coverage)

#--------------------------- Print status update 

log_message("Finished running 01_Choosing_WHO_or_OpenDengue_data script.")

# Record countries selected for historical data (Step 3a: WHO/OD Selection)
# Countries that will be used are those in WHO_OD_coverage
if (exists("record_countries_at_step") && exists("WHO_OD_coverage")) {
  tryCatch({
    selected_countries <- WHO_OD_coverage %>%
      dplyr::select(iso3) %>%
      dplyr::distinct() %>%
      dplyr::left_join(
        dplyr::bind_rows(
          WHO_clean %>% dplyr::select(country, iso3) %>% dplyr::distinct(),
          OD_national_clean %>% dplyr::select(country, iso3) %>% dplyr::distinct()
        ) %>% dplyr::distinct(),
        by = "iso3"
      ) %>%
      dplyr::filter(!is.na(country))
    
    record_countries_at_step(selected_countries, "Step_3a_WHO_OD_Selection")
  }, error = function(e) {
    # Silently fail - tracking should not stop pipeline
    if (exists("log_message")) {
      log_message("Warning: Country tracking failed at Step 3a Selection: " %+% conditionMessage(e), level = "WARNING")
    }
  })
}

#--------------------------- Saving

#----- Creating dir to save 
# 
# dir_to_save <- paste0("Output/01_Choosing_WHO_or_OpenDengue_data/", Sys.Date())
# dir.create(dir_to_save, recursive = TRUE)
# 
# #----- Saving
# write_csv(WHO_OD_coverage, 
#           file = paste0(
#             dir_to_save, "/Comparing_coverage_between_WHO_and_OpenDengue.csv")
# )
