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

#--------------- Clean data

WHO_clean <- WHO %>% 
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

OD_national_clean <- OD_national_extract %>%
  dplyr::filter(T_res != "Year") %>% 
  dplyr::mutate(adm_0_name = countrycode(ISO_A0, "iso3c", "country.name"))

#---------------------- Listing countries unique to one source + in both  

# Countries only in OpenDengue 
Countries_only_in_OD_national <- OD_national_clean$adm_0_name[!OD_national_clean$ISO_A0 %in% WHO_clean$iso3] %>% 
  unique()

# Countries only in WHO 
Countries_only_in_WHO_national <- WHO_clean$country[!WHO_clean$iso3 %in% OD_national_clean$ISO_A0] %>% 
  unique()

# Countries in both 
Countries_in_both_df <- tibble(
  Country = unique(OD_national_clean$adm_0_name[OD_national_clean$ISO_A0 %in% WHO_clean$iso3])
) %>%
  mutate(
    iso3 = countrycode(Country, "country.name", "iso3c"),
    iso3 = dplyr::if_else(Country == "SAINT MARTIN", "MAF", iso3)
  )

# Countries in both WHO and OpenDengue databases:
cat("Countries in both WHO and OpenDengue databases:\n", paste(as.character(Countries_in_both_df$Country), collapse = ", "), "\n")

# Countries with data in just OpenDengue 
cat("Countries only included in OpenDengue:\n", paste(as.character(Countries_only_in_OD_national), collapse = ", "), "\n")

# Countries with data in just WHO 
cat("Countries only included in WHO database:\n", paste(as.character(Countries_only_in_WHO_national), collapse = ", "), "\n")

#---------------------- Data coverage for countries in both (OpenDengue)

#---- Extract data for countries in both
OD_data_for_countries_in_both <- OD_national_clean %>% 
  dplyr::filter(ISO_A0 %in% Countries_in_both_df$iso3)

# Deduplicate 
OD_dedup <- deduplicate_OD_data(OD_data_for_countries_in_both)

# Interpolate
OD_interpolated <- OD_dedup %>% 
  # Remove locations with only one row
  add_count(ISO_A0, adm_0_name, name = "Counts") %>% 
  dplyr::filter(Counts > 1) %>% 
  dplyr::select(-Counts) %>%
  interpolate_missing_national_OD_data()

if(any(grepl("^interpolation", colnames(OD_interpolated)))){
   stop("Error in OpenDengue interpolation")}

# Data coverage of OD countries in both
OD_coverage <- OD_interpolated %>%
  group_by(ISO_A0) %>%
  group_modify(~ assess_OD_national_interpolated_data_coverage(.x)) %>%
  ungroup() %>%
  dplyr::mutate(
    iso3 = countrycode(adm_0_name, "country.name", "iso3c"),
    iso3 = dplyr::if_else(adm_0_name == "SAINT MARTIN", "MAF", iso3)
  )

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
      
      # Scaling weekly --> monthly
      OD_annual_counts_scaled = ifelse(
        T_res == "Month", OD_annual_counts, OD_annual_counts * (12/52)
      ),
      
      # Weekly vs monthly comparison - different coverage
      Which_to_keep = dplyr::case_when(
        is.na(Which_to_keep) & OD_annual_counts_scaled > WHO_annual_counts ~ "OpenDengue",
        is.na(Which_to_keep) & OD_annual_counts_scaled < WHO_annual_counts ~ "WHO",
        is.na(Which_to_keep) & OD_annual_counts_scaled == WHO_annual_counts ~ "Either",
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

print("Finished running 01_Choosing_WHO_or_OpenDengue_data script.")

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
