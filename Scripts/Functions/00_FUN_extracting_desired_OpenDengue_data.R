#' ---
#' title: "00 extracting_desired_OpenDengue_data_fun"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
#' Filtering OpenDengue data for desired years for countries in both WHO and OpenDengue databases. 
#' Join target overlapping data back to non overlapping data. 
#'  
#' Timeline:
#' =========
#' 14-05-2025: Prepared function.
#'

extracting_desired_OpenDengue_data_fun <- function(OpenDengue_national_extract, coverage_results){
  
  #--- Filter national extract to remove yearly reporting 
  OpenDengue_national_extract_clean <- OpenDengue_national_extract %>% 
    filter(T_res != "Year")
  
  #--- Splitting national extract into countries overlapping and not overlapping with WHO 
  Countries_in_OpenDengue_overlapping_with_WHO <- coverage_results$iso3
  
  Overlap_countries_data <- OpenDengue_national_extract_clean %>%
    filter(ISO_A0 %in% Countries_in_OpenDengue_overlapping_with_WHO)
  
  Non_overlap_countries_data <- OpenDengue_national_extract_clean %>%
    filter(!ISO_A0 %in% Countries_in_OpenDengue_overlapping_with_WHO)
  
  
  #--- Filtering overlap data for desired country years
  
  # Identify target years from OpenDengue
  Years_from_OD <- coverage_results %>%
    filter(Which_to_keep_clean == "OpenDengue") %>%
    select(Year, adm_0_name.OD, iso3) %>%
    mutate(OD_Country_Year = paste(adm_0_name.OD, "_", Year))
  
  Overlap_countries_target_data <- Overlap_countries_data %>% 
    mutate(Country_Year = paste(adm_0_name, "_", Year)) %>%
    filter(Country_Year %in% Years_from_OD$OD_Country_Year)  %>% 
    select(!Country_Year)
  
  #-- Join filtered data back to original 
  OpenDengue_processed_data <- rbind(Non_overlap_countries_data, 
                                     Overlap_countries_target_data)
  
  return(OpenDengue_processed_data)
}
