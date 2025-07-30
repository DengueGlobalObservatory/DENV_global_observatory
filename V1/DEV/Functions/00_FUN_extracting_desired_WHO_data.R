#' ---
#' title: "00 extracting_desired_WHO_data_fun"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
#' Filtering WHO data for desired years for countries in both WHO and OpenDengue databases. 
#' Join target overlapping data back to non overlapping data. 
#'  
#' Timeline:
#' =========
#' 14-05-2025: Prepared function.
#'

extracting_desired_WHO_data_fun <- function(WHO_extract, coverage_results){
  
  #--- Splitting national extract into countries overlapping and not overlapping with WHO 
  Countries_in_WHO_overlapping_with_OpenDengue <- coverage_results$iso3
  
  Overlap_countries_data <- WHO_extract %>%
    filter(iso3 %in% Countries_in_WHO_overlapping_with_OpenDengue)
  
  Non_overlap_countries_data <- WHO_extract %>%
    filter(!iso3 %in% Countries_in_WHO_overlapping_with_OpenDengue)
  
  #--- Filtering overlap data for desired country years
  
  # Identify target years from OpenDengue
  Years_from_WHO <- coverage_results %>%
    filter(Which_to_keep_clean == "WHO") %>%
    select(Year, adm_0_name.WHO, iso3) %>%
    mutate(WHO_Country_Year = paste(adm_0_name.WHO, "_", Year))
  
  Overlap_countries_target_data <- WHO_extract %>%
    mutate(Country_Year = paste(country, "_", Year)) %>%
    filter(Country_Year %in% Years_from_WHO$WHO_Country_Year) %>% 
    select(!Country_Year)
  
  #-- Join filtered data back to original 
  WHO_processed_data <- rbind(Non_overlap_countries_data, 
                              Overlap_countries_target_data)
  
  return(WHO_processed_data)
}