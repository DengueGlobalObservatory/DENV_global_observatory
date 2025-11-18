#' ---
#' title: "00_WHO_data_processing_functions"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
#' Functions needed to process WHO data:
#'    Temporal interpolation 
#'    Assess data coverage
#'     
#' 
#' Timeline:
#' =========
#' 19-09-2025: Initial commit

library(imputeTS)

interpolate_missing_WHO_data <- function(WHO_data) {
  
  # Clean and interpolate
  WHO_clean <- WHO_data %>% 
    ungroup() %>% 
    mutate(
      Month = month(date),
      Month.Year = paste0(Month, "-", Year)
    ) 
  
  WHO_interpolated <- WHO_clean %>%
    group_by(country) %>% 
    group_modify(~{
      tryCatch({
        
        x_interpolated <- .x %>%
          mutate(
            interpolated_cases = ceiling(na_interpolation(cases, option = "linear", maxgap = 1))
          ) %>%
          filter(
            !is.na(interpolated_cases)
          ) %>%
          mutate(
            iso3 = ifelse(is.na(iso3), unique(na.omit(iso3)), iso3)
          )
        return(x_interpolated)
        
      }, error = function(e){
        message("ERROR in WHO data interpolation: ", e$message)
        
        # Return failure info
        .x %>% 
          mutate(
            iso3 = unique(.x$iso3),
            interpolation_status = "Failed",
            interpolation_error = as.character(e$message)
          )
      })
    })
  
  return(WHO_interpolated)
}  

assess_WHO_interpolated_data_coverage <- function(WHO_extract){
  
  WHO_extract_coverage <- WHO_extract %>%
    na.omit() %>%
    group_by(country, Year) %>%
    group_modify(~{
      tryCatch({
        
        x_coverage <- .x %>% 
          dplyr::summarize(
            WHO_annual_counts = n()
          ) %>% 
          as.data.frame() 
        
        return(x_coverage)
      }, error = function(e){
        message("ERROR in WHO data coverage assessment: ", e$message)
        .x %>% 
          mutate(
            iso3 = unique(.x$iso3),
            Data_coverage_status = "Failed",
            error_message = as.character(e$message)        
          )
      })
    })
  
  WHO_extract_coverage_clean <- WHO_extract_coverage %>%
    dplyr::mutate(
      iso3 = countrycode(country, "country.name", "iso3c")
    )
  
  return(WHO_extract_coverage_clean)
}

extract_desired_WHO_data <- function(WHO_extract, coverage_results){
  
  #--- Splitting data into countries overlapping and not overlapping with OpenDengue 
  OD_WHO_overlap_countries <- coverage_results$iso3
  
  Overlap_countries_data <- WHO_extract %>%
    dplyr::filter(iso3 %in% OD_WHO_overlap_countries)
  
  Non_overlap_countries_data <- WHO_extract %>%
    dplyr::filter(!iso3 %in% OD_WHO_overlap_countries)
  
  #--- Filtering overlap data for desired country years
  
  # Identify target years from WHO
  Years_from_WHO <- coverage_results %>%
    dplyr::filter(
      Which_to_keep_clean == "WHO") %>%
    dplyr::select(
      Year, country.WHO, iso3) %>%
    dplyr::mutate(
      WHO_Country_Year = paste(country.WHO, "_", Year))
  
  # Extract desired data from overlap countries
  Overlap_countries_target_data <- WHO_extract %>%
    dplyr::mutate(
      Country_Year = paste(country, "_", Year)) %>%
    dplyr::filter(
      Country_Year %in% Years_from_WHO$WHO_Country_Year) %>% 
    dplyr::select(
      !Country_Year)
  
  #-- Join filtered data back to original 
  WHO_processed_data <- rbind(Non_overlap_countries_data, 
                              Overlap_countries_target_data)
  
  return(WHO_processed_data)
}
