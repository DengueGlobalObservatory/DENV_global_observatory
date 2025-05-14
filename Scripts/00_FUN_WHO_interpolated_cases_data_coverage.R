#' ---
#' title: "00 WHO_interpolated_cases_data_coverage_fun"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Identifies data coverage by year within WHO extract.
#' 
#' Timeline:
#' =========
#' 14-05-2025: Prepared function.
#'

WHO_interpolated_cases_data_coverage_fun <- function(WHO_extract){
  WHO_extract_coverage <- WHO_extract %>%
    na.omit() %>%
    group_by(Year) %>%
    summarize(Counts_by_year = n()) %>% as.data.frame()
  
  WHO_extract_coverage_clean <- WHO_extract_coverage %>%
    mutate(adm_0_name = unique(WHO_extract$country))
  
  return(WHO_extract_coverage_clean)
}