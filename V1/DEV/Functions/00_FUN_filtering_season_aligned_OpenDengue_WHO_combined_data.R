#' ---
#' title: "00_FUN_filtering_season_aligned_OpenDengue_WHO_combined_data"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Filter combined data for seasons without enough coverage (12 data points), all zero years and locations with < 3 years of data.
#' 
#' Timeline:
#' =========
#' 22-05-2025: Prepared script.
#' 20-06-2025: Added threshold of at leat 5 cases per month on average.

filtering_season_aligned_OpenDengue_WHO_combined_data_fun <- function(x){
  
  # Filter for coverage, remove locations with < 12 data points in a year. 
  x_coverage_filtered <- x %>% 
    ungroup() %>%
    group_by(Country, season) %>%
    mutate(Number_of_months_in_year = n()) %>% 
    filter(Number_of_months_in_year == 12) %>% 
    select(!Number_of_months_in_year) %>%
    ungroup() 
  
  # Filter for seasons with < 5 cases on average per month 
  threshold <- 5
  x_monthly_case_thresh_filtered <- x_coverage_filtered %>% 
    ungroup() %>% 
    group_by(Country, season) %>%
    mutate(Average_cases_per_month = ave(Cases_clean)) %>%
    filter(Average_cases_per_month >= threshold) %>%
    select(!Average_cases_per_month)

  # Filter for locations with less than three years of data.
  x_season_no_filtered <- x_monthly_case_thresh_filtered %>%
    group_by(Country, season_nMonth) %>% 
    mutate(Number_of_years = n()) %>% 
    filter(Number_of_years >= 3) %>% 
    ungroup() %>%
    select(!Number_of_years)
  
  return(x_season_no_filtered)
}