#' ---
#' title: "00 filtering_OpenDengue_WHO_combined_data_fun"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Filter combined data for years without enough coverage (12 data points), all zero years and locations with < 3 years of data.
#' 
#' Timeline:
#' =========
#' 14-05-2025: Prepared function
#' 15-05-2025: Added comments. Grouped by country for all zero filtering.
#' 20-06-2025: Added threshold of at leat 5 cases per month on average.

filtering_OpenDengue_WHO_combined_data_fun <- function(x){
  
  # Filter for coverage, remove locations with < 12 data points in a year. 
  x_coverage_filtered <- x %>% 
      ungroup() %>%
      group_by(Country, Year) %>%
      mutate(Number_of_months_in_year = n()) %>% 
      filter(Number_of_months_in_year == 12) %>% 
      select(!Number_of_months_in_year) %>%
      ungroup() 
    
  # Filter for years with < 5 cases on average per month 
  threshold <- 5
  x_monthly_case_thresh_filtered <- x_coverage_filtered %>% 
    ungroup() %>% 
    group_by(Country, Year) %>%
    mutate(Average_cases_per_month = ave(Cases_clean)) %>%
    filter(Average_cases_per_month >= threshold) %>%
    select(!Average_cases_per_month)
  
  # Filter for locations with less than three years of data.
    x_year_no_filtered <- x_monthly_case_thresh_filtered %>%
      group_by(Country, Month) %>% 
      mutate(Number_of_years = n()) %>% 
      filter(Number_of_years >= 3) %>% 
      ungroup() %>%
      select(!Number_of_years)
    
    return(x_year_no_filtered)
}