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

filtering_season_aligned_OpenDengue_WHO_combined_data_fun <- function(x){
  
  # Filter for coverage, remove locations with < 12 data points in a year. 
  x_coverage_filtered <- x %>% 
    ungroup() %>%
    group_by(Country, season) %>%
    mutate(Number_of_months_in_year = n()) %>% 
    filter(Number_of_months_in_year == 12) %>% 
    select(!Number_of_months_in_year) %>%
    ungroup() 
  
  # Filter for all zero years
  x_all_zero_filtered <- x_coverage_filtered %>% 
    ungroup() %>% 
    group_by(Country, season) %>%
    mutate(All_zeroes = case_when(sum(Cases_clean) == 0 ~ "Yes",
                                  sum(Cases_clean) != 0 ~ "No")) %>% 
    filter(All_zeroes == "No") %>% 
    select(!All_zeroes)
  
  # Filter for locations with less than three years of data.
  x_year_no_filtered <- x_all_zero_filtered %>%
    group_by(Country, season_nMonth) %>% 
    mutate(Number_of_years = n()) %>% 
    filter(Number_of_years >= 3) %>% 
    ungroup() %>%
    select(!Number_of_years)
  
  return(x_year_no_filtered)
}