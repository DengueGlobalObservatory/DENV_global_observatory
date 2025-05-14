#' ---
#' title: "00 filtering_OpenDengue_WHO_combined_data_fun"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#'   
#' 
#' Timeline:
#' =========
#' 14-05-2025: Prepared function
#'

filtering_OpenDengue_WHO_combined_data_fun <- function(x){
  
  x_coverage_filtered <- x %>% 
      ungroup() %>%
      group_by(Country, Year) %>%
      mutate(Number_of_months_in_year = n()) %>% 
      filter(Number_of_months_in_year == 12) %>% 
      select(!Number_of_months_in_year) %>%
      ungroup() 
    
  x_all_zero_filtered <- x_coverage_filtered %>% 
    ungroup() %>% 
    group_by(Year) %>% 
    mutate(All_zeroes = case_when(sum(Cases_clean) == 0 ~ "Yes",
                                  sum(Cases_clean) != 0 ~ "No")) %>% 
    filter(All_zeroes == "No") %>% 
    select(!All_zeroes)
  
    x_year_no_filtered <- x_all_zero_filtered %>%
      group_by(Country, Month) %>% 
      mutate(Number_of_years = n()) %>% 
      filter(Number_of_years >= 3) %>% 
      ungroup() %>%
      select(!Number_of_years)
    
    return(x_year_no_filtered)
}