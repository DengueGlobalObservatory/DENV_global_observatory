#' ---
#' title: "00 filtering_for_10_years_of_continuous_data_fun"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
#' Filter for locations with at least 10 years of continuous data.
#'  
#' Timeline:
#' =========
#' 14-05-2025: Prepared function.
#'

filtering_for_10_years_of_continuous_data_fun <- function(x){
  
  Countries_with_at_least_10_years <- x %>% 
    group_by(Country) %>% 
    arrange(Country, Year, Month) %>% 
    distinct(Year,.keep_all = TRUE) %>% 
    mutate(
      year_diff = Year - lag(Year, default = first(Year)),
      group = cumsum(coalesce(year_diff, 1) != 1)  # Identify breaks in continuity
    )  %>%
    ungroup() %>% 
    group_by(Country, group) %>% 
    mutate(Number_of_continuous_years = n()) %>%
    filter(Number_of_continuous_years >= 10) %>%
    select(Country, Year, group) 
    
  x_filtered <- x %>% 
    right_join(., Countries_with_at_least_10_years, by = c("Country", "Year"))

}