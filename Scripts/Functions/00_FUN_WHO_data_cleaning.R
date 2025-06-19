#' ---
#' title: "00 WHO_data_cleaning"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Clean WHO data.
#' 
#' Timeline:
#' =========
#' 14-05-2025: Prepared function.
#'

WHO_data_cleaning <- function(x){
  x_clean <- x %>%
    select(date, country, iso3, cases) %>% 
    mutate(date = as.Date(date),
           cases = as.numeric(cases)) %>%
    mutate(Year = year(date)) %>%
    mutate(cases = case_when(cases < 0 ~ NA,
                             TRUE ~ cases))
}