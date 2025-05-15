#' ---
#' title: "00 rolling_cross_validation_fun"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
#' Function to perform cross validation of monthly proportion of cases on a rolling window of years.
#'  
#' Timeline:
#' =========
#' 14-05-2025: Prepared function + added header description.
#' 15-05-2025: Fixed bug in function. Previously was not grouping by Country in actual monthly proportion calculation. 
#'

rolling_cross_validation_fun <- function(x, Number_of_years){
  
  # Calculate monthly proportions 
  x_monthly_proportions <- x %>% 
    group_by(Country, Year) %>% 
    mutate(Actual_monthly_proportion = Cases_clean / sum(Cases_clean)) %>% 
    ungroup() 
  
  # 5-year rolling cross validation 
  x_monthly_proportions_rolling_CV <- x_monthly_proportions %>%
    mutate(Date = ymd(paste0(Year, "-", Month, "-01"))) %>% 
    group_by(Country, Month) %>%
    mutate(Rolling_monthly_proportion = slide_period_dbl(Actual_monthly_proportion, 
                                                         Date, 
                                                         .f = ~mean(head(.x, -1)),  # Exclude current year
                                                         .before = Number_of_years,
                                                         .period = "year",
                                                         .complete = TRUE)) %>%  # Require full window
    ungroup() %>%
    filter(!is.na(Rolling_monthly_proportion))  
  
  x_monthly_proportions_rolling_CV_clean <- x_monthly_proportions_rolling_CV %>% 
    as.data.frame() %>% 
    group_by(Country, Year) %>% 
    mutate(Predicted_cases = sum(Cases_clean) * Rolling_monthly_proportion)
  
  return(x_monthly_proportions_rolling_CV_clean)
  
}
