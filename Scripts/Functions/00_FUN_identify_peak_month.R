#' ---
#' title: "00_FUN_identify_peak_month"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Calculate incidence. 
#' 
#' Timeline:
#' =========
#' 04-06-2025: Prepared script.
#' 09-06-2025: Adapted script to handle multiple peaks within a year.

identify_peak_month <- function(x){
  
  
  #---------------------- Identify mean peak month
  circular_mean <- function(month_number){
    
    # Convert months to angles 
    angles <- (month_number - 1) * 30
    
    # Create a circular object
    circ_months <- circular(angles, units = "degrees", modulo = "2pi")
    
    # Calculate circular mean and standard deviation
    circ_mean <- mean(circ_months)

    # Convert the mean angle back to month number
    # as.numeric(circ_mean) gives the mean angle in degrees
    mean_month <- ((as.numeric(circ_mean) %% 360) / 30) + 1
    mean_month <- ifelse(mean_month > 12, mean_month - 12, mean_month)
    mean_month_rounded <- round(mean_month)
    
    mean_month_rounded <- ifelse(mean_month_rounded == 0, 12, mean_month_rounded)
    
    return(mean_month_rounded)
    
  }
  
  x_peak <- x %>% 
    filter(!is.na(Country)) %>%
    group_by(Country, season) %>% 
    slice_max(order_by = Actual_monthly_cases_per_100000_pop, n = 1) %>% 
    rename(Peak_month = season_nMonth) %>% 
    mutate(No_of_obs = n()) %>% 
    mutate(within_season_mean_peak_month = case_when(No_of_obs > 1 ~ circular_mean(Peak_month),
                                                   No_of_obs == 1  ~ Peak_month)) %>%
    ungroup() %>% 
    group_by(Country, iso3) %>% 
    summarise(Average_season_peak_month = circular_mean(within_season_mean_peak_month))
  
  #---------------------- Add in mean peak month to main df
  
  x_final <- x %>% 
    left_join(., x_peak, by = c("Country", "iso3")) %>% 
    filter(!is.na(Country)) 
 
  #---------------------- Return data  
  
  return(x_final)
}