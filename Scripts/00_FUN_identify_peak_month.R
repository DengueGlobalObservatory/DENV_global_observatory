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
  
  x_ave_peak <- x %>% 
    filter(!is.na(Country)) %>%
    ungroup() %>% 
    select(Country, iso3, season, Month_to_predict, Actual_monthly_cases) %>%
    distinct() %>% 
    group_by(Country, iso3, season) %>% 
    summarise(Season_peak_Month = Month_to_predict[which.max(Actual_monthly_cases)]) %>% 
    ungroup() %>% 
    group_by(Country, iso3) %>% 
    summarise(Average_season_peak_month = circular_mean(Season_peak_Month))
  
  #---------------------- Add in mean peak month to main df
  
  x_final <- x %>% 
    left_join(., x_ave_peak, by = c("Country", "iso3"))
 
  #---------------------- Return data  
  
  return(x_final)
}