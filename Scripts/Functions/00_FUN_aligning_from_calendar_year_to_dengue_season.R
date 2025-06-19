#' ---
#' title: "00_FUN_aligning_from_calendar_year_to_dengue_season"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Align from calendar year to dengue season function. 
#' 
#' Timeline:
#' =========
#' 22-05-2025: Prepared function. 
#' 04-06-2025: Removed code to calculate circular sd from circular mean function, unnecessary. 
#'             Fixed code to allow for low months with equal number of cases and average them within the year before taking the average across years.

aligning_from_calendar_year_to_dengue_season <- function(country_counts_data){
  
  
  #----- Identify low month to use as reference point 
  dengue_season_low_month <- country_counts_data %>% 
    ungroup() %>% 
    group_by(Country, Year) %>% 
    slice_min(order_by = Cases_clean, n = 1) %>% 
    rename(Low_month = Month)
  
  #----- Identify average low month 
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
  
  dengue_season_ave_low_month <- dengue_season_low_month %>% 
    ungroup() %>% 
    group_by(Country, Year) %>%
    filter(sum(Cases_clean) > 0) %>%
    mutate(No_of_obs = n()) %>% 
    mutate(within_year_mean_low_month = case_when(No_of_obs > 1 ~ circular_mean(Low_month),
                                                  No_of_obs == 1  ~ Low_month)) %>%
    select(Country, Year, within_year_mean_low_month) %>%
    distinct() %>%
    ungroup() %>% 
    group_by(Country) %>%
    summarise(mean_low_month = circular_mean(within_year_mean_low_month))
  

  #----- Add low month back to original dengue counts df 
  country_counts_data_clean <- country_counts_data %>% 
    full_join(., dengue_season_ave_low_month, by = "Country") %>% 
    rename(Calendar_year_month = Month)
  
  
  #----- Align data from calendar year to dengue season 
  
  # Function 
  circular_encode <- function(x, shift, cycle_length) {
    (((x) - shift) %% cycle_length) + 1
  }
  
  country_counts_data_season_aligned <- country_counts_data_clean %>% 
      mutate(season_nMonth= circular_encode(Calendar_year_month, mean_low_month, 12)) %>%
      mutate(season = case_when(Calendar_year_month >= mean_low_month ~ paste0(Year, "/", (Year+1)),
                                Calendar_year_month < mean_low_month ~ paste0((Year-1), "/", Year)))
    
  #----- Return results
  return(country_counts_data_season_aligned)
    
}
