#' ---
#' title: "02_identify_seasonal_baseline"
#' author: "K Joshi"
#' 
#' ---
#'
#' Overview:
#' ========
#' Identify seasonal baseline. 
#' Save as a CSV.
#' Load new data as made available and when a year has complete data for a year add it to the dataset and re-identify the seasonal baseline.
#' Naming order: full_data_interpolated -> full_data_season_aligned -> full_data_filtered -> full_data_season_monthly_proportions -> full_data_average_season
#' 
#' Timeline:
#' ========
#' 08-09-2025: Reformatted DEV script. 

library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(circular)

#--------------- Loading data
full_data_interpolated <- read_csv("V1/DEV/02_Combining_WHO_and_OpenDengue_data/2025-09-08/National_clean_data.csv") # HARD CODED - change to load most recently available data...

#--------------- Align from calendar year to dengue season 

#----- Identify low month to use as reference point 
dengue_season_low_month <- full_data_interpolated %>% 
    ungroup() %>% 
    group_by(Country, 
             Year) %>% 
    slice_min(order_by = Cases_clean, 
              n = 1) %>% 
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
  group_by(Country, 
           Year) %>%
  filter(sum(Cases_clean) > 0) %>%
  mutate(No_of_obs = n()) %>% 
  mutate(within_year_mean_low_month = 
           case_when(No_of_obs > 1 ~ circular_mean(Low_month),
                     No_of_obs == 1  ~ Low_month)) %>%
  select(Country, 
         Year, 
         within_year_mean_low_month) %>%
  distinct() %>%
  ungroup() %>% 
  group_by(Country) %>%
  summarise(mean_low_month = circular_mean(within_year_mean_low_month))
  
#----- Add low month back to original dengue counts df 
full_data_interpolated <- full_data_interpolated %>% 
  full_join(., 
            dengue_season_ave_low_month, 
            by = "Country") %>% 
  rename(Calendar_year_month = Month) %>%
  distinct()
  
#----- Align data from calendar year to dengue season 
  
# Function 
circular_encode <- function(x, shift, cycle_length) {
  (((x) - shift) %% cycle_length) + 1
}
  
full_data_season_aligned <- full_data_interpolated %>% 
  mutate(season_nMonth= circular_encode(Calendar_year_month, 
                                        mean_low_month, 
                                        12)) %>%
  mutate(season = case_when(
    Calendar_year_month >= mean_low_month ~ paste0(Year, "/", (Year+1)),
    Calendar_year_month < mean_low_month ~ paste0((Year-1), "/", Year))
    )
  
#--------------- Filtering by season instead of calendar year
#' Filtering criteria:
#'  Coverage: remove locations with < 12 data points in a year. 
#'  Seasons with < 5 cases on average per month 
#'  Locations with less than three seasons of data. 

monthly_ave_case_threshold <- 5
full_data_filtered <- full_data_season_aligned %>% 
  ungroup() %>%
  
  # Filter for coverage: remove locations with < 12 data points in a year.
  group_by(Country, season) %>%
  mutate(Number_of_months_in_season = n()) %>% 
  filter(Number_of_months_in_season == 12) %>% 
  select(!Number_of_months_in_season) %>%
  
  # Filter for seasons with >=5 cases per month on average
  mutate(Average_cases_per_month = ave(Cases_clean)) %>%
  filter(Average_cases_per_month >= monthly_ave_case_threshold) %>%
  select(!Average_cases_per_month) %>% 
  
  # Filter for countries with at leas three seasons. 
  mutate(Number_of_seasons = n()) %>% 
  filter(Number_of_seasons >= 3) %>% 
  ungroup() %>%
  select(!Number_of_seasons)
  
#--------------- Identify average seasonal profile 

#----- Calculate monthly proportions by season
full_data_season_monthly_proportions <- full_data_filtered %>% 
    group_by(Country, iso3, season) %>% 
    arrange(season_nMonth) %>%
    mutate(
      Total_season_cases = sum(Cases_clean), 
      Actual_monthly_proportion = Cases_clean / Total_season_cases,
      Actual_cum_monthly_proportion = cumsum(Actual_monthly_proportion)
      ) %>% 
    ungroup()
  
#----- Identify ave seasonal profile (log CIs)
  
# offset to avoid log(0)
epsilon <- 1e-6
  
full_data_average_season <- full_data_season_monthly_proportions %>% 
  group_by(Country, iso3, season_nMonth) %>% 
  mutate(
    # Identify average season in case space
    Ave_season_cases = mean(Cases_clean),
    SD_monthly_cases = sd(Cases_clean),
    
    # Calculate one and two sd from mean values
    Ave_season_cases_two_sd_lower = Ave_season_cases - 2 * SD_monthly_cases, 
    Ave_season_cases_one_sd_lower = Ave_season_cases - SD_monthly_cases, 
    Ave_season_cases_one_sd_upper = Ave_season_cases + SD_monthly_cases, 
    Ave_season_cases_two_sd_upper = Ave_season_cases + 2 * SD_monthly_cases, 
    
    # Identify average season in proportion space
    Ave_monthly_proportion = mean(Actual_monthly_proportion),
    Ave_cum_monthly_proportion = mean(Actual_cum_monthly_proportion),
     
    # Log-transform proportions (+epsilon), compute mean and sd on log scale
    log_mean = mean(log(Actual_monthly_proportion + epsilon)),
    log_sd = sd(log(Actual_monthly_proportion + epsilon)),
    n = n(),
       
    # Calculate 1 and 2 sd lower and upper SD from mean 
    log_one_sd_lower = log_mean - log_sd, 
    log_two_sd_lower = log_mean - (2 * log_sd), 
    log_one_sd_upper = log_mean + log_sd, 
    log_two_sd_upper = log_mean + (2 * log_sd),
    
    # Back-transform lower and upper SD thresholds  
    one_sd_lower = exp(log_one_sd_lower),
    two_sd_lower = exp(log_two_sd_lower),
    one_sd_upper = exp(log_one_sd_upper),
    two_sd_upper = exp(log_two_sd_upper),
    
    # Calculate 95% CIs for mean 
    log_se = (sd(log(Actual_monthly_proportion + epsilon)) / sqrt(length(Actual_monthly_proportion))),
    log_upper_95CI = log_mean + (1.96 * log_se),
    log_lower_95CI = log_mean - (1.96 * log_se),
    
    # Back-transform lower and upper 95% CIs   
    lower_95CI = exp(log_lower_95CI),
    upper_95CI = exp(log_upper_95CI)
    ) %>% 
  
  arrange(Country, iso3, season_nMonth) %>%
  ungroup() %>%
  select(
    # Identifiers 
    Country, iso3, Calendar_year_month, season_nMonth, 
    
    # Case space ave season 
    Ave_season_cases_two_sd_lower, Ave_season_cases_one_sd_lower,
    Ave_season_cases,
    Ave_season_cases_one_sd_upper, Ave_season_cases_two_sd_upper,
    
    # Prop space ave season
    Ave_cum_monthly_proportion, 
    two_sd_lower, one_sd_lower, 
    Ave_monthly_proportion, 
    one_sd_upper, two_sd_upper,
    
    # 95% CIs around proportions 
    lower_95CI, 
    upper_95CI
    ) %>% 
  distinct()

#--------------- Saving 
dir_to_save <- paste0("V1/DEV/02_identify_seasonal_baseline/", Sys.Date()) # CHANGE FILEPATH TO SAVE AVE SEASONAL PROFILES
dir.create(dir_to_save)

write_csv(full_data_average_season, 
          file = paste0(dir_to_save, "/National_average_seasonal_profile.csv"))



#-------------------- ADD A LOG FOR WHETHER NATIONAL AVERAGE SEASONAL PROFILES HAVE BEEN UPDATED 