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
#' 09-09-2025: Added code to parameterise negative binomial distribution from monthly data.
#'             Removed SD + CI95 calculation. 

library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(circular)
library(MASS)
library(purrr)


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
  
  # Remove years with all zeroes from low month identification - circular mean undefined.
  filter(sum(Cases_clean) > 0) %>%
  mutate(No_of_obs = n()) %>% 
  
  # Where multiple months have equally few cases calculate within-year circular mean 
  mutate(within_year_mean_low_month = 
           case_when(No_of_obs > 1 ~ circular_mean(Low_month),
                     No_of_obs == 1  ~ Low_month)) %>%
  dplyr::select(Country, 
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
  dplyr::select(!Number_of_months_in_season) %>%
  
  # Filter for seasons with >=5 cases per month on average
  mutate(Average_cases_per_month = ave(Cases_clean)) %>%
  filter(Average_cases_per_month >= monthly_ave_case_threshold) %>%
  dplyr::select(!Average_cases_per_month) %>% 
  ungroup() %>%
  
  # Filter for countries with at least three seasons. 
  group_by(Country) %>% 
  mutate(Number_of_seasons = n() / 12) %>% 
  filter(Number_of_seasons >= 3) %>% 
  ungroup() %>%
  dplyr::select(!Number_of_seasons)
  
#--------------- Identify average seasonal profile 

#----- Calculate monthly proportions by season
full_data_season_monthly_proportions <- full_data_filtered %>% 
    group_by(Country, iso3, season) %>% 
    arrange(season_nMonth) %>%
    mutate(
      Actual_cum_cases = cumsum(Cases_clean),
      Actual_monthly_proportion = Cases_clean / sum(Cases_clean),
      Actual_cum_monthly_proportion = cumsum(Actual_monthly_proportion)
      ) %>% 
    ungroup()
  
#----- Identify ave seasonal profile (log CIs)
  
full_data_average_season <- full_data_season_monthly_proportions %>% 
  group_by(Country, iso3, season_nMonth) %>% 
  mutate(
    # Identify average season in case space 
    Ave_season_monthly_cum_cases = mean(Actual_cum_cases),
    Ave_season_monthly_cases = mean(Cases_clean),
    
    # Define negative binomial 
    # nb_fit = list(
    #   tryCatch(
    #     fitdistr(Cases_clean, densfun = "negative binomial"),
    #     error = function(e){NULL}
    #   )),
    # 
    # # Extract negative binomial dist parameters 
    # nb_size = purrr::map_dbl(nb_fit, ~ ifelse(is.null(.x), NA_real_, .x$estimate["size"])),
    # nb_mu = purrr::map_dbl(nb_fit, ~ifelse(is.null(.x), NA_real_, .x$estimate["mu"])), 
    
    nb_fit = list(
      tryCatch(
        glm.nb(Cases_clean ~ 1),
        error = function(e){NULL}
      )
    ),
    nb_size = purrr::map_dbl(nb_fit, ~ ifelse(is.null(.x), NA_real_, .x$theta)),
    nb_mean = purrr::map_dbl(nb_fit, ~ ifelse(is.null(.x), NA_real_, exp(coef(.x)))), 
    
    # Identify average season in proportion space - for nowcasting/ prediction 
    Ave_monthly_proportion = mean(Actual_monthly_proportion),
    Ave_cum_monthly_proportion = mean(Actual_cum_monthly_proportion),
    ) %>% 
  
  arrange(Country, iso3, season_nMonth) %>%
  ungroup() %>%
  dplyr::select(
    # Identifiers 
    Country, iso3, Calendar_year_month, season_nMonth, 
    
    # Negative binomial parameters 
    nb_size, 
    nb_mean,
    
    # Case space ave season 
    Ave_season_monthly_cases, Ave_season_monthly_cum_cases,

    # Prop space ave season
    Ave_cum_monthly_proportion, 
    Ave_monthly_proportion
    ) %>% 
  distinct()

#--------------- Saving 
full_data_average_season <-full_data_average_season %>%
  dplyr::rename(
    country= Country,
    Month = Calendar_year_month
  )


write_csv(full_data_average_season, 
          file = paste0("V1/Output/seasonal_baseline/National_average_seasonal_profile_",Sys.Date() ,".csv")
          )



#-------------------- ADD A LOG FOR WHETHER NATIONAL AVERAGE SEASONAL PROFILES HAVE BEEN UPDATED 