#' ---
#' title: "00_FUN_identify_average_seasonal_profile"
#' author: "K Joshi"
#' 
#' ---
#'
#' Overview:
#' ========
#' 
#' 

identify_average_seasonal_profile <- function(x){
  
  #--------------- Calculate monthly proportions by season
  x_monthly_proportions <- x %>% 
    group_by(Country, iso3, season) %>% 
    mutate(Cumulative_season_cases = cumsum(Cases_clean),
           Total_season_cases = sum(Cases_clean), 
           Actual_monthly_proportion = Cases_clean / Total_season_cases) %>% 
    arrange(season_nMonth) %>%
    mutate(Actual_cumulative_monthly_proportion = cumsum(Actual_monthly_proportion)) %>%
    ungroup()
  
  #--------------- Identify ave seasonal profile (log CIs)
  
  # offset to avoid log(0)
  epsilon <- 1e-6
  
  x_ave_seasonal_profile <- x_monthly_proportions %>% 
    ungroup() %>%
    group_by(Country, iso3, season_nMonth) %>% 
    mutate(Ave_monthly_proportion = mean(Actual_monthly_proportion),
              
      # Log-transform proportions (+epsilon), compute mean and sd on log scale
      log_mean = mean(log(Actual_monthly_proportion + epsilon)),
      log_sd = sd(log(Actual_monthly_proportion + epsilon)),
      n = n(),
      
      # Calculate log-scale 95% CI bounds (using normal approx)
      log_CI_lower = log_mean - 1.96 * log_sd / sqrt(n),
      log_CI_upper = log_mean + 1.96 * log_sd / sqrt(n),
      
      # Back-transform CIs to original scale
      CI95_monthly_proportion_lower = exp(log_CI_lower),
      CI95_monthly_proportion_upper = exp(log_CI_upper),
      
      Average_cum_proportion = mean(Actual_cumulative_monthly_proportion),
      CI95_cum_proportion = sd(Actual_cumulative_monthly_proportion) * 1.96) %>% 
    arrange(Country, iso3, season_nMonth) %>%
    ungroup() %>%
    select(Country, iso3, Calendar_year_month, season_nMonth, 
           Ave_monthly_proportion, CI95_monthly_proportion_lower, CI95_monthly_proportion_upper, 
           Average_cum_proportion, CI95_cum_proportion) %>% 
  
  
  # #--------------- Identify ave seasonal profile
  # x_ave_seasonal_profile <- x_monthly_proportions %>% 
  #   ungroup() %>%
  #   group_by(Country, iso3, season_nMonth) %>% 
  #   mutate(Ave_monthly_proportion = ave(Actual_monthly_proportion),
  #          CI95_monthly_proportion = (sd(Actual_monthly_proportion) *1.96),
  #          Average_cum_proportion = ave(Actual_cumulative_monthly_proportion),
  #          CI95_cum_proportion = sd(Actual_cumulative_monthly_proportion) * 1.96) %>% 
  #   arrange(Country, iso3, season_nMonth) %>%
  #   ungroup() %>%
  #   select(Country, iso3, Calendar_year_month, season_nMonth, Ave_monthly_proportion, CI95_monthly_proportion, Average_cum_proportion, CI95_cum_proportion) %>% 
  #   distinct()
  
  return(x_ave_seasonal_profile)

}