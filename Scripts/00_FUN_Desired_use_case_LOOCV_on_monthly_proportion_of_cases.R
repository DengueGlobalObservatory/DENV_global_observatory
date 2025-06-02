#' ---
#' title: "00_FUN_Desired_use_case_LOOCV_on_monthly_proportion_of_cases"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
#' Test performance of monthly case proportion as predictor in desired use case, i.e. predicting the monthly distribution of cases from each month observed and the total seasonal cases. 
#' Perform leave one year out cross validation and for the season in the test set iterate through the months generating a new prediction of monthly case distribution and total seasonal
#' cases.  
#'
#' Timeline:
#' =========
#' 22-05-2025: Prepared scripts.
#' 23-05-2025: Fixed calculation of monthly cases iteratively portion of function.
#' 27-05-2025: Swapped from using monthly cases to cumulative cases for prediction of total seasonal cases, and monthly cases.
#' 28-05-2025: Removed redundant code lines RE cumulative monthly prop being calculated twice.
#' 02-06-2025: Swapped incorrect names from annual --> seasonal predictions/ cases.

desired_use_case_LOOCV_on_monthly_proportion_of_cases <- function(x){
  
  #--------------- Calculate monthly proportions 
  x_monthly_proportions <- x %>% 
    group_by(season) %>% 
    mutate(Cumulative_season_cases = cumsum(Cases_clean),
           Total_season_cases = sum(Cases_clean), 
           Actual_monthly_proportion = Cases_clean / Total_season_cases) %>% 
    arrange(season_nMonth) %>%
    mutate(Actual_cumulative_monthly_proportion = cumsum(Actual_monthly_proportion)) %>%
    ungroup() 
  
  #--------------- Predict total seasonal cases at time point T using leave one season out CV 
  
  # Define groups by year
  groups <- unique(x_monthly_proportions$season)
  
  # Define df to take leave one out total seasonal case load results
  predicting_total_seasonal_case_load_results <- data.frame()
  
  for(i in groups){
    
    filtered_data <- x_monthly_proportions %>% 
      filter(season != i)
    
    average_proportion <- filtered_data %>% 
      group_by(season_nMonth) %>% 
      summarise(LOO_cum_monthly_proportion = ave(Actual_cumulative_monthly_proportion)) %>% 
      mutate(season = i) %>% 
      distinct()
    
    predicting_total_seasonal_case_load_results <- rbind(predicting_total_seasonal_case_load_results, 
                                                         average_proportion)
  }
  
  x_seasonal_cases_LOOCV <- full_join(x_monthly_proportions, predicting_total_seasonal_case_load_results, by= c("season", "season_nMonth")) %>%
    as.data.frame() %>% 
    mutate(Predicted_seasonal_cases = Cumulative_season_cases / LOO_cum_monthly_proportion)
  
  #--------------- Predict monthly cases using LOOCV 
  
  # Define df to take leave one out season month case load prediction results
  predicting_monthly_case_load_results <- data.frame()
  
  for(i in groups){
    
    training_data <- x_monthly_proportions %>% 
      filter(season != i) %>% 
      ungroup() %>%
      group_by(season_nMonth) %>% 
      summarise(Ave_monthly_proportion = ave(Actual_monthly_proportion),
                Average_cum_proportion = ave(Actual_cumulative_monthly_proportion)) %>% 
      arrange(season_nMonth) %>%
      ungroup() %>%
      rename(Month_to_predict = season_nMonth)
    
    test_data <- x_monthly_proportions %>% 
      filter(season == i) %>% 
      arrange(season_nMonth) %>%
      select(!Total_season_cases)
    
    iterative_monthly_predictions <- data.frame()
    Country <- unique(test_data$Country)
    iso3 <- unique(test_data$iso3)
    season <- unique(test_data$season)
    
    
    for(j in 1:12){
      
      monthly_filtered_data <- test_data[1:j,]
      monthly_predictions <- monthly_filtered_data %>% 
        ungroup() %>% 
        mutate(season = i, 
               Month_to_predict = j) %>%
        full_join(., training_data, by = "Month_to_predict") %>% 
        na.omit() %>%
        distinct() %>%
        filter(season_nMonth != Month_to_predict) %>%
        mutate(Actual_cases_to_date = cumsum(Cases_clean)) %>%
        mutate(Predicted_total_seasonal_cases = Actual_cases_to_date / Average_cum_proportion) %>% 
        mutate(Predicted_monthly_cases = Predicted_total_seasonal_cases * Ave_monthly_proportion)
        
      #  filter(Predictor_month < season_nMonth)  %>%
      #   mutate(Cases_to_date = sum(Cases_clean),
      #          Average_cum_proportion_to_date = sum()) %>% 
      #   mutate(Predicted_total_season_cases = Cases_to_date / ) %>% 
      # 
      # monthly_predictions <- monthly_filtered_data %>% 
      #   ungroup() %>% 
      #   mutate(Cases_to_date = sum(Cases_clean)) %>% 
      #   full_join(., training_data, by = "season_nMonth") %>%
      #    mutate(season = i, 
      #           Predictor_month = j) %>%
      #   mutate(Total_season_cases = na.omit(unique(Total_season_cases))) %>% 
      #   filter(Predictor_month > season_nMonth) #
        # mutate(Predicted_monthly_cases = Total_season_cases * Ave_monthly_proportion) %>% 
        # filter(Predictor_month < season_nMonth) 
  
      iterative_monthly_predictions <- rbind(iterative_monthly_predictions, 
                                             monthly_predictions) %>%
        distinct()
      
      Actual_monthly_cases <- test_data %>% 
        select(season, season_nMonth, Cases_clean) %>% 
        rename(Month_to_predict = season_nMonth)
      
      iterative_monthly_predictions_clean <- iterative_monthly_predictions %>% 
        select(!Cases_clean) %>%
        left_join(., Actual_monthly_cases, by = c("season", "Month_to_predict")) %>%
        distinct() %>%
        rename(Actual_monthly_cases = Cases_clean) %>% 
        select(Country, iso3, source, Year, Calendar_year_month, mean_low_month, season, season_nMonth, Actual_cases_to_date, Actual_monthly_proportion, 
               Month_to_predict, Ave_monthly_proportion, Average_cum_proportion, Predicted_total_seasonal_cases, Actual_monthly_cases, Predicted_monthly_cases) %>%
        select(Country, iso3, source, Year, Calendar_year_month, mean_low_month, season, season_nMonth, 
               Actual_cases_to_date, Month_to_predict, Actual_monthly_cases, Predicted_monthly_cases)
      
      
    }
    
    predicting_monthly_case_load_results <- rbind(predicting_monthly_case_load_results,
                                                  iterative_monthly_predictions_clean)  
  }
  
  
  #--------------- Prepare results
  
  final_results = list(Seasonal_cases_LOOCV = x_seasonal_cases_LOOCV,
                       Monthly_cases_LOOCV = predicting_monthly_case_load_results)
  return(final_results)
  
}
