#' ---
#' title: "00_FUN_Desired_use_case_LOOCV_on_monthly_proportion_of_cases"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
#' Test performance of monthly case proportion as predictor in desired use case, i.e. predicting the monthly distribution of cases from each month observed and the total annual cases. 
#' Perform leave one year out cross validation and for the year in the test set iterate through the months generating a new prediction of monthly case distribution and total annual cases.  
#'
#' Timeline:
#' =========
#' 22-05-2025: Prepared scripts.
#' 

desired_use_case_LOOCV_on_monthly_proportion_of_cases <- function(x){
  
  #--------------- Calculate monthly proportions 
  x_monthly_proportions <- x %>% 
    group_by(season) %>% 
    mutate(Total_season_cases = sum(Cases_clean)) %>% 
    mutate(Actual_monthly_proportion = Cases_clean / Total_season_cases) %>% 
    ungroup() 
  
  #--------------- Predict annual cases at time point T using leave one season out CV 
  
  # Define groups by year
  groups <- unique(x_monthly_proportions$season)
  
  # Define df to take leave one out total seasonal case load results
  predicting_total_seasonal_case_load_results <- data.frame()
  
  for(i in groups){
    
    filtered_data <- x_monthly_proportions %>% 
      filter(season != i)
    
    average_proportion <- filtered_data %>% 
      group_by(season_nMonth) %>% 
      summarise(LOO_monthly_proportion = ave(Actual_monthly_proportion)) %>% 
      mutate(season = i) %>% 
      distinct()
    
    predicting_total_seasonal_case_load_results <- rbind(predicting_total_seasonal_case_load_results, 
                                                         average_proportion)
  }
  
  x_annual_cases_LOOCV <- full_join(x_monthly_proportions, predicting_total_seasonal_case_load_results, by= c("season", "season_nMonth")) %>%
    as.data.frame() %>% 
    mutate(Predicted_seasonal_cases = Cases_clean / LOO_monthly_proportion)
  
  #--------------- Predict monthly cases using LOOCV 
  
  # Define df to take leave one out season month case load prediction results
  predicting_monthly_case_load_results <- data.frame()
  
  for(i in groups){
    
    training_data <- x_monthly_proportions %>% 
      filter(season != i) %>% 
      group_by(season_nMonth) %>% 
      summarise(Ave_monthly_proportion = ave(Actual_monthly_proportion)) %>% 
      distinct() %>%
      arrange(season_nMonth) 
    
    test_data <- x_monthly_proportions %>% 
      filter(season == i) %>% 
      group_by(season_nMonth) 
    
    iterative_monthly_predictions <- data.frame()
    
    for(j in 1:12){
      
      monthly_filtered_data <- test_data[1:j,]
      monthly_predictions <- monthly_filtered_data %>% 
        ungroup() %>% 
        mutate(Cases_to_date = sum(Cases_clean)) %>% 
        full_join(., training_data, by = "season_nMonth") %>%
        mutate(season = i, 
               Predictor_month = j) %>%
        mutate(Total_season_cases = na.omit(unique(Total_season_cases))) %>% 
        mutate(Predicted_monthly_cases = Total_season_cases * Ave_monthly_proportion) %>% 
        filter(Predictor_month <= season_nMonth) 
  
      iterative_monthly_predictions <- rbind(iterative_monthly_predictions, 
                                             monthly_predictions)
                                             # monthly_predictions[1,] %>% 
                                             #   mutate(Cases_to_date = sum(Cases_clean),
                                             #          season = i,
                                             #          Predictor_month = NA,
                                             #          Total_season_cases = unique(Total_season_cases),
                                             #          Predicted_monthly_cases = NA))
      
    }
    
    predicting_monthly_case_load_results <- rbind(predicting_monthly_case_load_results,
                                                  iterative_monthly_predictions) %>% 
      distinct() %>%
      filter(!is.na(Country))
  }
  
  
  #--------------- Prepare results
  
  final_results = list(Annual_cases_LOOCV = x_annual_cases_LOOCV,
                       Monthly_cases_LOOCV = predicting_monthly_case_load_results)
  return(final_results)
  
}
