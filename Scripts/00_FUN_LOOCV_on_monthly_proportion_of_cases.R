#' ---
#' title: "00 LOOCV_on_monthly_proportion_of_cases_fun"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
#' Function to calculate LOOCV using monthly proportion of cases to predict a given year. 
#'  
#' Timeline:
#' =========
#' 14-05-2025: Prepared function + added header description.
#'

LOOCV_on_monthly_proportion_of_cases_fun <- function(x){
  
  # Calculate monthly proportions 
  x_monthly_proportions <- x %>% 
    group_by(Year) %>% 
    mutate(Total_annual_cases = sum(Cases_clean)) %>% 
    mutate(Actual_monthly_proportion = Cases_clean / Total_annual_cases) %>% 
    ungroup() 
  
  # Define groups by year
  groups <- unique(x_monthly_proportions$Year)
  
  # Define df to take leave one out monthly proportions
  results <- data.frame()

  for(i in groups){
    
    filtered_data <- x_monthly_proportions %>% 
      filter(Year != i)
    
    average_proportion <- filtered_data %>% 
      group_by(Month) %>% 
      summarise(LOO_monthly_proportion = ave(Actual_monthly_proportion)) %>% 
      mutate(Year = i) %>% 
      distinct()
    
    results <- rbind(results, 
                     average_proportion)
  }
  
  x_monthly_proportions_LOOCV <- full_join(x_monthly_proportions, results, by= c("Year", "Month")) %>%
    as.data.frame() %>% 
    mutate(Predicted_cases = Total_annual_cases * LOO_monthly_proportion)
  
  return(x_monthly_proportions_LOOCV)
  
}