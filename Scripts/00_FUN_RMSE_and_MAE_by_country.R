#' ---
#' title: "00 RMSE_and_MAE_by_country_fun"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
#' Function to calculate RMSE and MAE by country.
#'  
#' Timeline:
#' =========
#' 15-05-2025: Prepared function.
#'

RMSE_and_MAE_by_country_fun <- function(x){
  
  x_performance <- x %>% 
    ungroup() %>% 
    group_by(Country, iso3) %>% 
    summarise(RMSE = rmse(Cases_clean, Predicted_cases),
              MAE = mae(Cases_clean, Predicted_cases)) 
  
  return(x_performance)
    
}