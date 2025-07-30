#' ---
#' title: "00_FUN_two_month_ahead_predictions"
#' author: "K Joshi"
#' 
#' ---
#'
#' Overview:
#' ========
#' 

two_month_ahead_predictions <- function(national_ave_seasonal_profile, prediction_data){
  
  current_month <- month(Sys.Date())
  
  x_predictions <- national_ave_seasonal_profile %>% 
    ungroup() %>% 
    left_join(., prediction_data, join_by("Calendar_year_month" == "Month", "Country", "iso3")) %>% #---------- CHANGE BY ARGUMENT AS NECESSARY
    distinct() %>%
    arrange(Country, iso3, Calendar_year_month) %>%
    mutate(End_prediction_month = current_month + 2) %>%
    filter(Calendar_year_month <= End_prediction_month) %>%
    group_by(Country, iso3) %>% 
    mutate(Actual_cases_to_date = cumsum(Cases_clean)) %>%
    mutate(Data_status = case_when(is.na(Cases_clean) ~ "Predicted",
                                   !is.na(Cases_clean) ~ "Observed")) %>%
    group_by(Country, iso3, Data_status) %>%
    mutate(most_recent_month = case_when(Calendar_year_month == max(Calendar_year_month) & !is.na(Cases_clean) ~ "Most_recent",
                                         Calendar_year_month != max(Calendar_year_month) & !is.na(Cases_clean) ~ "Not_most_recent",
                                         is.na(Cases_clean) ~ "No_data")) %>%
    mutate(Predicted_total_seasonal_cases = case_when(most_recent_month == "Most_recent" ~ Actual_cases_to_date / Average_cum_proportion,
                                                      most_recent_month == "Not_most_recent" ~ NA,
                                                      most_recent_month == "No_data" ~ NA)) %>%
    ungroup() %>% 
    group_by(Country, iso3) %>% 
    mutate(Any_data_available = case_when(length(unique(na.omit(Predicted_total_seasonal_cases))) == 0 ~ "No_data_available",
                                          length(unique(na.omit(Predicted_total_seasonal_cases))) > 0 ~ "Data_available")) %>% 
    mutate(Predicted_total_seasonal_cases = case_when(Any_data_available == "No_data_available" ~ Predicted_total_seasonal_cases,
                                                      Any_data_available == "Data_available" ~ first(na.omit(Predicted_total_seasonal_cases)))) %>%
    mutate(Predicted_monthly_cases = Predicted_total_seasonal_cases * Ave_monthly_proportion) %>%
    mutate(Predicted_monthly_cases_lower_CI95 = Predicted_total_seasonal_cases * CI95_monthly_proportion_lower,
           Predicted_monthly_cases_upper_CI95 = Predicted_total_seasonal_cases * CI95_monthly_proportion_upper)
  
  return(x_predictions)
}