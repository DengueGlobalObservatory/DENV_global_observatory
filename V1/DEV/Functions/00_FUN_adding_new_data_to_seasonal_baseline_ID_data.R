#' ---
#' title: "00_FUN_adding_new_data_to_seasonal_baseline_ID_data"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview:
#' ========
#' 
#' 
#' Timeline
#' ========

adding_new_data_to_seasonal_baseline_ID_data <- function(national_data_old, prediction_data){
  
  #--------------- Identify the most recent year of data by country 
  
  national_data_old_most_recent_year <- national_data_old %>%
    ungroup() %>% 
    group_by(Country, iso3) %>%
    slice_max(Year) %>% 
    select(Country, iso3, Year) %>%
    distinct()

  #--------------- Check whether a whole calendar year has passed since the national data was last updated
  # If not then no need to check for complete data from the prediction set 
  
  if(max(national_data_old_most_recent_year$Year) + 1 == year(Sys.Date())){
    print("A whole year hasn't passed since the last update.")
    return(national_data_old)
    
  } else if(max(national_data_old_most_recent_year$Year) + 1 < year(Sys.Date())){
    
    print("A whole year has passed since the last update. Checking whether complete data is available for the last calendar year for any countries.")
    
    #--- Check whether a new complete year is available for any country 
    prediction_data_complete_years <- prediction_data %>%
      ungroup() %>% 
      group_by(Country, iso3) %>%
      mutate(Obs_to_date = n()) %>%
      filter(Obs_to_date == 12)
    
    #--- If new data is available, add the data to the national_data_old df
    
    if(nrow(prediction_data_complete_years) > 0){
      print(paste0("New data is available for: ", cat(prediction_data_complete_years$Country, sep = ", ")))
      
      #--------------- Filter for the new year of data 
      new_year_data <- prediction_data %>% 
        filter(iso3 %in% prediction_data_complete_years$iso3)
      
      national_data_updated <- rbind(national_data_old, 
                                     new_year_data)
      
      return(national_data_updated)
    } else if(nrow(prediction_data_complete_years) == 0){
      
      print(paste0("No countries yet have a complete year of data."))
      return(national_data_old)
    }
  }
  
}

