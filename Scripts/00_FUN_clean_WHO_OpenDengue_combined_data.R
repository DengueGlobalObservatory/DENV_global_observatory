#' ---
#' title: "00 clean_WHO_OpenDengue_combined_data_fun"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
# Clean combined WHO and OpenDengue dataset 
#'  
#' Timeline:
#' =========
#' 14-05-2025: Prepared function.
#' 15-05-2025: Updated function to regenerate country names from iso3 codes to ensure that all country names are consistent. 
#'

clean_WHO_OpenDengue_combined_data_fun <- function(x){
  x_clean <- x %>% 
    mutate(Country = countrycode(iso3, "iso3c", "country.name")) %>%
    group_by(Country, Year, Month) %>% 
    mutate(Number_of_obs = n()) %>%
    mutate(Max_val = which.max(Cases_clean)) %>%
    mutate(To_keep = case_when(Number_of_obs == 1 ~ "Keep",
                               Number_of_obs != 2 & Max_val == TRUE ~ "Keep",
                               Number_of_obs != 2 & Max_val == FALSE ~ "Remove")) %>% 
    filter(To_keep == "Keep") %>% 
    select(!To_keep & !Number_of_obs & !Max_val)

  return(x_clean)
  
}