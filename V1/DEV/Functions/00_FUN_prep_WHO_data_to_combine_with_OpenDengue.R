#' ---
#' title: "00 prep_WHO_data_to_combine_with_OpenDengue_fun"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
#' Prepare WHO data to add to OpenDengue data 
#'  
#' Timeline:
#' =========
#' 14-05-2025: Prepared function.
#'

prep_WHO_data_to_combine_with_OpenDengue_fun <- function(x){
  
  x_df <- Reduce(rbind, x) %>%
    as.data.frame() %>%
    select(country, iso3, Month, Year, interpolated_cases) %>%
    rename(Country = country,
           Cases_clean = interpolated_cases) %>%
    mutate(source = "WHO")
  
  return(x_df)
}