#' ---
#' title: "00 prep_OpenDengue_data_to_combine_with_WHO_fun"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
#' Prepare OpenDengue data to add to WHO data.
#'  
#' Timeline:
#' =========
#' 14-05-2025: Prepared function.
#'

prep_OpenDengue_data_to_combine_with_WHO_fun <- function(x){
  
  x_df <- Reduce(rbind, x) %>%
    as.data.frame() %>%
    arrange(adm_0_name, calendar_start_date) %>%
    mutate(monthly_cases_clean = round(monthly_cases),
           Month = month(calendar_start_date)) %>%
    select(adm_0_name, ISO_A0, Month, Year, monthly_cases_clean) %>%
    rename(Country = adm_0_name,
           iso3 = ISO_A0,
           Cases_clean = monthly_cases_clean) %>%
    mutate(source = "OpenDengue") %>%
    distinct()
    
  return(x_df)
}