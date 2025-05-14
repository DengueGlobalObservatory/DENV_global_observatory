#' ---
#' title: "00 interpolating_missing_WHO_data_FUN"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Function handles monthly Resolution Only - all WHO reporting at monthly T-res
#' WHO database includes a row for each month even if there is no data - filled with NA   
#' 
#' Timeline:
#' =========
#' 14-05-2025: Prepared function.
#'

interpolating_missing_WHO_data_FUN <- function(x) {

  # Clean and interpolate
  x_clean <- x %>% 
    ungroup() %>% 
    mutate(Month = month(date)) %>% 
    mutate(Month.Year = paste0(Month, "-", Year)) 
  
  x_interpolated <- x_clean %>%
    mutate(interpolated_cases = ceiling(na_interpolation(cases, option = "linear", maxgap = 1))) %>%
    filter(!is.na(interpolated_cases))
  
  return(x_interpolated)
}  