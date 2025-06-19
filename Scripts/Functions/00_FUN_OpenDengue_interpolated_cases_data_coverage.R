#' ---
#' title: "00 OpenDengue_interpolated_cases_data_coverage_FUN"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Calculate data coverage by country by year for OpenDengue. 
#' 
#' Timeline:
#' =========
#' 14-05-2025: Prepared function.
#'

OpenDengue_interpolated_cases_data_coverage_FUN <- function(OD_extract){
  OD_extract_coverage <- OD_extract %>%
    filter(!is.na(interpolated_cases)) %>% 
    select(adm_0_name, ISO_A0, calendar_start_date, calendar_end_date, Year, dengue_total, interpolated_cases, T_res) %>%
    group_by(Year, T_res) %>%
    summarize(National_extract_counts = n()) %>% as.data.frame()
  
  OD_extract_coverage_clean <- OD_extract_coverage %>%
    mutate(adm_0_name = na.omit(unique(OD_extract$adm_0_name)))
  
  return(OD_extract_coverage_clean)
}
