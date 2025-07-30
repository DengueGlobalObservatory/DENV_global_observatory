#' ---
#' title: "00 deduplicating_OpenDengue_data_FUN"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Extract data source category + where duplicate counts exist select the source with greater continuity for that year.    
#' 
#' Timeline:
#' =========
#' 14-05-2025: Prepared function.
#'

deduplicating_OpenDengue_data_FUN <- function(x){
  
  extract_before_second_dash <- function(x) {
    sapply(x, function(s) {
      dashes <- gregexpr("-", s)[[1]]
      if (length(dashes) < 2 || dashes[2] == -1) {
        return(s)
      } else {
        return(substr(s, 1, dashes[2] - 1))
      }
    })
  }
  
  x_clean <- x %>%
    mutate(Week = week(calendar_start_date)) %>%
    group_by(adm_0_name, Year, Week) %>%
    mutate(Number_of_counts_per_week = n()) %>% 
    mutate(Year_Week = paste0(Year, "_", Week),
           observation = row_number()) %>%
    ungroup() %>%
    mutate(data_source_cat = extract_before_second_dash(UUID)) %>%
    group_by(adm_0_name, Year, data_source_cat) %>% 
    mutate(source_count = dplyr::n()) %>% 
    ungroup() %>%
    group_by(adm_0_name, Year) %>%
    mutate(counts_highest_continity_source = max(source_count)) %>%
    ungroup() %>%
    mutate(week_to_keep = case_when(Number_of_counts_per_week == 1  ~ "Keep",
                                    Number_of_counts_per_week == 1 & source_count == counts_highest_continity_source ~ "Keep",
                                    Number_of_counts_per_week > 1 & source_count < counts_highest_continity_source ~ "Remove")) %>%
    filter(week_to_keep == "Keep")
  
  return(x_clean)
}