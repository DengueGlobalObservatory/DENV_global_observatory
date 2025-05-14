#' ---
#' title: "00 weekly_to_monthly_aggregation_OpenDengue_fun"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
#' Aggregate weekly to monthly data
#'  
#' Timeline:
#' =========
#' 14-05-2025: Prepared function.
#'

weekly_to_monthly_aggregation_OpenDengue_fun <- function(x){
  x <- x %>% 
    filter(T_res == "Week" | 
             T_res == "Month") %>%
    ungroup()
  
  if(length(unique(x$T_res)) == 1 && unique(x$T_res) == "Month"){
    x_clean <- x %>% 
      mutate(monthly_cases = interpolated_cases_clean)
    return(x_clean)
    
  } else if(length(unique(x$T_res)) == 1 && unique(x$T_res) == "Week"){
    
    x_clean <- x %>%
      mutate(Month = month(calendar_start_date)) %>%
      mutate(Month.Year = paste0(Month, ".", Year))
    
    x_aggregated <- x_clean %>% 
      mutate(Month = month(calendar_start_date)) %>%
      group_by(Month, Year) %>%
      summarize(monthly_cases = sum(interpolated_cases_clean)) %>%
      mutate(Month.Year = paste0(Month, ".", Year))
    
    x_aggregated_final <- x_clean %>%
      merge(x_aggregated, by = "Month.Year") %>%
      rename(Year = Year.x,
             Month = Month.x) %>%
      select(c(colnames(x_clean), "monthly_cases")) 
    
    x_final <- x_aggregated_final %>%
      select(!Month & ! Month.Year)
    
    return(x_final)
  } else if(length(unique(x$T_res)) == 2){
    
    x_monthly <- x %>%
      ungroup() %>%
      filter(T_res == "Month") %>%
      mutate(monthly_cases = interpolated_cases_clean)
    
    x_weekly <- x %>%
      ungroup() %>%
      filter(T_res == "Week") %>%
      mutate(Month = month(calendar_start_date)) %>%
      mutate(Month.Year = paste0(Month, ".", Year))
    
    x_weekly_aggregated <- x_weekly %>%
      group_by(Month, Year) %>%
      summarize(monthly_cases = sum(interpolated_cases_clean)) %>%
      mutate(Month.Year = paste0(Month, ".", Year))
    
    x_weekly_aggregated_final <- x_weekly %>%
      merge(x_weekly_aggregated, by = "Month.Year") %>%
      rename(Year = Year.x) %>%
      select(colnames(x_monthly))
    
    x_final <- rbind(x_weekly_aggregated_final, x_monthly)
    return(x_final)
  }
}
