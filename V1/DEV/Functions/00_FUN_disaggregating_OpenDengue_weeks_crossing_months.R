#' ---
#' title: "00 disaggregating_OpenDengue_weeks_crossing_months"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
#' Allocate cases in weeks crossing months to their respective months.
#'  
#' Timeline:
#' =========
#' 14-05-2025: Prepared function.
#'

disaggregating_OpenDengue_weeks_crossing_months <- function(x){
  
  # separate weekly, monthly and weekly + monthly reporting 
  if(length(unique(x$T_res)) == 1 && unique(x$T_res) == "Month"){
    
    # if only reporting T-res is monthly no handling needed
    x_filtered <- x %>%
      select(adm_0_name, ISO_A0, calendar_start_date, calendar_end_date, Year, dengue_total, T_res, interpolated_cases) %>%
      mutate(interpolated_cases_clean = interpolated_cases)
    
    return(x_filtered)
    
  } else if(length(unique(x$T_res)) == 1 && unique(x$T_res) == "Week"){
    
    x_clean <- x %>%
      select(adm_0_name, ISO_A0, calendar_start_date, calendar_end_date, Year, dengue_total, T_res, interpolated_cases) %>%
      mutate(calendar_start_month = month(calendar_start_date),
             calendar_end_month = month(calendar_end_date)) %>%
      mutate(same_start_end_month = case_when(calendar_start_month == calendar_end_month ~ "Same",
                                              calendar_start_month != calendar_end_month ~ "Different")) 
    
    weeks_not_crossing_months <- x_clean %>%
      filter(same_start_end_month == "Same")
    
    weeks_crossing_months <- x_clean %>%
      filter(same_start_end_month == "Different") %>%
      mutate(month_start_date = floor_date(calendar_end_date, unit = "month"),
             month_end_date = ceiling_date(calendar_start_date, unit = "month") - 1)
    
    weeks_crossing_months_week_start_to_month_end <- weeks_crossing_months %>%
      mutate(week_start_month_end_diff = month_start_date - calendar_start_date) %>%
      mutate(calendar_end_date = calendar_start_date + week_start_month_end_diff - 1) %>%
      mutate(interpolated_cases_clean = interpolated_cases*as.numeric(week_start_month_end_diff)/7) %>%
      select(!week_start_month_end_diff & !month_start_date &!month_end_date)
    
    weeks_crossing_months_to_month_start_to_week_end <- weeks_crossing_months %>%
      mutate(month_start_week_end_diff = calendar_end_date - month_end_date) %>%
      mutate(calendar_start_date = calendar_end_date - month_start_week_end_diff + 1) %>%
      mutate(interpolated_cases_clean = interpolated_cases*as.numeric(month_start_week_end_diff)/7) %>%
      select(!month_start_week_end_diff & !month_start_date & !month_end_date)
    
    x_final <- weeks_not_crossing_months %>% 
      mutate(interpolated_cases_clean = interpolated_cases) %>%
      rbind(., weeks_crossing_months_week_start_to_month_end, weeks_crossing_months_to_month_start_to_week_end) %>%
      select(!calendar_start_month & !calendar_end_month & !same_start_end_month)
    
    return(x_final)
  } else if(length(unique(x$T_res)) == 2){
    
    x_monthly <- x %>%
      filter(T_res == "Month") %>%
      select(adm_0_name, ISO_A0, calendar_start_date, calendar_end_date, Year, dengue_total, T_res, interpolated_cases) %>%
      mutate(interpolated_cases_clean = interpolated_cases)
    
    x_weekly <- x %>%
      filter(T_res == "Week") %>%
      select(adm_0_name, ISO_A0, calendar_start_date, calendar_end_date, Year, dengue_total, T_res, interpolated_cases) %>% 
      mutate(calendar_start_month = month(calendar_start_date),
             calendar_end_month = month(calendar_end_date)) %>%
      mutate(same_start_end_month = case_when(calendar_start_month == calendar_end_month ~ "Same",
                                              calendar_start_month != calendar_end_month ~ "Different")) 
    
    weeks_not_crossing_months <- x_weekly %>%
      filter(same_start_end_month == "Same")
    
    weeks_crossing_months <- x_weekly %>%
      filter(same_start_end_month == "Different") %>%
      mutate(month_start_date = floor_date(calendar_end_date, unit = "month"),
             month_end_date = ceiling_date(calendar_start_date, unit = "month") - 1)
    
    weeks_crossing_months_week_start_to_month_end <- weeks_crossing_months %>%
      mutate(week_start_month_end_diff = month_start_date - calendar_start_date) %>%
      mutate(calendar_end_date = calendar_start_date + week_start_month_end_diff - 1) %>%
      mutate(interpolated_cases_clean = interpolated_cases*as.numeric(week_start_month_end_diff)/7) %>%
      select(!week_start_month_end_diff & !month_start_date &!month_end_date)
    
    weeks_crossing_months_to_month_start_to_week_end <- weeks_crossing_months %>%
      mutate(month_start_week_end_diff = calendar_end_date - month_end_date) %>%
      mutate(calendar_start_date = calendar_end_date - month_start_week_end_diff + 1) %>%
      mutate(interpolated_cases_clean = interpolated_cases*as.numeric(month_start_week_end_diff)/7) %>%
      select(!month_start_week_end_diff & !month_start_date & !month_end_date)
    
    x_weekly_clean <-  weeks_not_crossing_months %>% 
      mutate(interpolated_cases_clean = interpolated_cases) %>%
      rbind(., weeks_crossing_months_week_start_to_month_end, weeks_crossing_months_to_month_start_to_week_end) %>%
      select(!calendar_start_month & !calendar_end_month & !same_start_end_month)
    
    x_final <- rbind(x_monthly, x_weekly_clean)
    return(x_final)
  }
}
