#' ---
#' title: "00 interpolating_missing_OpenDengue_data_FUN"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Interpolate missing data within OpenDengue dataset up to gaps of 4 weeks or 1 month.
#' Function processes input data depending on whether reporting is weekly or monthly or both at different points in time.     
#' 
#' Timeline:
#' =========
#' 14-05-2025: Prepared function.
#'

interpolating_missing_OpenDengue_data_FUN <- function(x) {
  
  x_filtered <- x %>% 
    filter(T_res != "Year")
  
  # Handle Monthly Resolution Only
  if (length(unique(x_filtered$T_res)) == 1 && unique(x_filtered$T_res) == "Month") {
    # Generate sequence of all months and years present in data
    years_in_data <- unique(x_filtered$Year)
    month.years <- lapply(years_in_data, function(y) {
      seq.Date(from = as.Date(paste0(y, "-01-01"), format = "%Y-%m-%d"),
               to = as.Date(paste0(y, "-12-01"), format = "%Y-%m-%d"),
               by = "month")}) %>%
      Reduce(c, .) %>% 
      as.data.frame()
    colnames(month.years) <- "Date"
    
    month.years <- month.years %>%
      mutate(Year = year(Date),
             Month = month(Date)) %>%
      mutate(Month.Year = paste0(Month, "-", Year))
    
    # Clean and interpolate
    x_clean <- x_filtered %>% 
      ungroup() %>% 
      mutate(Month = month(calendar_start_date)) %>% 
      mutate(Month.Year = paste0(Month, "-", Year)) 
    
    x_merged <- x_clean %>%
      merge(month.years, by = "Month.Year", all.y = TRUE) %>%
      mutate(Month.y = as.numeric(Month.y)) %>%
      arrange(Year.y, Month.y) %>% 
      ungroup()
    
    x_interpolated <- x_merged %>%
      select(adm_0_name, ISO_A0, calendar_start_date, calendar_end_date, Year.y, Month.y, Month.Year, dengue_total, T_res) %>%
      mutate(interpolated_cases = ceiling(na_interpolation(dengue_total, option = "linear", maxgap = 1))) %>%
      rename(Year = Year.y,
             Period = Month.y,
             Period.Year = Month.Year) %>%
      mutate(T_res = case_when(is.na(T_res) ~ "Week",
                               TRUE ~ T_res)) %>%
      filter(!is.na(interpolated_cases))
    
    # Clean interpolation results
    cleaning_interpolation_results <- function(x){
      country_name <- na.omit(unique(x$adm_0_name)) 
      ISO3_code <- na.omit(unique(x$ISO_A0))
      
      x_clean <- x %>%
        mutate(adm_0_name = country_name,
               ISO_A0 = ISO3_code,
               Previous_week_start_date = as.Date(c(NA, as.Date(x$calendar_start_date)[-length(x$calendar_start_date)]))) %>%
        mutate(calendar_start_date = case_when(is.na(calendar_start_date) ~ Previous_week_start_date + 7,
                                               TRUE ~ calendar_start_date)) %>%
        mutate(calendar_end_date = case_when(is.na(calendar_end_date) ~ calendar_start_date + 6,
                                             TRUE ~ calendar_end_date))
      
      return(x_clean)
    }
    x_interpolated_clean <- cleaning_interpolation_results(x_interpolated)
    
    return(x_interpolated_clean)
    
  }   else if(length(unique(x_filtered$T_res)) == 2) {
    # Handle Both Monthly and Weekly Data
    # Monthly Interpolation
    x_monthly <- x_filtered %>% filter(T_res == "Month")
    years_in_monthly_data <- unique(x_monthly$Year)
    
    month.years <- lapply(years_in_monthly_data, function(y) {
      seq.Date(from = as.Date(paste0(y, "-01-01"), format = "%Y-%m-%d"),
               to = as.Date(paste0(y, "-12-01"), format = "%Y-%m-%d"),
               by = "month")}) %>%
      Reduce(c, .) %>% as.data.frame()
    colnames(month.years) <- "Date"
    
    month.years <- month.years %>%
      mutate(Year = year(Date),
             Month = month(Date)) %>%
      mutate(Month.Year = paste0(Month, "-", Year))
    
    # Clean and interpolate
    x_monthly_clean <- x_filtered %>% 
      ungroup() %>%
      mutate(Month = month(calendar_start_date)) %>%
      mutate(Month.Year = paste0(Month, "-", Year)) 
    
    x_monthly_merged <- x_monthly_clean %>%
      merge(month.years, by = "Month.Year", all.y = TRUE) %>%
      mutate(Month.x = as.numeric(Month.x))
    
    x_monthly_interpolated <- x_monthly_merged %>%
      ungroup() %>%
      arrange(Year.y, Month.y) %>% 
      select(adm_0_name, ISO_A0, calendar_start_date, calendar_end_date, Year.y, Month.y, Month.Year, dengue_total, T_res)  %>%
      mutate(interpolated_cases = ceiling(na_interpolation(dengue_total, option = "linear", maxgap = 1))) %>%
      rename(Period.Year = Month.Year,
             Period = Month.y,
             Year = Year.y) %>%
      mutate(T_res = case_when(is.na(T_res) ~ "Month",
                               TRUE ~ T_res)) %>%
      filter(!is.na(interpolated_cases))
    
    # Weekly Interpolation
    x_weekly <- x_filtered %>% filter(T_res == "Week")
    years_in_weekly_data <- unique(x_weekly$Year)
    
    week.years <- lapply(years_in_weekly_data, function(y) {
      seq.Date(from = as.Date(paste0(y, "-01-01"), format = "%Y-%m-%d"),
               to = as.Date(paste0(y, "-12-31"), format = "%Y-%m-%d"),
               by = "week")}) %>%
      Reduce(c, .) %>% as.data.frame()
    colnames(week.years) <- "Date"
    
    week.years <- week.years %>%
      mutate(Year = year(Date),
             Week = week(Date)) %>% 
      mutate(Week.Year = paste0(Week, "-", Year))
    
    x_weekly_clean <- x_weekly %>% 
      ungroup() %>%
      mutate(Week = week(calendar_start_date)) %>%
      mutate(Week.Year = paste0(Week, "-", Year))
    
    if(nrow(x_weekly_clean) == 1){
      return(x_monthly_interpolated)
      
    } else if(nrow(x_weekly_clean) > 1){
      x_weekly_merged <- x_weekly_clean %>%
        merge(week.years, by = "Week.Year", all.y = TRUE) %>%
        mutate(Week.y = as.numeric(Week.y))
      
      x_weekly_interpolated <- x_weekly_merged %>%
        ungroup() %>%
        arrange(Year.y, Week.y) %>%
        select(adm_0_name, ISO_A0, calendar_start_date, calendar_end_date, Year.y, Week.y, Week.Year, dengue_total, T_res)  %>%
        mutate(interpolated_cases = ceiling(na_interpolation(dengue_total, option = "linear", maxgap = 4))) %>%
        rename(Period.Year = Week.Year,
               Period = Week.y,
               Year = Year.y) %>%
        mutate(T_res = case_when(is.na(T_res) ~ "Week",
                                 TRUE ~ T_res)) %>%
        filter(!is.na(interpolated_cases))
      
      # Combine Monthly and Weekly Interpolations
      x_monthly_weekly_combined <- rbind(x_weekly_interpolated, x_monthly_interpolated)
      
      # Clean interpolation results
      cleaning_interpolation_results <- function(x){
        country_name <- na.omit(unique(x$adm_0_name)) 
        ISO3_code <- na.omit(unique(x$ISO_A0))
        
        x_clean <- x %>%
          mutate(adm_0_name = country_name,
                 ISO_A0 = ISO3_code,
                 Previous_week_start_date = as.Date(c(NA, as.Date(x$calendar_start_date)[-length(x$calendar_start_date)]))) %>%
          mutate(calendar_start_date = case_when(is.na(calendar_start_date) ~ Previous_week_start_date + 7,
                                                 TRUE ~ calendar_start_date)) %>%
          mutate(calendar_end_date = case_when(is.na(calendar_end_date) ~ calendar_start_date + 6,
                                               TRUE ~ calendar_end_date))
        
        return(x_clean)
      }
      x_final <- cleaning_interpolation_results(x_monthly_weekly_combined)
      
      return(x_final)
    }
    
    
  } else if (length(unique(x_filtered$T_res)) == 1 && unique(x_filtered$T_res) == "Week") {
    # Handle Weekly Resolution Only
    years_in_data <- unique(x_filtered$Year)
    
    week.years <- lapply(years_in_data, function(y) {
      seq.Date(from = as.Date(paste0(y, "-01-01"), format = "%Y-%m-%d"),
               to = as.Date(paste0(y, "-12-31"), format = "%Y-%m-%d"),
               by = "week")}) %>%
      Reduce(c, .) %>% as.data.frame()
    colnames(week.years) <- "Date"
    
    week.years <- week.years %>%
      mutate(Year = year(Date),
             Week = week(Date),) %>%
      mutate(Week.Year = paste0(Week, "-", Year))
    
    x_weekly_clean <- x_filtered %>% 
      mutate(Week = week(calendar_start_date)) %>% 
      mutate(Week.Year = paste0(Week, "-", Year))
    
    x_weekly_merged <- x_weekly_clean %>%
      ungroup() %>%
      merge(week.years, by = c("Week.Year"), all.y = TRUE) %>%
      mutate(Week.x = as.numeric(Week.x))
    
    x_weekly_interpolated <- x_weekly_merged %>% 
      arrange(Year.y, Week.y) %>%
      select(adm_0_name, ISO_A0, calendar_start_date, calendar_end_date, Year.y, Week.y, Week.Year, dengue_total, T_res)  %>%
      mutate(interpolated_cases = ceiling(na_interpolation(dengue_total, option = "linear", maxgap = 4))) %>%
      rename(Year = Year.y,
             Period = Week.y,
             Period.Year = Week.Year) %>%
      mutate(T_res = case_when(is.na(T_res) ~ "Week",
                               TRUE ~ T_res)) %>%
      filter(!is.na(interpolated_cases))
    
    # Clean interpolation results
    cleaning_interpolation_results <- function(x){
      country_name <- na.omit(unique(x$adm_0_name)) 
      ISO3_code <- na.omit(unique(x$ISO_A0))
      
      x_clean <- x %>%
        mutate(adm_0_name = country_name,
               ISO_A0 = ISO3_code,
               Previous_week_start_date = as.Date(c(NA, as.Date(x$calendar_start_date)[-length(x$calendar_start_date)]))) %>%
        mutate(calendar_start_date = case_when(is.na(calendar_start_date) ~ Previous_week_start_date + 7,
                                               TRUE ~ calendar_start_date)) %>%
        mutate(calendar_end_date = case_when(is.na(calendar_end_date) ~ calendar_start_date + 6,
                                             TRUE ~ calendar_end_date))
      
      return(x_clean)
    }
    x_weekly_interpolated_clean <- cleaning_interpolation_results(x_weekly_interpolated)
    
    return(x_weekly_interpolated_clean)
  }
}
