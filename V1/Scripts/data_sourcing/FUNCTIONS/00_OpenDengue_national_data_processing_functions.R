#' ---
#' title: "00_OpenDengue_national_data_processing_functions"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
#' Functions needed to process national OpenDengue data:
#'    Deduplication 
#'    Temporal interpolation 
#'    Assess data coverage
#'    Disaggregating weeks crossing months + allocating days to their respective months. 
#'     
#' 
#' Timeline:
#' =========
#' 19-09-2025: Initial commit. 
#' 23-09-2025: Corrected fill_weekly_missing_entries function to account for differences in intra-annual date_sequence alignment.

deduplicate_OD_data <- function(OD_national){
  
  extract_before_second_dash <- function(x) {
    second_dash_pos <- gregexpr("-", x)[[1]][2]
    
    if(is.na(second_dash_pos)){return(x)}
    
    return(substr(x, 1, second_dash_pos - 1))
  }
  
  # Are observations duplicated
  OD_national_clean <- OD_national %>%
    dplyr::mutate(
      Week = week(calendar_start_date),
      data_source_cat = extract_before_second_dash(UUID)
    ) %>% 
    
    add_count(
      adm_0_name, adm_1_name, adm_2_name, Year, Week,
      name = "Number_of_counts_per_week") %>% 
    
    # Identify obs by source by year 
    add_count(adm_0_name, adm_1_name, adm_2_name, Year, data_source_cat, 
              name = "source_count")
  
  # Identify highest continuity source by country by year 
  max_continuity <-  OD_national_clean %>% 
    group_by(
      adm_0_name, adm_1_name, adm_2_name, Year) %>%
    dplyr::summarise(
      counts_highest_continuity_source = max(source_count), .groups = "drop")
  
  # Where duplicate counts exist retain obs from highest continuity source 
  OD_national_dedup <- OD_national_clean %>% 
    left_join(
      .,
      max_continuity, 
      by = c("adm_0_name", "adm_1_name", "adm_2_name", "Year")
    ) %>% 
    dplyr::mutate(
      week_to_keep = case_when(Number_of_counts_per_week == 1  ~ "Keep",
                               Number_of_counts_per_week = 1 & source_count == counts_highest_continuity_source ~ "Keep",
                               Number_of_counts_per_week > 1 & source_count < counts_highest_continuity_source ~ "Remove")
    ) %>%
    dplyr::filter(week_to_keep == "Keep") %>% 
    dplyr::select(-counts_highest_continuity_source, -week_to_keep, -source_count, -Number_of_counts_per_week, -data_source_cat)
  
  return(OD_national_dedup)
}

#----- Interpolation

generate_month_sequence <- function(years) {
  map_dfr(years, ~ {
    tibble(
      Date = seq.Date(
        from = as.Date(paste0(.x, "-01-01")),
        to = as.Date(paste0(.x, "-12-01")),
        by = "month"
      )
    ) %>%
      dplyr::mutate(
        Year = year(Date),
        Month = month(Date),
      )
  })
}

generate_week_sequence <- function(years) {
  map_dfr(years, ~ {
    tibble(
      Date = seq.Date(
        from = as.Date(paste0(.x, "-01-01")),
        to = as.Date(paste0(.x, "-12-31")),
        by = "week"
      )
    ) %>%
      dplyr::mutate(
        Year = year(Date),
        Week = week(Date),
      )
  })
}

fill_monthly_missing_entries <- function(OpenDengue_data) {
  
  has_ISO_A0 <- "ISO_A0" %in% names(OpenDengue_data) && !all(is.na(OpenDengue_data$ISO_A0))
  has_adm_0_name <- "adm_0_name" %in% names(OpenDengue_data) && !all(is.na(OpenDengue_data$adm_0_name))
  
  result <- OpenDengue_data %>%
    dplyr::group_by(Year) %>%
    dplyr::mutate(
      calendar_start_date = case_when(
        is.na(calendar_start_date) ~ as.Date(x = paste0("01-", Period, "-", Year), format = "%d-%m-%Y"),
        TRUE ~ calendar_start_date),
      
      calendar_end_date = case_when(
        is.na(calendar_end_date) ~ ceiling_date(calendar_start_date, unit = "month"),
        TRUE ~ calendar_end_date)
    )
  
  if (has_ISO_A0) {
    result <- result %>%
      dplyr::mutate(ISO_A0 = unique(na.omit(ISO_A0)))
  }
  if (has_adm_0_name) {
    result <- result %>%
      dplyr::mutate(adm_0_name = unique(na.omit(adm_0_name)))
  }
  
  result <- result %>%
    ungroup()
  
  return(result)
}
{
# fill_weekly_missing_entries <- function(OpenDengue_data) {
#   
#   has_ISO_A0 <- "ISO_A0" %in% names(OpenDengue_data) && !all(is.na(OpenDengue_data$ISO_A0))
#   has_adm_0_name <- "adm_0_name" %in% names(OpenDengue_data) && !all(is.na(OpenDengue_data$adm_0_name))
#   
#   result <- OpenDengue_data %>%
#     dplyr::group_by(Year) %>%
#     dplyr::mutate(
#       date_diff = unique(na.omit(calendar_start_date - Date)),
#       calendar_start_date = if_else(
#         is.na(calendar_start_date),
#         Date + date_diff,
#         calendar_start_date
#       ),
#       calendar_end_date = if_else(
#         is.na(calendar_end_date),
#         calendar_start_date + 6,
#         calendar_end_date
#       )
#     )
#   
#   if (has_ISO_A0) {
#     result <- result %>%
#       dplyr::mutate(ISO_A0 = unique(na.omit(ISO_A0)))
#   }
#   if (has_adm_0_name) {
#     result <- result %>%
#       dplyr::mutate(adm_0_name = unique(na.omit(adm_0_name)))
#   }
#   
#   result %>%
#     ungroup() %>%
#     dplyr::select(-date_diff, -Date)
# }
}
fill_weekly_missing_entries <- function(OpenDengue_data) {
  
  has_ISO_A0 <- "ISO_A0" %in% names(OpenDengue_data) && !all(is.na(OpenDengue_data$ISO_A0))
  has_adm_0_name <- "adm_0_name" %in% names(OpenDengue_data) && !all(is.na(OpenDengue_data$adm_0_name))
  
  result <- OpenDengue_data %>%
    arrange(Year, Date) %>% 
    ungroup() %>%
    group_by(Year) %>%
    dplyr::mutate(       
      # Identify whether a year has multiple diff_dates
      date_diff = as.integer(calendar_start_date - Date),
      multiple_date_diff = ifelse(length(unique(na.omit(date_diff))) > 1, TRUE, FALSE),
      date_diff = ifelse(multiple_date_diff, date_diff, unique(na.omit(date_diff)))) %>% 
    ungroup() %>% 
    mutate(
      # Where there are multiple diff_dates identify the boundaries
      missing_start = is.na(calendar_start_date) & !lag(is.na(calendar_start_date), default = FALSE),
      missing_end   = is.na(calendar_start_date) & !lead(is.na(calendar_start_date), default = FALSE)) %>% 
    
    mutate(
      # Fill boundary start and end dates
      calendar_start_date = case_when(multiple_date_diff & is.na(calendar_start_date) & missing_start ~ lag(calendar_end_date) + 1,
                                      !multiple_date_diff & is.na(calendar_start_date) ~ Date + date_diff,
                                      TRUE ~ calendar_start_date),
      calendar_end_date = case_when(multiple_date_diff & is.na(calendar_end_date) & missing_end  ~ lead(calendar_start_date) - 1,
                                    !multiple_date_diff & is.na(calendar_end_date) ~ calendar_start_date + 6,
                                    TRUE ~ calendar_end_date)) %>%
    
    mutate(
      # Fill boundary week end and start dates  
      calendar_start_date = case_when(multiple_date_diff & is.na(calendar_start_date) & missing_end ~ calendar_end_date - 6,
                                      TRUE ~ calendar_start_date),
      calendar_end_date = case_when(multiple_date_diff & is.na(calendar_end_date) & missing_start ~ calendar_start_date + 6,
                                    TRUE ~ calendar_end_date)) %>% 
    
    mutate(
      # Fill middle missing data
      calendar_start_date = case_when(multiple_date_diff & is.na(calendar_start_date) & !missing_end & !missing_start ~ lag(calendar_end_date) + 1,
                                      TRUE ~ calendar_start_date),
      calendar_end_date = case_when(multiple_date_diff & is.na(calendar_end_date) & !missing_end & !missing_start ~ lead(calendar_start_date) - 1,
                                    TRUE ~ calendar_end_date)
    ) 
  
  if (has_ISO_A0) {
    result <- result %>%
      dplyr::mutate(ISO_A0 = unique(na.omit(ISO_A0)))
  }
  
  if (has_adm_0_name) {
    result <- result %>%
      dplyr::mutate(adm_0_name = unique(na.omit(adm_0_name)))
  }
  
  result <- result %>%
    ungroup() #%>%
  #dplyr::select(-date_diff, -multiple_date_diff, -missing_start , -missing_end )
  
  return(result)
}

remove_redundant_observations <- function(OpenDengue_data) {
  OpenDengue_data %>%
    ungroup() %>%
    dplyr::group_by(calendar_start_date, calendar_end_date) %>%
    dplyr::mutate(
      No_of_obs = n(),
      Obs_to_keep = case_when(
        No_of_obs > 1 & is.na(dengue_total) & !is.na(interpolated_cases) ~ "Remove",
        No_of_obs > 1 & !is.na(dengue_total) & !is.na(interpolated_cases) ~ "Keep",
        No_of_obs == 1 ~ "Keep"
      )
    ) %>%
    dplyr::filter(Obs_to_keep == "Keep") %>%
    dplyr::select(-No_of_obs, -Obs_to_keep)
}

interpolate_monthly_data <- function(OpenDengue_data, date_sequence) {
  
  OD_interpolated <- OpenDengue_data %>%
    dplyr::mutate(
      Month = month(calendar_start_date),
    ) %>%
    full_join(date_sequence, by = c("Month", "Year")) %>%
    arrange(Year, Month) %>%
    dplyr::mutate(
      interpolated_cases = ceiling(na_interpolation(dengue_total, option = "linear", maxgap = 1)),
      T_res = if_else(is.na(T_res), "Month", T_res)
    ) %>%
    dplyr::filter(!is.na(interpolated_cases)) %>%
    dplyr::rename(Period = Month) %>%
    fill_monthly_missing_entries()
  
  return(OD_interpolated)
}

interpolate_weekly_data <- function(OpenDengue_data, date_sequence) {
  
  OD_interpolated <- OpenDengue_data %>%
    dplyr::mutate(
      Week = week(calendar_start_date),
      week_length = as.numeric(calendar_end_date - calendar_start_date)
    ) %>%
    full_join(date_sequence, by = c("Year", "Week")) %>%
    arrange(Year, Week) %>%
    dplyr::mutate(
      interpolated_cases = ceiling(na_interpolation(dengue_total, option = "linear", maxgap = 4) * week_length / 6),
      T_res = if_else(is.na(T_res), "Week", T_res)
    ) %>%
    dplyr::filter(!is.na(interpolated_cases)) %>%
    dplyr::rename(Period = Week) %>%
    fill_weekly_missing_entries() %>% 
    remove_redundant_observations()
  
  return(OD_interpolated)
}

# Main function
interpolate_missing_national_OD_data <- function(OpenDengue_data) {
  
  OD_filtered <- OpenDengue_data %>% 
    dplyr::filter(T_res != "Year")
  
  # Group by spatial identifiers and apply interpolation within each group
  OD_filtered %>%
    group_by(ISO_A0) %>%
    
    group_modify(~ {
      tryCatch({
        temporal_resolutions <- unique(.x$T_res)
        
        if (length(temporal_resolutions) == 1) {
          if (temporal_resolutions == "Month") {
            # Monthly interpolation
            years <- unique(.x$Year)
            date_sequence <- generate_month_sequence(years)
            result <- interpolate_monthly_data(.x, date_sequence)
            
            
          } else if (temporal_resolutions == "Week") {
            # Weekly interpolation
            years <- unique(.x$Year)
            date_sequence <- generate_week_sequence(years)
            result <- interpolate_weekly_data(.x, date_sequence)
            
          }
        } else if (length(temporal_resolutions) == 2) {
          
          # Handle mixed monthly and weekly data
          monthly_data <- .x %>% dplyr::filter(T_res == "Month")
          weekly_data <- .x %>% dplyr::filter(T_res == "Week")
          
          years_monthly <- unique(monthly_data$Year)
          years_weekly <- unique(weekly_data$Year)
          
          monthly_result <- if (length(years_monthly) > 0) {
            interpolate_monthly_data(monthly_data, generate_month_sequence(years_monthly))
          } else {
            NULL
          }
          
          weekly_result <- if (length(years_weekly) > 0) {
            interpolate_weekly_data(weekly_data, generate_week_sequence(years_weekly))
          } else {
            NULL
          }
          
          result <- bind_rows(monthly_result, weekly_result)
          return(result)
          
        }
        
      }, error = function(e) {
        message("ERROR in main tryCatch: ", e$message)
        # Return failure info
        tibble(
          adm_0_name = unique(.x$adm_0_name),
          T_res = case_when(
            length(unique(.x$T_res)) == 2 ~ "Both",
            length(unique(.x$T_res)) == 1 & unique(.x$T_res) == "Week" ~ "Week",
            length(unique(.x$T_res)) == 1 & unique(.x$T_res) == "Month" ~ "Month",
            TRUE ~ "Unknown"
          ),
          interpolation_status = "Failed",
          interpolation_error = as.character(e$message)
        )
      })
    }) %>%
    ungroup()
}

#-----
assess_OD_national_interpolated_data_coverage <- function(OD_extract){
  
  tryCatch({
    OD_extract_coverage <- OD_extract %>%
      dplyr::filter(!is.na(interpolated_cases)) %>% 
      dplyr::select(
        # Spatial identifiers
        adm_0_name, 
        
        # Temporal identifiers
        calendar_start_date, calendar_end_date, Year, T_res,
        
        # Cases
        dengue_total, interpolated_cases
      ) %>%
      group_by(
        Year, T_res
      ) %>%
      dplyr::summarize(OD_annual_counts = n(), .groups = "drop") %>% 
      as.data.frame() %>% 
      dplyr::mutate(
        adm_0_name = na.omit(unique(OD_extract$adm_0_name))
      )
    
    return(OD_extract_coverage)
    
  }, error = function(e){
    message("ERROR in OpenDengue data coverage assessment: ", e$message)
    tibble(
      ISO_A0 = unique(.x$ISO_A0),
      adm_0_name = unique(.x$adm_0_name),
      Data_coverage_status = "Failed",
      error_message = as.character(e$message)
    )
  })
  
}

extract_desired_OD_data <- function(OpenDengue_national_extract, coverage_results){
  
  #--- Filter national extract to remove yearly reporting 
  OpenDengue_national_extract_clean <- OpenDengue_national_extract %>% 
    dplyr::filter(T_res != "Year")
  
  #--- Split national extract into countries overlapping and not overlapping with WHO 
  OD_WHO_overlap_countries <- coverage_results$iso3
  
  Overlap_countries_data <- OpenDengue_national_extract_clean %>%
    dplyr::filter(ISO_A0 %in% OD_WHO_overlap_countries)
  
  Non_overlap_countries_data <- OpenDengue_national_extract_clean %>%
    dplyr::filter(!ISO_A0 %in% OD_WHO_overlap_countries)
  
  #--- Filter overlap data for desired country years
  
  # Identify target years from OpenDengue
  Years_from_OD <- coverage_results %>%
    dplyr::filter(
      Which_to_keep_clean == "OpenDengue") %>%
    dplyr::select(
      Year, adm_0_name, iso3) %>%
    dplyr::mutate(
      OD_Country_Year = paste(adm_0_name, "_", Year)
    )
  
  # Filter for target years
  Overlap_countries_target_data <- Overlap_countries_data %>% 
    dplyr::mutate(
      Country_Year = paste(adm_0_name, "_", Year)) %>%
    dplyr::filter(
      Country_Year %in% Years_from_OD$OD_Country_Year) %>% 
    dplyr::select(
      !Country_Year)
  
  #-- Join filtered data back to original 
  OpenDengue_processed_data <- rbind(Non_overlap_countries_data, 
                                     Overlap_countries_target_data)
  
  return(OpenDengue_processed_data)
}

#----- Disaggregate weeks crossing months 

disaggregate_weekly_helper <- function(weekly_data) {
  # Add month indicators
  weekly_data <- weekly_data %>%
    dplyr::mutate(
      calendar_start_month = month(calendar_start_date),
      calendar_end_month = month(calendar_end_date),
      same_start_end_month = calendar_start_month == calendar_end_month
    )
  
  # Split into weeks that cross months and those that don't
  weeks_not_crossing <- weekly_data %>%
    dplyr::filter(same_start_end_month) %>%
    dplyr::mutate(interpolated_cases_clean = interpolated_cases) %>%
    dplyr::select(-calendar_start_month, -calendar_end_month, -same_start_end_month)
  
  weeks_crossing <- weekly_data %>%
    dplyr::filter(!same_start_end_month) %>%
    dplyr::mutate(
      month_start_date = floor_date(calendar_end_date, unit = "month"),
      month_end_date = ceiling_date(calendar_start_date, unit = "month") - 1
    )
  
  # Disaggregate crossing weeks
  if (nrow(weeks_crossing) > 0) {
    # First part: week start to month end
    weeks_part1 <- weeks_crossing %>%
      dplyr::mutate(
        week_start_month_end_diff = month_start_date - calendar_start_date,
        calendar_end_date = calendar_start_date + week_start_month_end_diff - 1,
        interpolated_cases_clean = interpolated_cases * as.numeric(week_start_month_end_diff) / 7
      ) %>%
      dplyr::select(-week_start_month_end_diff, -month_start_date, -month_end_date,
             -calendar_start_month, -calendar_end_month, -same_start_end_month)
    
    # Second part: month start to week end
    weeks_part2 <- weeks_crossing %>%
      dplyr::mutate(
        month_start_week_end_diff = calendar_end_date - month_end_date,
        calendar_start_date = calendar_end_date - month_start_week_end_diff + 1,
        interpolated_cases_clean = interpolated_cases * as.numeric(month_start_week_end_diff) / 7
      ) %>%
      dplyr::select(-month_start_week_end_diff, -month_start_date, -month_end_date,
             -calendar_start_month, -calendar_end_month, -same_start_end_month)
    
    # Combine all weekly data
    bind_rows(weeks_not_crossing, weeks_part1, weeks_part2)
  } else {
    weeks_not_crossing
  }
}

disaggregate_OD_cases_weeks_crossing_months <- function(OD_extract){
  
  OD_extract_disagg <- OD_extract %>% 
    ungroup() %>% 
    group_by(ISO_A0) %>%
    group_modify(~{ 
      tryCatch({
        
        # Define common columns to select
        common_cols <- c("adm_0_name", "calendar_start_date", "calendar_end_date", 
                         "Year", "T_res", "dengue_total", "interpolated_cases")
        
        # Get unique temporal resolutions
        unique_t_res <- unique(.x$T_res)
        n_t_res <- length(unique_t_res)
        
        # Process data based on temporal resolution
        if (n_t_res == 1 && unique_t_res == "Month") {
          # Only monthly data - no processing needed
          result <- .x %>%
            dplyr::select(all_of(common_cols)) %>%
            dplyr::mutate(interpolated_cases_clean = interpolated_cases)
          return(result)
          
        } else if (n_t_res == 1 && unique_t_res == "Week") {
          # Only weekly data - process all
          result <- .x %>%
            dplyr::select(all_of(common_cols)) %>%
            disaggregate_weekly_helper()
          return(result)
          
          
        } else if (n_t_res == 2) {
          # Both monthly and weekly data
          monthly_data <- .x %>%
            dplyr::filter(T_res == "Month") %>%
            dplyr::select(all_of(common_cols)) %>%
            dplyr::mutate(interpolated_cases_clean = interpolated_cases)
          
          weekly_data <- .x %>%
            dplyr::filter(T_res == "Week") %>%
            dplyr::select(all_of(common_cols)) %>%
            disaggregate_weekly_helper()
          
          result <- bind_rows(monthly_data, weekly_data)
          return(result)
        } else {
          tibble(
            adm_0_name = unique(.x$adm_0_name),
            T_res = paste(unique(.x$T_res), collapse = ","),
            disaggregation_status = "Unhandled T_res",
            disaggregation_error = NA_character_
          )
        }
        
      }, error = function(e) {
        message("ERROR in disaggregation of weeks crossing months: ", e$message)
        # Return failure info
        tibble(
          adm_0_name = unique(.x$adm_0_name),
          T_res = case_when(
            length(unique(.x$T_res)) == 2 ~ "Both",
            length(unique(.x$T_res)) == 1 & unique(.x$T_res) == "Week" ~ "Week",
            length(unique(.x$T_res)) == 1 & unique(.x$T_res) == "Month" ~ "Month",
            TRUE ~ "Unknown"
          ),
          disaggregation_status = "Failed",
          disaggregation_error = as.character(e$message)
        )
      })
    })
}

#----- Aggregate weeks to months
aggregate_weekly_to_monthly_helper <- function(weekly_data) {
  
  # Aggregate
  monthly_aggregate <- weekly_data %>%
    dplyr::mutate(
      Month = month(calendar_start_date),
    ) %>%
    group_by(adm_0_name, T_res, Month, Year) %>%
    dplyr::summarize(monthly_cases = round(sum(interpolated_cases_clean)), .groups = "drop")
  
  return(monthly_aggregate)
}

aggregate_weekly_to_monthly_OD_cases <- function(OD_extract){
  
  # Filter and prepare data
  OD_extract <- OD_extract %>% 
    dplyr::filter(T_res %in% c("Week", "Month")) %>%
    ungroup()
  
  OD_extract %>% 
    group_by(ISO_A0) %>% 
    group_modify(~{
      tryCatch({
        
        desired_cols <- c("adm_0_name", "Month", "Year", "monthly_cases")  
        
        # Get unique temporal resolutions
        unique_t_res <- unique(.x$T_res)
        n_t_res <- length(unique_t_res)
        
        # Process data based on temporal resolution
        if (n_t_res == 1 && unique_t_res == "Month") {
          # Only monthly data - no aggregation needed
          .x %>%
            dplyr::mutate(
              Month = month(calendar_start_date),
              monthly_cases = interpolated_cases_clean) %>%
            dplyr::select(desired_cols)
          
        } else if (n_t_res == 1 && unique_t_res == "Week") {
          # Only weekly data - aggregate to monthly
          aggregate_weekly_to_monthly_helper(.x) %>%
            dplyr::select(desired_cols)
          
        } else if (n_t_res == 2) {
          # Both monthly and weekly data
          monthly_data <- .x %>%
            dplyr::filter(T_res == "Month") %>%
            dplyr::mutate(
              Month = month(calendar_start_date),
              monthly_cases = interpolated_cases_clean) %>%
            dplyr::select(desired_cols)
          
          weekly_data <- .x %>%
            dplyr::filter(T_res == "Week") %>%
            aggregate_weekly_to_monthly_helper(.) %>%
            dplyr::select(desired_cols)
          
          bind_rows(monthly_data, weekly_data) 
          
        }
        
      }, error = function(e) {
        message("ERROR in OpenDengue monthly aggregation: ", e$message)
        # Return failure info
        tibble(
          adm_0_name = unique(.x$adm_0_name),
          T_res = case_when(
            length(unique(.x$T_res)) == 2 ~ "Both",
            length(unique(.x$T_res)) == 1 & unique(.x$T_res) == "Week" ~ "Week",
            length(unique(.x$T_res)) == 1 & unique(.x$T_res) == "Month" ~ "Month",
            TRUE ~ "Unknown"
          ),
          aggregation_status = "Failed",
          aggregation_error = as.character(e$message)
        )
      })
    })
}
