#' ---
#' title: "02_identify_seasonal_baseline"
#' author: "K Joshi"
#' 
#' ---
#'
#' Overview:
#' ========
#' Identify seasonal baseline. 
#' Save as a CSV.
#' Load new data as made available and when a year has complete data for a year add it to the dataset and re-identify the seasonal baseline.
#' Naming order: full_data -> full_data_season_aligned -> full_data_filtered -> full_data_season_monthly_proportions -> full_data_average_season
#' 
#' Timeline:
#' ========
#' 08-09-2025: Reformatted DEV script. 
#' 09-09-2025: Added code to parameterise negative binomial distribution from monthly data.
#'             Removed SD + CI95 calculation. 

library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(circular)
library(MASS)
library(purrr)

if (!exists("log_message")) {
  source("Scripts/utils/logging.R")
  ensure_logger(console = TRUE)
}

if (!exists("record_countries_at_step")) {
  source("Scripts/utils/country_tracking.R")
}

log_message("Running 02_identify_seasonal_baseline.")

#--------------- Align from calendar year to dengue season 

#----- Identify low month to use as reference point 
dengue_season_low_month <- full_data %>% 
    ungroup() %>% 
    group_by(country, 
             Year) %>% 
    slice_min(order_by = cases, 
              n = 1) %>% 
    dplyr::rename(Low_month = Month)
  
#----- Identify average low month 
circular_mean <- function(month_number){
    
    # Convert months to angles 
    angles <- (month_number - 1) * 30
    
    # Create a circular object
    circ_months <- circular(angles, units = "degrees", modulo = "2pi")
    
    # Calculate circular mean and standard deviation
    circ_mean <- mean(circ_months)
    
    # Convert the mean angle back to month number
    # as.numeric(circ_mean) gives the mean angle in degrees
    mean_month <- ((as.numeric(circ_mean) %% 360) / 30) + 1
    mean_month <- ifelse(mean_month > 12, mean_month - 12, mean_month)
    mean_month_rounded <- round(mean_month)
    
    mean_month_rounded <- ifelse(mean_month_rounded == 0, 12, mean_month_rounded)
    
    return(mean_month_rounded)
    
  }
dengue_season_ave_low_month <- dengue_season_low_month %>% 
  ungroup() %>% 
  group_by(country, 
           iso3,
           Year) %>%
  
  # Remove years with all zeroes from low month identification - circular mean undefined.
  filter(sum(cases) > 0) %>%
  dplyr::mutate(No_of_obs = n()) %>% 
  
  # Where multiple months have equally few cases calculate within-year circular mean 
  mutate(within_year_mean_low_month = 
           case_when(No_of_obs > 1 ~ circular_mean(Low_month),
                     No_of_obs == 1  ~ Low_month)) %>%
  dplyr::select(country, 
         Year, 
         within_year_mean_low_month) %>%
  distinct() %>%
  ungroup() %>% 
  group_by(country, iso3) %>%
  dplyr::summarize(mean_low_month = circular_mean(within_year_mean_low_month))
  
#----- Add low month back to original dengue counts df 
full_data <- full_data %>% 
  full_join(., 
            dengue_season_ave_low_month, 
            by = c("country", "iso3")) %>% 
  distinct()
  
#----- Align data from calendar year to dengue season 
  
# Function 
circular_encode <- function(x, shift, cycle_length) {
  (((x) - shift) %% cycle_length) + 1
}
  
full_data_season_aligned <- full_data %>% 
  mutate(season_nMonth= circular_encode(Month, 
                                        mean_low_month, 
                                        12)) %>%
  mutate(season = case_when(
    Month >= mean_low_month ~ paste0(Year, "/", (Year+1)),
    Month < mean_low_month ~ paste0((Year-1), "/", Year))
    )

# Record countries before seasonal filtering (Step 3c: Seasonal Before Filter)
if (exists("record_countries_at_step")) {
  tryCatch({
    record_countries_at_step(full_data_season_aligned, "Step_3c_Seasonal_Before_Filter")
  }, error = function(e) {
    if (exists("log_message")) {
      log_message("Warning: Country tracking failed at Step 3c Before Filter: " %+% conditionMessage(e), level = "WARNING")
    }
  })
}
  
#--------------- Filtering by season instead of calendar year
#' Filtering criteria:
#'  Coverage: remove locations with < 12 data points per season. 
#'  Seasons with < 3 cases on average per month 
#'  Locations with less than three seasons of data. 

monthly_ave_case_threshold <- 3

# Step 1: Filter for complete seasons (12 months per season)
full_data_complete_seasons <- full_data_season_aligned %>% 
  ungroup() %>%
  group_by(country, season) %>%
  dplyr::mutate(Number_of_months_in_season = n()) %>% 
  dplyr::filter(Number_of_months_in_season == 12) %>% 
  dplyr::select(!Number_of_months_in_season) %>%
  ungroup()

# Diagnostic logging: Track countries at each filtering stage
if (exists("log_message")) {
  countries_before_complete <- full_data_season_aligned %>%
    dplyr::select(country, iso3) %>%
    dplyr::distinct()
  countries_after_complete <- full_data_complete_seasons %>%
    dplyr::select(country, iso3) %>%
    dplyr::distinct()
  dropped_at_complete <- countries_before_complete %>%
    dplyr::filter(!iso3 %in% countries_after_complete$iso3)
  log_message("Step 3c Filter 1 (complete seasons): " %+% nrow(countries_before_complete) %+% " countries before, " %+% 
              nrow(countries_after_complete) %+% " countries after, " %+% nrow(dropped_at_complete) %+% " dropped")
  if (nrow(dropped_at_complete) > 0 && any(dropped_at_complete$iso3 == "AFG")) {
    log_message("AFG dropped at complete seasons filter", level = "WARNING")
  }
}

# Step 2: Filter for seasons with >=5 cases per month on average
full_data_min_cases <- full_data_complete_seasons %>%
  group_by(country, season) %>%
  dplyr::mutate(Average_cases_per_month = ave(cases)) %>%
  dplyr::filter(Average_cases_per_month >= monthly_ave_case_threshold) %>%
  dplyr::select(!Average_cases_per_month) %>% 
  ungroup()

# Diagnostic logging: Track countries at min cases filter
if (exists("log_message")) {
  countries_after_min_cases <- full_data_min_cases %>%
    dplyr::select(country, iso3) %>%
    dplyr::distinct()
  dropped_at_min_cases <- countries_after_complete %>%
    dplyr::filter(!iso3 %in% countries_after_min_cases$iso3)
  log_message("Step 3c Filter 2 (min cases): " %+% nrow(countries_after_complete) %+% " countries before, " %+% 
              nrow(countries_after_min_cases) %+% " countries after, " %+% nrow(dropped_at_min_cases) %+% " dropped")
  if (nrow(dropped_at_min_cases) > 0 && any(dropped_at_min_cases$iso3 == "AFG")) {
    log_message("AFG dropped at min cases filter", level = "WARNING")
    # Get AFG's average cases per month for each season
    afg_seasons <- full_data_complete_seasons %>%
      dplyr::filter(iso3 == "AFG") %>%
      group_by(season) %>%
      dplyr::summarise(avg_cases = mean(cases, na.rm = TRUE), .groups = "drop")
    log_message("AFG season averages: " %+% paste(afg_seasons$season, "=", round(afg_seasons$avg_cases, 2), collapse = ", "))
  }
}

# Step 3: Filter for countries with at least three seasons
# Count distinct seasons (each season should have 12 months after previous filters)
full_data_filtered <- full_data_min_cases %>%
  group_by(country, iso3) %>% 
  dplyr::mutate(
    # Count distinct seasons - more reliable than n() / 12
    Number_of_seasons = length(unique(season))
  ) %>% 
  dplyr::filter(Number_of_seasons >= 3) %>% 
  ungroup() %>%
  dplyr::select(!Number_of_seasons)

# Diagnostic logging: Track countries at min seasons filter
if (exists("log_message")) {
  countries_after_final <- full_data_filtered %>%
    dplyr::select(country, iso3) %>%
    dplyr::distinct()
  dropped_at_min_seasons <- countries_after_min_cases %>%
    dplyr::filter(!iso3 %in% countries_after_final$iso3)
  log_message("Step 3c Filter 3 (min seasons): " %+% nrow(countries_after_min_cases) %+% " countries before, " %+% 
              nrow(countries_after_final) %+% " countries after, " %+% nrow(dropped_at_min_seasons) %+% " dropped")
  if (nrow(dropped_at_min_seasons) > 0 && any(dropped_at_min_seasons$iso3 == "AFG")) {
    log_message("AFG dropped at min seasons filter", level = "WARNING")
    # Get AFG's number of seasons
    afg_season_count <- full_data_min_cases %>%
      dplyr::filter(iso3 == "AFG") %>%
      dplyr::select(country, iso3, season) %>%
      dplyr::distinct() %>%
      dplyr::count(country, iso3)
    log_message("AFG has " %+% nrow(afg_season_count) %+% " complete seasons after min cases filter")
  }
}

# Determine specific drop reasons for countries that were in before_filter but not after
if (exists("record_countries_at_step")) {
  tryCatch({
    # Get countries before and after each filtering step
    countries_before <- full_data_season_aligned %>%
      dplyr::select(country, iso3) %>%
      dplyr::distinct()
    
    countries_after_complete <- full_data_complete_seasons %>%
      dplyr::select(country, iso3) %>%
      dplyr::distinct()
    
    countries_after_min_cases <- full_data_min_cases %>%
      dplyr::select(country, iso3) %>%
      dplyr::distinct()
    
    countries_after_final <- full_data_filtered %>%
      dplyr::select(country, iso3) %>%
      dplyr::distinct()
    
    # Create drop reason mapping - check in order of filtering steps
    # Only include countries that were dropped (not in final)
    dropped_countries <- countries_before %>%
      dplyr::filter(!iso3 %in% countries_after_final$iso3)
    
    if (nrow(dropped_countries) > 0) {
      drop_reasons_df <- dropped_countries %>%
        dplyr::mutate(
          drop_reason = dplyr::case_when(
            # Dropped at complete seasons step (first filter)
            !iso3 %in% countries_after_complete$iso3 ~ "Filtered: incomplete seasons (<12 months per season)",
            # Dropped at minimum cases step (second filter) - but passed complete seasons
            !iso3 %in% countries_after_min_cases$iso3 ~ "Filtered: <5 cases/month average",
            # Dropped at minimum seasons step (third filter) - but passed previous two
            !iso3 %in% countries_after_final$iso3 ~ "Filtered: <3 complete seasons",
            # Should not happen, but just in case
            TRUE ~ "Filtered: unknown reason"
          )
        ) %>%
        dplyr::select(iso3, drop_reason)  # Ensure only iso3 and drop_reason columns
      
      # Store for use in tracking
      assign("seasonal_drop_reasons", drop_reasons_df, envir = .GlobalEnv)
      
      if (exists("log_message")) {
        log_message("Created drop reasons for " %+% nrow(drop_reasons_df) %+% " countries dropped at seasonal filtering")
      }
    } else {
      # No countries dropped, create empty data frame with correct structure
      assign("seasonal_drop_reasons", 
             data.frame(iso3 = character(), drop_reason = character(), stringsAsFactors = FALSE),
             envir = .GlobalEnv)
      
      if (exists("log_message")) {
        log_message("No countries dropped at seasonal filtering")
      }
    }
  }, error = function(e) {
    if (exists("log_message")) {
      log_message("Warning: Could not determine specific drop reasons: " %+% conditionMessage(e), level = "WARNING")
    }
    # Create empty data frame on error
    assign("seasonal_drop_reasons", 
           data.frame(iso3 = character(), drop_reason = character(), stringsAsFactors = FALSE),
           envir = .GlobalEnv)
  })
}
  
#--------------- Identify average seasonal profile 

#----- Calculate monthly proportions by season
full_data_season_monthly_proportions <- full_data_filtered %>% 
    group_by(country, iso3, season) %>% 
    arrange(season_nMonth) %>%
    dplyr::mutate(
      Actual_cum_cases = cumsum(cases),
      Actual_monthly_proportion = cases / sum(cases),
      Actual_cum_monthly_proportion = cumsum(Actual_monthly_proportion)
      ) %>% 
    ungroup()
  
#----- Identify ave seasonal profile (log CIs)

# Fit negative binomial to cumulative cases at each season_nMonth
# This allows real-time comparison as season progresses
full_data_average_season <- full_data_season_monthly_proportions %>% 
  group_by(country, iso3, season_nMonth, Month) %>%  # ← Add Month to grouping
  dplyr::summarise(  # ← Change from mutate() to summarise()
    # Identify average season in case space 
    Ave_season_monthly_cum_cases = mean(Actual_cum_cases, na.rm = TRUE),
    Ave_season_monthly_cases = mean(cases, na.rm = TRUE),
    
    # Fit negative binomial to CUMULATIVE cases at this season_nMonth
    # This allows real-time severity assessment during ongoing seasons
    nb_fit_cum = list(
      tryCatch({
        # Get cumulative cases at this season_nMonth across all historical seasons
        cum_cases_clean <- Actual_cum_cases[!is.na(Actual_cum_cases) & Actual_cum_cases >= 0]
        if (length(cum_cases_clean) < 3 || sum(cum_cases_clean) == 0) {
          NULL
        } else {
          glm.nb(cum_cases_clean ~ 1)
        }
      }, error = function(e){NULL}, warning = function(w){NULL})
    ),
    
    # Extract NB parameters for cumulative cases
    nb_size_cum = purrr::map_dbl(nb_fit_cum, ~ {
      if (is.null(.x) || !inherits(.x, "negbin")) {
        NA_real_
      } else {
        tryCatch(.x$theta, error = function(e) NA_real_)
      }
    }),
    nb_mean_cum = purrr::map_dbl(nb_fit_cum, ~ {
      if (is.null(.x) || !inherits(.x, "negbin")) {
        NA_real_
      } else {
        tryCatch({
          coef_val <- coef(.x)
          if (length(coef_val) > 0 && !is.na(coef_val[1])) {
            exp(coef_val[1])
          } else {
            NA_real_
          }
        }, error = function(e) NA_real_)
      }
    }),
    
    # Keep monthly NB parameters for other uses (if needed)
    nb_fit_monthly = list(
      tryCatch({
        cases_clean <- cases[!is.na(cases) & cases >= 0]
        if (length(cases_clean) < 3 || sum(cases_clean) == 0) {
          NULL
        } else {
          glm.nb(cases_clean ~ 1)
        }
      }, error = function(e){NULL}, warning = function(w){NULL})
    ),
    nb_size = purrr::map_dbl(nb_fit_monthly, ~ {
      if (is.null(.x) || !inherits(.x, "negbin")) {
        NA_real_
      } else {
        tryCatch(.x$theta, error = function(e) NA_real_)
      }
    }),
    nb_mean = purrr::map_dbl(nb_fit_monthly, ~ {
      if (is.null(.x) || !inherits(.x, "negbin")) {
        NA_real_
      } else {
        tryCatch({
          coef_val <- coef(.x)
          if (length(coef_val) > 0 && !is.na(coef_val[1])) {
            exp(coef_val[1])
          } else {
            NA_real_
          }
        }, error = function(e) NA_real_)
      }
    }), 
    
    # Identify average season in proportion space - for nowcasting/ prediction 
    Ave_monthly_proportion = mean(Actual_monthly_proportion, na.rm = TRUE),
    Ave_cum_monthly_proportion = mean(Actual_cum_monthly_proportion, na.rm = TRUE),
    
    .groups = "drop"  # ← Drop grouping after summarise
  ) %>%
  arrange(country, iso3, season_nMonth, Month) %>%
  # Remove the distinct() call - no longer needed since summarise() already gives one row per group
  dplyr::select(
    # Identifiers 
    country, iso3, Month, season_nMonth, 
    
    # Negative binomial parameters for monthly cases
    nb_size, 
    nb_mean,
    
    # Negative binomial parameters for cumulative cases (for real-time severity)
    nb_size_cum,
    nb_mean_cum,
    
    # Case space ave season 
    Ave_season_monthly_cases, Ave_season_monthly_cum_cases,
    
    # Prop space ave season
    Ave_cum_monthly_proportion, 
    Ave_monthly_proportion
  )

# Record countries after seasonal filtering (Step 3c: Seasonal After Filter)
if (exists("record_countries_at_step")) {
  tryCatch({
    # Use country-specific drop reasons if available
    if (exists("seasonal_drop_reasons") && is.data.frame(seasonal_drop_reasons) && nrow(seasonal_drop_reasons) > 0) {
      if (exists("log_message")) {
        log_message("Using country-specific drop reasons for " %+% nrow(seasonal_drop_reasons) %+% " countries")
      }
      record_countries_at_step(full_data_average_season, "Step_3c_Seasonal_After_Filter",
                               drop_reason = seasonal_drop_reasons)
      # Clean up
      rm(seasonal_drop_reasons, envir = .GlobalEnv)
    } else {
      if (exists("log_message")) {
        log_message("Warning: No seasonal_drop_reasons found, using generic drop reason", level = "WARNING")
      }
      # Fallback to generic reason
      record_countries_at_step(full_data_average_season, "Step_3c_Seasonal_After_Filter",
                               drop_reason = "Filtered: incomplete seasons, <5 cases/month average, or <3 seasons")
    }
  }, error = function(e) {
    if (exists("log_message")) {
      log_message("Warning: Country tracking failed at Step 3c After Filter: " %+% conditionMessage(e), level = "WARNING")
    }
  })
}

# After full_data_average_season is created, add diagnostic logging:

log_message("Seasonal baseline identified for " %+% length(unique(full_data_average_season$iso3)) %+% " locations.")

# Log NB parameter availability for diagnostic purposes
if (exists("log_message")) {
  # Check monthly NB parameters
  locations_with_monthly_nb <- full_data_average_season %>%
    dplyr::filter(!is.na(nb_size) & !is.na(nb_mean)) %>%
    dplyr::select(country, iso3) %>%
    dplyr::distinct()
  
  # Check cumulative NB parameters (for real-time severity)
  locations_with_cum_nb <- full_data_average_season %>%
    dplyr::filter(!is.na(nb_size_cum) & !is.na(nb_mean_cum)) %>%
    dplyr::select(country, iso3) %>%
    dplyr::distinct()
  
  log_message("Locations with monthly NB parameters: " %+% nrow(locations_with_monthly_nb) %+% 
                " (" %+% paste(locations_with_monthly_nb$iso3, collapse = ", ") %+% ")")
  log_message("Locations with cumulative NB parameters (for real-time severity): " %+% 
                nrow(locations_with_cum_nb) %+% 
                " (" %+% paste(locations_with_cum_nb$iso3, collapse = ", ") %+% ")")
  
  # Check for locations missing cumulative NB parameters
  all_locations <- full_data_average_season %>%
    dplyr::select(country, iso3) %>%
    dplyr::distinct()
  
  missing_cum_nb <- all_locations %>%
    dplyr::filter(!iso3 %in% locations_with_cum_nb$iso3)
  
  if (nrow(missing_cum_nb) > 0) {
    log_message("Warning: " %+% nrow(missing_cum_nb) %+% 
                  " locations missing cumulative NB parameters (real-time severity unavailable): " %+% 
                  paste(missing_cum_nb$iso3, collapse = ", "), level = "WARNING")
  }
  
  # Summary statistics
  cum_nb_summary <- full_data_average_season %>%
    dplyr::filter(!is.na(nb_mean_cum)) %>%
    dplyr::summarise(
      min_mean = min(nb_mean_cum, na.rm = TRUE),
      max_mean = max(nb_mean_cum, na.rm = TRUE),
      median_mean = median(nb_mean_cum, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (nrow(cum_nb_summary) > 0) {
    log_message("Cumulative NB mean range: " %+% round(cum_nb_summary$min_mean, 1) %+% 
                  " to " %+% round(cum_nb_summary$max_mean, 1) %+% 
                  " (median: " %+% round(cum_nb_summary$median_mean, 1) %+% ")")
  }
}

log_message("Completed 02_identify_seasonal_baseline.")
