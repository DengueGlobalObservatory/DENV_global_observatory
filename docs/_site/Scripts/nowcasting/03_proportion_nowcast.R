
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(scales)
library(countrycode)
library(purrr)
library(ggnewscale)

if (!exists("log_message")) {
  source("Scripts/utils/logging.R")
  ensure_logger(console = TRUE)
}

if (!exists("record_countries_at_step")) {
  source("Scripts/utils/country_tracking.R")
}

log_message("Running 03_proportion_nowcast.")

#### ------ Universal Variables ---- ####

current_year   <- year(Sys.Date())
last_month_data <- month(Sys.Date()) - 1

# previous-year logic when month==1, uncomment:
if (last_month_data == 0) {
  last_month_data <- 12
  current_year <- current_year - 1
}
#### ------ Nowcasting ---- ####

# combine historic and current data 
# Use outer join to preserve all countries from both datasets
# Match on iso3 and Month (iso3 is more reliable than country names)
# This ensures countries with both seasonal baseline and current data are not dropped

# Log countries before merge
if (exists("log_message")) {
  seasonal_countries <- unique(full_data_average_season$iso3)
  current_countries <- unique(current_data$iso3)
  countries_in_both <- intersect(seasonal_countries, current_countries)
  countries_only_seasonal <- setdiff(seasonal_countries, current_countries)
  countries_only_current <- setdiff(current_countries, seasonal_countries)
  
  log_message("Countries in seasonal baseline: " %+% length(seasonal_countries))
  log_message("Countries in current data: " %+% length(current_countries))
  log_message("Countries in both: " %+% length(countries_in_both))
  log_message("Countries only in seasonal: " %+% length(countries_only_seasonal))
  log_message("Countries only in current: " %+% length(countries_only_current))
  
  # Check for the 14 specific countries that should be preserved
  target_countries <- c("ATG", "BES", "CUW", "CIV", "FSM", "REU", "KNA", "MAF", "VCT", "TTO", "TCA", "USA", "VGB", "WLF")
  target_in_seasonal <- target_countries[target_countries %in% seasonal_countries]
  target_in_current <- target_countries[target_countries %in% current_countries]
  target_in_both <- target_countries[target_countries %in% countries_in_both]
  
  log_message("Target countries (14) in seasonal: " %+% length(target_in_seasonal) %+% " (" %+% paste(target_in_seasonal, collapse = ", ") %+% ")")
  log_message("Target countries (14) in current: " %+% length(target_in_current) %+% " (" %+% paste(target_in_current, collapse = ", ") %+% ")")
  log_message("Target countries (14) in both: " %+% length(target_in_both) %+% " (" %+% paste(target_in_both, collapse = ", ") %+% ")")
}

# Merge using outer join - preserve all countries from both datasets
# First, we need to align current_data to seasonal months
# Add season_nMonth to current_data by matching with seasonal baseline

# Get season_nMonth mapping from seasonal baseline
season_month_map_old <- full_data_average_season %>%
  dplyr::select(iso3, Month, season_nMonth) %>%
  dplyr::distinct()

# Get season_nMonth mapping from seasonal baseline
# Ensure one-to-one mapping: one season_nMonth per Month per iso3
season_month_map <- full_data_average_season %>%
  dplyr::select(iso3, Month, season_nMonth) %>%
  dplyr::group_by(iso3, Month) %>%
  # If multiple season_nMonth values exist for same Month, pick the most common one
  dplyr::add_count(iso3, Month, season_nMonth) %>%
  dplyr::arrange(iso3, Month, desc(n)) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::select(-n) %>%
  dplyr::ungroup() %>%
  dplyr::distinct()

# Add season_nMonth to current_data
current_data_with_season <- current_data %>%
  dplyr::left_join(season_month_map, by = c("iso3", "Month"))

# Merge using iso3 and season_nMonth (not calendar Month)
data <- merge(full_data_average_season, current_data_with_season, 
              all.x = TRUE, all.y = TRUE,
              by = c("iso3", "season_nMonth")) %>%
  unique()

# Handle country name mismatches - prefer country name from seasonal data
if ("country.x" %in% names(data) && "country.y" %in% names(data)) {
  data <- data %>%
    dplyr::mutate(
      country = dplyr::coalesce(country.x, country.y)
    ) %>%
    dplyr::select(-country.x, -country.y)
} else if ("country.x" %in% names(data)) {
  data <- data %>%
    dplyr::rename(country = country.x)
} else if ("country.y" %in% names(data)) {
  data <- data %>%
    dplyr::rename(country = country.y)
}

# Handle Month - prefer from current_data if available, otherwise from seasonal
if ("Month.x" %in% names(data) && "Month.y" %in% names(data)) {
  data <- data %>%
    dplyr::mutate(
      Month = dplyr::coalesce(Month.y, Month.x)  # Prefer current_data Month
    ) %>%
    dplyr::select(-Month.x, -Month.y)
} else if ("Month.x" %in% names(data)) {
  data <- data %>%
    dplyr::rename(Month = Month.x)
} else if ("Month.y" %in% names(data)) {
  data <- data %>%
    dplyr::rename(Month = Month.y)
}

log_message("Merged historic and current data rows: " %+% nrow(data))
log_message("Unique countries after merge: " %+% length(unique(data$iso3)))

# define if data is observed or unobserved
data <- data %>%
  dplyr::mutate(
    Data_status = dplyr::case_when(is.na(cases) ~ "Unobserved",
                                   !is.na(cases) ~ "Observed")
  )

# Calculate cumulative cases to date by season (needed for predicted total)
# This must be done BEFORE estimating predicted totals
data <- data %>%
  dplyr::arrange(iso3, Year, season_nMonth) %>%
  dplyr::group_by(country, iso3, Year) %>%  # Group by year to handle season boundaries
  dplyr::mutate(
    # Calculate cumulative cases within the season (by season_nMonth)
    cum_todate_cases_season = cumsum(dplyr::coalesce(cases, 0))
  ) %>%
  dplyr::ungroup()

# Estimate predicted total seasonal cases using cumulative cases to date
# Use the most recent observed cumulative cases and cumulative proportion
data <- data %>%
  dplyr::group_by(iso3, Year) %>%
  dplyr::mutate(
    # Find the most recent month with observed cases
    last_obs_idx = if (any(!is.na(cases))) {
      max(which(!is.na(cases)))
    } else {
      NA_integer_
    },
    
    # Get cumulative cases and cumulative proportion at that point
    last_cum_cases = if_else(
      is.na(last_obs_idx),
      NA_real_,
      cum_todate_cases_season[last_obs_idx]
    ),
    last_cum_prop = if_else(
      is.na(last_obs_idx),
      NA_real_,
      Ave_cum_monthly_proportion[last_obs_idx]
    ),
    
    # Calculate predicted total: cumulative cases / cumulative proportion
    Predicted_total_seasonal_cases = if_else(
      !is.na(last_cum_cases) & !is.na(last_cum_prop) & last_cum_prop > 0,
      round(last_cum_cases / last_cum_prop, 0),
      NA_real_
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-last_obs_idx, -last_cum_cases, -last_cum_prop)

# ---- Estimate cases for missing months using average monthly proportion ----
data <- data %>%
  dplyr::group_by(iso3, Year) %>%
  dplyr::mutate(
    # Get the predicted total (should be same for all months in a season)
    group_predicted_total = dplyr::first(Predicted_total_seasonal_cases[!is.na(Predicted_total_seasonal_cases)])
  ) %>%
  dplyr::mutate(
    # Fill missing cases using average monthly proportion
    cases = dplyr::case_when(
      !is.na(cases) ~ cases,   # keep observed values
      
      # Only estimate for current year, months <= last_month_data, and if we have predicted total
      is.na(cases) &
        Year == current_year &
        Month <= last_month_data &
        !is.na(group_predicted_total) &
        !is.na(Ave_monthly_proportion) ~
        round(group_predicted_total * Ave_monthly_proportion, 0),
      
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-group_predicted_total)

# Recalculate cumulative cases after filling estimates
data <- data %>%
  dplyr::arrange(iso3, Year, season_nMonth) %>%
  dplyr::group_by(country, iso3, Year) %>%
  dplyr::mutate(
    cum_todate_cases_season = cumsum(dplyr::coalesce(cases, 0))
  ) %>%
  dplyr::ungroup()

# Calculate calendar cumulative cases
data <- data %>%
  dplyr::arrange(iso3, Year, Month) %>%
  dplyr::group_by(country, iso3, Year) %>%
  dplyr::mutate(
    cum_todate_cases_calendar = cumsum(dplyr::coalesce(cases, 0))
  ) %>%
  dplyr::ungroup()

# Recalculate predicted total after estimates (for consistency)
data <- data %>%
  dplyr::group_by(iso3, Year) %>%
  dplyr::mutate(
    last_obs_idx = if (any(!is.na(cases))) {
      max(which(!is.na(cases)))
    } else {
      NA_integer_
    },
    last_cum_cases = if_else(
      is.na(last_obs_idx),
      NA_real_,
      cum_todate_cases_season[last_obs_idx]
    ),
    last_cum_prop = if_else(
      is.na(last_obs_idx),
      NA_real_,
      Ave_cum_monthly_proportion[last_obs_idx]
    ),
    Predicted_total_seasonal_cases = if_else(
      !is.na(last_cum_cases) & !is.na(last_cum_prop) & last_cum_prop > 0,
      round(last_cum_cases / last_cum_prop, 0),
      NA_real_
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-last_obs_idx, -last_cum_cases, -last_cum_prop)

# further define Data_status
data <- data %>%
  dplyr::mutate(
    source = dplyr::case_when(
      Data_status == "Unobserved" & !is.na(cases) ~ "Estimates",
      !is.na(cases) ~ source,
      TRUE ~ source
    )
  )

estimated_rows <- data %>%
  filter(source == "Estimates") %>%
  nrow()
log_message("Nowcasting completed; estimated rows: " %+% estimated_rows)
log_message("Completed 03_proportion_nowcast.")

# Record countries after nowcast merge (Step 5: Nowcast Merge)
if (exists("record_countries_at_step")) {
  tryCatch({
    record_countries_at_step(data, "Step_5_Nowcast_Merge")
  }, error = function(e) {
    if (exists("log_message")) {
      log_message("Warning: Country tracking failed at Step 5: " %+% conditionMessage(e), level = "WARNING")
    }
  })
}