
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
  source("V1/Scripts/utils/logging.R")
  ensure_logger(console = TRUE)
}

if (!exists("record_countries_at_step")) {
  source("V1/Scripts/utils/country_tracking.R")
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
# Match on iso3 and Month (iso3 is more reliable than country names)
data <- merge(full_data_average_season, current_data, 
              all.x = TRUE, all.y = TRUE,
              by = c("iso3", "Month")) %>%
  unique()

# Handle country name mismatches - prefer country name from seasonal data
# merge() will create country.x and country.y if both datasets have "country" column
if ("country.x" %in% names(data) && "country.y" %in% names(data)) {
  data <- data %>%
    dplyr::mutate(
      country = dplyr::coalesce(country.x, country.y)
    ) %>%
    dplyr::select(-country.x, -country.y)
} else if ("country.x" %in% names(data)) {
  # Only seasonal data had country column
  data <- data %>%
    dplyr::rename(country = country.x)
} else if ("country.y" %in% names(data)) {
  # Only current data had country column
  data <- data %>%
    dplyr::rename(country = country.y)
}

log_message("Merged historic and current data rows: " %+% nrow(data))
log_message("Unique countries after merge: " %+% length(unique(data$iso3)))

# Verify target countries are preserved after merge
if (exists("log_message")) {
  countries_after_merge <- unique(data$iso3)
  target_countries <- c("ATG", "BES", "CUW", "CIV", "FSM", "REU", "KNA", "MAF", "VCT", "TTO", "TCA", "USA", "VGB", "WLF")
  target_preserved <- target_countries[target_countries %in% countries_after_merge]
  target_missing <- target_countries[!target_countries %in% countries_after_merge]
  
  log_message("Target countries preserved after merge: " %+% length(target_preserved) %+% "/14")
  if (length(target_preserved) > 0) {
    log_message("  Preserved: " %+% paste(target_preserved, collapse = ", "))
  }
  if (length(target_missing) > 0) {
    log_message("  Missing: " %+% paste(target_missing, collapse = ", "), level = "WARNING")
  }
}

# define if data is observed or unobserved
data <- data %>%
  mutate(
  Data_status = case_when(is.na(cases) ~ "Unobserved",
                          !is.na(cases) ~ "Observed")
)


# estimate the final cummulative cases given the months case count 

data <- data %>%
  mutate(
    Predicted_total_seasonal_cases = cases /Ave_cum_monthly_proportion
  )

# ---- Estimate cases for unknowns ----
data <- data %>%
  group_by(iso3, Year) %>%            # <- important: restrict to same year/season
  mutate(
    # index of the last non-NA predicted total within this iso3-year
    last_pred_idx = if (any(!is.na(Predicted_total_seasonal_cases))) {
      max(which(!is.na(Predicted_total_seasonal_cases)))
    } else {
      NA_integer_
    },
    
    # scalar predicted total for this iso3-year (NA if none available)
    group_predicted_total = if_else(
      is.na(last_pred_idx),
      NA_real_,
      Predicted_total_seasonal_cases[last_pred_idx]
    ),
    
    # fill cases only for missing months in the current year up to last_month_data
    cases = case_when(
      !is.na(cases) ~ cases,   # keep observed values
      
      # only predict for current year and months <= last_month_data, and if we have a predicted total
      is.na(cases) &
        Year == current_year &
        Month <= last_month_data &
        !is.na(group_predicted_total) ~
        round(group_predicted_total * Ave_monthly_proportion, 0),
      
      TRUE ~ NA_real_         # otherwise keep NA (or keep original cases if you prefer)
    )
  ) %>%
  ungroup() %>%
  dplyr::select(-last_pred_idx, -group_predicted_total)   # drop helper cols

# calculated the cummulative cases to date - calendar

data <- data %>%
  plyr::arrange(Month, Year) %>%
  group_by(country, iso3, Year) %>% 
  dplyr::mutate(
    cum_todate_cases_calendar = cumsum(cases)
  ) %>%
  ungroup()

# calculated the cummulative cases to date - season 

data <- data %>%
  plyr::arrange(season_nMonth, Year) %>%
  group_by(country, iso3) %>% 
  dplyr::mutate(
    cum_todate_cases_season = cumsum(cases)
  ) %>%
  ungroup()

# calcaute predicted annual cases via estimates
data <- data %>%
  mutate(
    Predicted_total_seasonal_cases = round(cases /Ave_cum_monthly_proportion, 0)
  )


# further define Data_status
data <- data %>%
  mutate(
    source = case_when(
      Data_status == "Unobserved" & !is.na(cases) ~ "Estimates",
      !is.na(cases) ~ source,)
  )


# Identify percentile of most recent month cum cases within negbin dist

data <- data %>%
  mutate(
percentile_most_recent = pnbinom(
  q = replace_na(Predicted_total_seasonal_cases, 0), 
  size = replace_na(nb_size, 1),
  mu   = replace_na(nb_mean, 1)
) * 100
)
# format fixes
data <- data %>%
  mutate(
    Country = country
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

