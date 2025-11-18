
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(scales)
library(countrycode)
library(purrr)
library(ggnewscale)


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
data <- merge (full_data_average_season, current_data, 
               # all.x = T, all.y = T, 
               by = c( "country", "iso3", "Month")) %>%
  unique()

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

