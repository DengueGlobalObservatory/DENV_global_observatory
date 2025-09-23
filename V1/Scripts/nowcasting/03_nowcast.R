
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(scales)
library(countrycode)
library(purrr)
library(ggnewscale)

#--------------- Fill missing data to date + identify percentile of most recent month cum cases

monthly_data_complete <-  full_data_average_season %>% 
  ungroup() %>% 
  
  # Join average season to year to fill missing data to date. 
  left_join(
    .,
    current_data, 
    join_by("Month", "iso3")
  ) # %>% 
  distinct() %>%
  dplyr::select(
    -c(country.x, country.y)
  ) %>%
  dplyr::mutate(
    country = countrycode(iso3, "iso3c", "country.name"),
  ) %>%
  arrange(country, 
          iso3, 
          Month) %>%
  
  # Define previous month as last period requiring data filling.  
  dplyr::mutate(End_prediction_month =  month(Sys.Date()) - 1) %>%
  
  # Assign var based on whether cases are observed 
  group_by(country, iso3) %>% 
  dplyr::mutate(
    Observed_cum_cases = cumsum(cases),
    Data_status = case_when(is.na(cases) ~ "Unobserved",
                            !is.na(cases) ~ "Observed")
  ) %>%
  
  # Identify most recent month with observations by country
  group_by(country, 
           iso3, 
           Data_status) %>%
  dplyr::mutate(most_recent_month = case_when(
    Month == max(Month) & !is.na(cases) ~ "Most_recent",
    Month != max(Month) & !is.na(cases) ~ "Not_most_recent",
    is.na(cases) ~ "No_data")
  ) # %>%
  
  # Calculate predicted total seasonal cases using cases observed to date 
  dplyr::mutate(
    Predicted_total_seasonal_cases = 
      case_when(most_recent_month == "Most_recent" ~ Observed_cum_cases / Ave_cum_monthly_proportion,
                most_recent_month == "Not_most_recent" ~ NA,
                most_recent_month == "No_data" ~ NA)
  ) %>%
  ungroup() %>% 
  group_by(country, iso3) %>% 
  
  # Is there any data available for that season?
  dplyr::mutate(
    Any_data_available = 
      case_when(
        length(unique(na.omit(Predicted_total_seasonal_cases))) == 0 ~ "No_data_available",
        length(unique(na.omit(Predicted_total_seasonal_cases))) > 0 ~ "Data_available"), 
  ) %>%
  dplyr::mutate(
    
    # Fill predicted total seasonal case col, where no data available enter NA.
    Predicted_total_seasonal_cases = 
      case_when(Any_data_available == "No_data_available" ~ Predicted_total_seasonal_cases,
                Any_data_available == "Data_available" ~ first(na.omit(Predicted_total_seasonal_cases))
      ),
    
    # Where data is missing fill using predictions 
    complete_cases = case_when(!is.na(cases) ~ cases,
                               is.na(cases) & Month <= End_prediction_month ~ round(Predicted_total_seasonal_cases * Ave_monthly_proportion),
                               Month > End_prediction_month ~ NA),
    complete_cum_cases = cumsum(complete_cases),
    
    # Identify percentile of most recent month cum cases within negbin dist
    percentile_most_recent = pnbinom(q = complete_cum_cases, 
                                     size = nb_size, 
                                     mu = nb_mean) * 100
  ) %>% 
  
  # Remove entries from predicted data col to only be two months ahead 
  dplyr::select(
    # Identifiers
    country, iso3, Month, season_nMonth, 
    
    # Observed and complete cases + whether cases are observed or predicted, 
    cases, complete_cases, Data_status, 
    
    # Cumulative cases to date 
    Observed_cum_cases, complete_cum_cases,
    
    # Average season 
    Ave_season_monthly_cases, Ave_season_monthly_cum_cases, 
    
    # Negative binomial distribution parameters 
    nb_mean, nb_size,
    
    # Percentile position of the observed cases to date within negbin dist 
    percentile_most_recent
  ) %>%
  ungroup()


### SAVE OUTPUT