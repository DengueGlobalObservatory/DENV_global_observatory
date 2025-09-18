#' ---
#' title: "01_historic_national_data"
#' author: "K M Susong and K Joshi"
#' 
#' ---

#'
#' Overview:
#' ========
#' 
#' open the WHO and OPENdengue data
#' create fuller dataset 
#' 

library(opendenguedata)
library(imputeTS)
library(readr)
library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)

#--------------- Loading data
# opens in current version from the github
OD_national_extract <- read_data(extract = "national", as_data_frame = TRUE, showProgress = FALSE)
#!!!# this is currently hard coded, this will need to be updated to an API pull of the most recent data
WHO_data <- read_excel("Data/WHO/dengue-global-data-2025-06-24.xlsx")

# Clean WHO data
WHO_data <- WHO_data %>%
  select(date, country, iso3, cases) %>% 
  mutate(
    date = as.Date(date),
    cases = as.numeric(cases)) %>%
  mutate(
    Year = year(date)) %>%
  mutate(
    cases = case_when(cases < 0 ~ NA,
                           TRUE ~ cases))

# Clean and filter OPENdengue national data

opendengue_data <- OD_national_extract %>%
  mutate(
    date = calendar_start_date,
    iso3 = ISO_A0,
    cases = dengue_total,
    country = str_to_title(full_name) # fix ALL CAP
  ) %>%
    select(
      date, 
      country, 
      iso3, 
      cases) %>% 
    mutate(
      Year = year(date)
      ) %>%
  filter(Year > 2009)
    

#-------------- Threshold of case aviablity  

#HERE

#--------------- identify countries that are only in the WHO data

WHO_only <- WHO_data  %>%
  filter(!iso3 %in% opendengue_data$iso3)

WHO_only %>%
  select(country, iso3) %>%
  unique()

#--------------- add the WHO only data and open dengue for a complete list 

full_data <- rbind(WHO_only, opendengue_data)

# data coverage test:
## there are a lot of data gaps... i am not sure about interpolating or not...
full_data %>%
  mutate(
  data_avai = case_when(
    is.na(cases) ~ FALSE,
    TRUE ~ TRUE )
  ) %>%
  ggplot( aes( x = date, y = iso3, fill = data_avai)) +
  geom_tile()


full_data <- full_data %>% 
  ungroup() %>% 
  mutate(Month = month(date)) %>% 
  mutate(Month.Year = paste0(Month, "-", Year)) 

# CHECK HERE
full_data_interpolated <- full_data %>%
  mutate(interpolated_cases = ceiling(na_interpolation(cases, option = "linear", maxgap = 1))) %>%
  filter(!is.na(interpolated_cases))

# data coverage test:
## this doesnt seem to make a differnce... still confused 
full_data_interpolated %>%
  mutate(
    data_avai = case_when(
      is.na(interpolated_cases) ~ FALSE,
      TRUE ~ TRUE )
  ) %>%
  ggplot( aes( x = date, y = country, fill = data_avai)) +
  geom_tile()


#--------------- save full data 

write_csv(full_data, 
          file = paste0("V1/Output/historic_data/National_clean_data_",Sys.Date(),".csv"))
