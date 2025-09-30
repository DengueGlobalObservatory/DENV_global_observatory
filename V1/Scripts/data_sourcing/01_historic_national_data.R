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
library(countrycode)

#--------------- Loading data
Combining_WHO_and_OpenDengue_data <- new.env()
source("V1/Scripts/data_sourcing/02_Combining_WHO_and_OpenDengue_data.R",
       local = Combining_WHO_and_OpenDengue_data,
       echo = FALSE)

full_data <- Combining_WHO_and_OpenDengue_data$WHO_OD_combined_final
    
#-------------- Threshold of case availability  

#HERE

#--------------- add the WHO only data and open dengue for a complete list 

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
  mutate(Month = month(date),
         Year = year(date)) %>% 
  mutate(Month.Year = paste0(Month, "-", Year)) 

# CHECK HERE
full_data_interpolated <- full_data %>%
  mutate(interpolated_cases = cases)

# data coverage test:
## this doesnt seem to make a differnce... still confused 
full_data_interpolated %>%
  ggplot( aes( x = date, y = country, fill = data_avai)) +
  geom_tile()


#--------------- save full data 

write_csv(full_data, 
          file = paste0("V1/Output/historic_data/National_clean_data_",Sys.Date(),".csv"))
