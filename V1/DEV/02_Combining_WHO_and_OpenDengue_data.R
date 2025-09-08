#' ---
#' title: "02 Combining WHO and OpenDengue data"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Combine WHO and OpenDengue data. For countries in both use coverage comparison to select year.   
#' 
#' Timeline:
#' =========
#' 14-05-2025: Combined WHO and OpenDengue data. 
#' 19-06-2025: Corrected sourced function filepaths.
#' 29-07-2025: Added code to save file. Moved to V1/DEV, updated filepaths for sourced functions + scripts.
#' 08-09-2025: Updated WHO data used. 

library(dplyr)
library(tidyverse)
library(readr)
library(countrycode)
library(readxl)
library(imputeTS)

Choosing_WHO_or_OpenDengue_data_by_country_year_env <- new.env()
Comparing_coverage_between_WHO_and_OpenDengue <- source("V1/DEV/01_Choosing_WHO_or_OpenDengue_data_by_country_year.R", 
                                                        local = Choosing_WHO_or_OpenDengue_data_by_country_year_env)$value

#--------------- Loading data
OD_national_extract <- read_csv("Data/OpenDengue_V1_2/National_extract_V1_2.csv")
WHO_data <- read_excel("Data/WHO_dengue_data/dengue-global-data-2025-09-08.xlsx")

#--------------- Cleaning data 
source("V1/DEV/Functions/00_FUN_WHO_data_cleaning.R", local = environment())
WHO_data_clean <- WHO_data_cleaning(WHO_data)

#--------------- Filtering OpenDengue and WHO datasets for chosen years 

# OpenDengue 
source("V1/DEV/Functions/00_FUN_extracting_desired_OpenDengue_data.R", local = environment())
target_OpenDengue_data <- extracting_desired_OpenDengue_data_fun(OD_national_extract, Comparing_coverage_between_WHO_and_OpenDengue)

# WHO
source("V1/DEV/Functions/00_FUN_extracting_desired_WHO_data.R", local = environment())
target_WHO_data <- extracting_desired_WHO_data_fun(WHO_data_clean, Comparing_coverage_between_WHO_and_OpenDengue)

#--------------- Preparing OpenDengue data to join to WHO 

#----- Identify highest continuity data source by year + where duplicate counts exist select the source with the highest continuity. 
source("V1/DEV/Functions/00_FUN_deduplicating_OpenDengue_data.R", local = environment())
target_OpenDengue_data_dedup <- deduplicating_OpenDengue_data_FUN(target_OpenDengue_data)

#----- Interpolating missing OpenDengue data

#' Remove countries with only 1 data point - not suitable for interpolation:  
#' This removes PAHO countries who started reporting in the last week of 2013 - data from 2014 onwards taken from WHO database. 
target_OpenDengue_data_dedup_split <- split(target_OpenDengue_data_dedup, target_OpenDengue_data_dedup$ISO_A0)
target_OpenDengue_data_dedup_split_clean <- target_OpenDengue_data_dedup_split[lapply(target_OpenDengue_data_dedup_split,
                                                                                      nrow) %>% Reduce(c,.) > 1]

# Interpolate
source("V1/DEV/Functions/00_FUN_interpolating_missing_OpenDengue_data.R", local = environment())
target_OpenDengue_data_interpolated_split <- Map(interpolating_missing_OpenDengue_data_FUN, target_OpenDengue_data_dedup_split_clean)

#----- Disaggregating weeks crossing months 
# Allocate cases to respective months, assume equal case numbers on every day in weeks crossing months. 
source("V1/DEV/Functions/00_FUN_disaggregating_OpenDengue_weeks_crossing_months.R", local = environment())
target_OpenDengue_data_interpolated_split_clean <- Map(disaggregating_OpenDengue_weeks_crossing_months, target_OpenDengue_data_interpolated_split)

#----- Aggregate weekly --> monthly 
source("V1/DEV/Functions/00_FUN_weekly_to_monthly_aggregation_OpenDengue.R", local = environment())
target_OpenDengue_data_monthly_aggregate <- Map(weekly_to_monthly_aggregation_OpenDengue_fun, 
                                                                    target_OpenDengue_data_interpolated_split_clean)

#----- Prepare OpenDengue data to combine with WHO data 
source("V1/DEV/Functions/00_FUN_prep_OpenDengue_data_to_combine_with_WHO.R", local = environment())
target_OpenDengue_data_monthly_aggregate_FINAL <- prep_OpenDengue_data_to_combine_with_WHO_fun(target_OpenDengue_data_monthly_aggregate)

#--------------- Preparing WHO data to join to OpenDengue 

#----- Interpolating missing WHO data
source("V1/DEV/Functions/00_FUN_interpolating_missing_WHO_data.R", local = environment())
target_WHO_data_split <- split(target_WHO_data, target_WHO_data$iso3)
target_WHO_data_interpolated_split <- Map(interpolating_missing_WHO_data_FUN, target_WHO_data_split)

#----- Prepare WHO data to combine with OpenDengue data 
source("V1/DEV/Functions/00_FUN_prep_WHO_data_to_combine_with_OpenDengue.R", local = environment())
target_WHO_data_FINAL <- prep_WHO_data_to_combine_with_OpenDengue_fun(target_WHO_data_interpolated_split)

#--------------- Combining data from WHO and OpenDengue 

WHO_OD_target_data_combined <- rbind(target_OpenDengue_data_monthly_aggregate_FINAL, 
                                     target_WHO_data_FINAL)

#--------------- Cleaning combined data 
source("V1/DEV/Functions/00_FUN_clean_WHO_OpenDengue_combined_data.R", local = environment())
WHO_OD_target_data_combined_clean <- clean_WHO_OpenDengue_combined_data_fun(WHO_OD_target_data_combined)

#--------------- Filter combined data for complete years, all zero years, and countries with at least 3 years of data.
source("V1/DEV/Functions/00_FUN_filtering_OpenDengue_WHO_combined_data.R", local = environment())
WHO_OD_target_data_combined_FINAL <- filtering_OpenDengue_WHO_combined_data_fun(WHO_OD_target_data_combined_clean)

#--------------- Saving 

dir_to_save <- paste0("V1/DEV/02_Combining_WHO_and_OpenDengue_data/", Sys.Date()) 
dir.create(dir_to_save, recursive = TRUE)

write_csv(WHO_OD_target_data_combined_FINAL, 
          file = paste0(dir_to_save, "/National_clean_data.csv"))

