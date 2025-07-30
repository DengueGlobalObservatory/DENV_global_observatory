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
#' 
#' Timeline:
#' ========

library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(circular)

#--------------- Loading data
#' Load the most recent version of the national dataset. 
national_data_dirs <- list.dirs("V1/DEV/02_Combining_WHO_and_OpenDengue_data", recursive = FALSE, full.names = FALSE) # %>%
national_data_dirs_dates <- as.Date(national_data_dirs, format = "%Y-%m-%d")

if (length(national_data_dirs_dates) == 1) {
  national_data_old_target_version <- national_data_dirs_dates
} else if (length(national_data_dirs_dates) > 1) {
  national_data_old_target_version <- national_data_dirs_dates[which.max(national_data_dirs_dates)]
} else if (length(national_data_dirs_dates) == 0) {
  stop("No data in national data directory.")
}

national_data_old <- read_csv(paste0("V1/DEV/02_Combining_WHO_and_OpenDengue_data/", national_data_old_target_version , "/National_clean_data.csv"))

#--------------- Assessing whether a new year of data is available
source("V1/DEV/Functions/00_FUN_adding_new_data_to_seasonal_baseline_ID_data.R")
national_data_updated <- adding_new_data_to_seasonal_baseline_ID_data(national_data_old, prediction_data)

#--------------- Align from calendar year to dengue season 
source("V1/DEV/Functions/00_FUN_aligning_from_calendar_year_to_dengue_season.R")
national_data_season_aligned <- aligning_from_calendar_year_to_dengue_season(national_data_updated)

#--------------- Filtering by season instead of calendar year
source("V1/DEV/Functions/00_FUN_filtering_season_aligned_OpenDengue_WHO_combined_data.R")
national_data_season_aligned_filtered <- filtering_season_aligned_OpenDengue_WHO_combined_data_fun(national_data_season_aligned)

#--------------- Identify average seasonal profile 
source("V1/DEV/Functions/00_FUN_identify_average_seasonal_profile.R")
national_ave_seasonal_profile <- identify_average_seasonal_profile(national_data_season_aligned_filtered)

#--------------- Saving 
dir_to_save <- paste0("V1/DEV/02_identify_seasonal_baseline/", Sys.Date())
dir.create(dir_to_save)

write_csv(national_ave_seasonal_profile, 
          file = paste0(dir_to_save, "/National_average_seasonal_profile.csv"))



#-------------------- ADD A LOG FOR WHETHER NATIONAL AVERAGE SEASONAL PROFILES HAVE BEEN UPDATED 