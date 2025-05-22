#' ---
#' title: "02_Aligning_from_calendar_year_to_dengue_season"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Identify average low month in dengue season. 
#' Align from calendar year to dengue season, using the low month as month 1. 
#' 
#' Timeline:
#' =========
#' 22-05-2025: Prepared script.

library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(countrycode)
library(circular)

Combining_WHO_and_OpenDengue_data <- new.env()
source("Scripts/02_Combining_WHO_and_OpenDengue_data.R", local = Combining_WHO_and_OpenDengue_data)

dengue_data <- Combining_WHO_and_OpenDengue_data$WHO_OD_target_data_combined_clean

#--------------- Align from calendar year to dengue season 
source("Scripts/00_FUN_aligning_from_calendar_year_to_dengue_season.R")
dengue_data_season_aligned <- aligning_from_calendar_year_to_dengue_season(dengue_data)

#--------------- Filtering by season instead of calendar year
source("Scripts/00_FUN_filtering_season_aligned_OpenDengue_WHO_combined_data.R")
dengue_data_season_aligned_filtered <- filtering_season_aligned_OpenDengue_WHO_combined_data_fun(dengue_data_season_aligned)
