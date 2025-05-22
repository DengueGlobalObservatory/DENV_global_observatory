#' ---
#' title: "02_Cross_validation_of_monthly_proportion_of_cases_desired_use_case"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Test performance of monthly case proportion as predictor in desired use case, i.e. predicting the monthly distribution of cases from each month observed and the total annual cases. 
#' Perform leave one year out cross validation and for the year in the test set iterate through the months generating a new prediction of monthly case distribution and total annual cases. 
#' 
#' Timeline:
#' =========
#' 22-05-2025: Prepared script.

library(readr)
library(dplyr)
library(tidyverse)

#--------------- Load data 
Aligning_from_calendar_year_to_dengue_season <- new.env()
source("Scripts/02_Aligning_from_calendar_year_to_dengue_season.R", local = Aligning_from_calendar_year_to_dengue_season)
season_aligned_dengue_data <- Aligning_from_calendar_year_to_dengue_season$dengue_data_season_aligned_filtered

#--------------- LOOCV to predict total seasonal cases and monthly case distribution iteratively with each year of new data 
season_aligned_dengue_data_split <- split(season_aligned_dengue_data, season_aligned_dengue_data$Country)
source("Scripts/00_FUN_Desired_use_case_LOOCV_on_monthly_proportion_of_cases.R")
dengue_data_desired_use_case_LOOCV_split <- Map(desired_use_case_LOOCV_on_monthly_proportion_of_cases, season_aligned_dengue_data_split)

#----- Extract results
annual_case_predictions <- lapply(dengue_data_desired_use_case_LOOCV_split, function(x){x$Annual_cases_LOOCV}) %>%
  Reduce(rbind,.)
iterative_monthly_case_predictions <- lapply(dengue_data_desired_use_case_LOOCV_split, function(x){x$Monthly_cases_LOOCV}) %>%
  Reduce(rbind,.)



