#' ---
#' title: "02 Cross validation on monthly proportion of cases"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Calculate monthly proportion of cases. 
#' Perform LOOCV and for countries with >= 10 years of continuous data available perform 5-year rolling CV.
#' 
#' Timeline:
#' =========
#' 14-05-2025: Performed LOOCV + 5-year rolling CV for countries with >= 10 years of continuous data.
#' 19-06-2025: Corrected sourced function filepaths.

library(dplyr)
library(tidyverse)
library(readr)
library(countrycode)
library(readxl)
library(imputeTS)
library(slider)

Combining_WHO_and_OpenDengue_data <- new.env()
dengue_data <- source("Scripts/02_Combining_WHO_and_OpenDengue_data.R",
                      local = Combining_WHO_and_OpenDengue_data)$value

#--------------- Leave one out cross validation
dengue_data_split <- split(dengue_data, dengue_data$Country)
source("Scripts/Functions/00_FUN_LOOCV_on_monthly_proportion_of_cases.R")
dengue_data_LOOCV_split <- Map(LOOCV_on_monthly_proportion_of_cases_fun, dengue_data_split)
dengue_data_LOOCV_df <- Reduce(rbind, dengue_data_LOOCV_split)

#--------------- Rolling cross validation

#----- Filtering for countries with at least 10 years of continuous data 
source("Scripts/Functions/00_FUN_filtering_for_10_years_of_continuous_data.R")
dengue_data_10_year_filtered <- filtering_for_10_years_of_continuous_data_fun(dengue_data)

#----- Performing rolling CV 
source("Scripts/Functions/00_FUN_rolling_cross_validation.R")
dengue_data_5_year_rolling_CV <- rolling_cross_validation_fun(dengue_data_10_year_filtered, 5)

