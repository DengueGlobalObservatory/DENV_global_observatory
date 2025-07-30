#' ---
#' title: "03_Lineplot_ave_and_CIs_monthly_case_proportion"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Generate a lineplot showing the average and 95% CIs of the monthly proportion of cases by country.
#' 
#' Timeline:
#' =========
#' 20-06-2025: Initial commit. 

library(dplyr)
library(tidyverse)
library(readr)
library(countrycode)
library(readxl)
library(imputeTS)

#--------------- Load data
Aligning_from_calendar_year_to_dengue_season <- new.env()
source("Scripts/02_Aligning_from_calendar_year_to_dengue_season.R", local = Aligning_from_calendar_year_to_dengue_season)
season_aligned_dengue_data <- Aligning_from_calendar_year_to_dengue_season$dengue_data_season_aligned_filtered

#--------------- Generate lineplots of average monthly cumulative proportion of cases
source("Scripts/Functions/00_FUN_generate_lineplot_ave_monthly_cum_prop_and_CIs.R")
Cumulative_monthly_proportion_lineplots <- generate_lineplot_ave_monthly_cum_prop_and_CIs(season_aligned_dengue_data)

#--------------- Saving 

dir_to_save <- paste0("Results/03_Lineplot_ave_and_CIs_monthly_case_proportion/", Sys.Date())
dir.create(dir_to_save, recursive = TRUE)

ggsave(Cumulative_monthly_proportion_lineplots,
       filename = paste0(dir_to_save, "/Average_cumulative_monthly_proportion_by_country_lineplot.png"),
       dpi = 300,
       width = 35, 
       height = 18, 
       unit = "cm")

