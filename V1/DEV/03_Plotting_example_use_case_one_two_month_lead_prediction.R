#' ---
#' title: "03_Plotting_example_use_case_one_two_month_lead_prediction"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Perform one and two month lead prediction for two sample countries, one with lots of data and one with few. 
#' Plot prediction for that given year against the actual observation within that year. 
#' 
#' Timeline:
#' =========
#' 20-06-2025: Initial commit.

library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(patchwork)

#--------------- Loading data 
iterative_monthly_load_predictions <- read_csv("Results/Slow_processing_results/02_Cross_validation_of_monthly_proportion_of_cases_desired_use_case/2025-06-20/Monthly_iterative_seasonal_load_dist_predictions.csv") %>%
  filter(!is.na(Country))

#--------------- Select one and two month lead time rows 
iterative_monthly_load_prediction_desired_lead <- iterative_monthly_load_predictions %>%
  ungroup() %>% 
  mutate(Pred_lead = Month_to_predict - season_nMonth) %>% 
  filter(Pred_lead <= 2) 

#--------------- Choose one country with lots of data and another with little 
Number_of_seasons_per_location <- iterative_monthly_load_prediction_desired_lead %>% 
  ungroup() %>% 
  group_by(Country, iso3) %>%
  summarise(Number_of_seasons = length(unique(season))) %>%
  arrange(Number_of_seasons)

#' Fiji as country with few seasons of data (3 seasons). Thailand as country with lots of data (32 seasons).

#--------------- Generate lineplots showing performance of prediction method on Thailand and Bahamas, using most recent season as test season. 
source("V1/DEV/Functions/00_FUN_plotting_example_use_case_one_two_month_lead_prediction.R")
example_use_case_plot <- plotting_example_use_case_one_two_month_lead_prediction(iterative_monthly_load_prediction_desired_lead, c("THA", "FJI"))

#--------------- Saving 
dir_to_save <- paste0("Results/03_Plotting_example_use_case_one_two_month_lead_prediction/", Sys.Date())
dir.create(dir_to_save, recursive = TRUE)

ggsave(example_use_case_plot,
       file = paste0(dir_to_save, "/Example_use_case_plot_THA_FJI.png"),
       dpi = 300, 
       width = 8,
       height = 8,
       units = "in")


