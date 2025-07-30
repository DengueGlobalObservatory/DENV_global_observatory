#' ---
#' title: "03_generate_and_plot_predictions"
#' author: "K Joshi"
#' 
#' ---
#'
#' Overview:
#' ========
#' 


library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(terra)
library(exactextractr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

#--------------- Load data 
#' Load the most up to date version of the national ave seasonal profiles. 
ave_seasonal_profile_dirs <- list.dirs("V1/DEV/02_identify_seasonal_baseline/", recursive = FALSE, full.names = FALSE)  
ave_seasonal_profile_dirs_dates <- as.Date(ave_seasonal_profile_dirs, format = "%Y-%m-%d")

if (length(ave_seasonal_profile_dirs_dates) == 1) {
  target_data <- ave_seasonal_profile_dirs_dates
} else if (length(ave_seasonal_profile_dirs_dates) > 1) {
  target_data <- ave_seasonal_profile_dirs_dates[which.max(ave_seasonal_profile_dirs_dates)]
} else if (length(ave_seasonal_profile_dirs_dates) == 0) {
  stop("No data in national data directory.")
}

ave_seasonal_profiles <- read_csv(paste0("V1/DEV/02_identify_seasonal_baseline/", target_data , "/National_average_seasonal_profile.csv"))

#--------------- Two month ahead predictions 
Combining_WHO_and_OpenDengue_data <- new.env()
source("V1/DEV/02_Combining_WHO_and_OpenDengue_data.R", local = Combining_WHO_and_OpenDengue_data)
MOCK_PREDICTION_DATA <- Combining_WHO_and_OpenDengue_data$WHO_OD_target_data_combined_clean %>% filter(Year == 2025)

source("V1/DEV/Functions/00_FUN_two_month_ahead_predictions.R")
predictions <- two_month_ahead_predictions(ave_seasonal_profiles, MOCK_PREDICTION_DATA)

#--------------- Calculate incidence 
source("V1/DEV/Functions/00_FUN_calculate_incidence_for_predictions.R")

Pop_raster_2025 <- rast("Data/GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2030 <- rast("Data/GHS_POP_E2030_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2030_GLOBE_R2023A_4326_30ss_V1_0.tif")

predictions_incid <- calculate_incidence_for_predictions(predictions, Pop_raster_2025, Pop_raster_2030)

#--------------- Visualising predictions 
source("V1/DEV/Functions/00_FUN_plotting_two_month_ahead_predictions.R")
tmp_predictions_incid <- predictions_incid %>%
  filter(iso3 == "BRA")

tmp_ave_seasonal_profiles <- ave_seasonal_profiles %>%
  filter(iso3 == "BRA")

tmp_res <- plotting_two_month_ahead_predictions(tmp_ave_seasonal_profiles, tmp_predictions_incid)
tmp_res[[2]]
