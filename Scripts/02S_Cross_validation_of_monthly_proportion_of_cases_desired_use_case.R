#' ---
#' title: "02S_Cross_validation_of_monthly_proportion_of_cases_desired_use_case"
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
#' 02-06-2025: Changed incorrectly names variables from annual to seasonal and saved results.
#' 05-06-2026: Added code to save data by date generated. Added code to add incidence to LOOCV results dfs. 

library(readr)
library(dplyr)
library(tidyverse)

#--------------- Load data 
Aligning_from_calendar_year_to_dengue_season <- new.env()
source("Scripts/02_Aligning_from_calendar_year_to_dengue_season.R", local = Aligning_from_calendar_year_to_dengue_season)
season_aligned_dengue_data <- Aligning_from_calendar_year_to_dengue_season$dengue_data_season_aligned_filtered

#--------------- LOOCV to predict total seasonal cases and monthly case distribution iteratively with each season of new data 
season_aligned_dengue_data_split <- split(season_aligned_dengue_data, season_aligned_dengue_data$Country)
source("Scripts/00_FUN_Desired_use_case_LOOCV_on_monthly_proportion_of_cases.R")
dengue_data_desired_use_case_LOOCV_split <- Map(desired_use_case_LOOCV_on_monthly_proportion_of_cases, season_aligned_dengue_data_split)

#----- Extract results
seasonal_case_predictions <- lapply(dengue_data_desired_use_case_LOOCV_split, function(x){x$Seasonal_cases_LOOCV}) %>%
  Reduce(rbind,.)
iterative_monthly_case_predictions <- lapply(dengue_data_desired_use_case_LOOCV_split, function(x){x$Monthly_cases_LOOCV}) %>%
  Reduce(rbind,.)

#--------------- Add incidence 

#----- Load pop rasters  
# https://human-settlement.emergency.copernicus.eu/download.php?ds=pop
Pop_raster_1990 <- rast("Data/GHS_POP_E1990_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E1990_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_1995 <- rast("Data/GHS_POP_E1995_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E1995_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2000 <- rast("Data/GHS_POP_E2000_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2000_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2005 <- rast("Data/GHS_POP_E2005_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2005_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2010 <- rast("Data/GHS_POP_E2010_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2010_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2015 <- rast("Data/GHS_POP_E2015_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2015_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2020 <- rast("Data/GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2025 <- rast("Data/GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0.tif")

#----- Convert total seasonal case prediction to incidence
source("Scripts/00_FUN_Calculate_incidence_desired_use_case.R")
seasonal_case_predictions_incidence <- calculate_incidence_desired_use_case(seasonal_case_predictions, 
                                                                            Pop_raster_1990, Pop_raster_1995, Pop_raster_2000, Pop_raster_2005, 
                                                                            Pop_raster_2010, Pop_raster_2015, Pop_raster_2020, Pop_raster_2025)

#----- Convert monthly iterative case prediction to incidence
source("Scripts/00_FUN_Calculate_incidence_monthly_case_prediction_desired_use_case.R")
iterative_monthly_case_predictions_incidence <- Calculate_incidence_monthly_case_prediction_desired_use_case(iterative_monthly_case_predictions, 
                                                                                                             Pop_raster_1990, Pop_raster_1995, Pop_raster_2000, Pop_raster_2005, 
                                                                                                             Pop_raster_2010, Pop_raster_2015, Pop_raster_2020, Pop_raster_2025)

#--------------- Saving
dir_to_save <- paste0("Results/Slow_processing_results/02_Cross_validation_of_monthly_proportion_of_cases_desired_use_case/", Sys.Date())
dir.create(dir_to_save)

 write_csv(seasonal_case_predictions_incidence,
           paste0(dir_to_save, "/Monthly_iterative_total_seasonal_load_predictions.csv"))
 write_csv(iterative_monthly_case_predictions_incidence, 
           paste0(dir_to_save, "/Monthly_iterative_seasonal_load_dist_predictions.csv"))
