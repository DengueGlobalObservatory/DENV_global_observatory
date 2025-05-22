#' ---
#' title: "03_Visualising_desired_use_case_monthly_case_proportion_CV_results"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Visualise results from LOOCV of total seasonal and monthly, actual vs predicted cases. 
#' 
#' Timeline:
#' =========
#' 22-05-2025: Prepared script.

library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(Metrics)
library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(exactextractr)

#--------------- Load results
Cross_validation_of_monthly_proportion_of_cases_desired_use_case <- new.env()
source("Scripts/02_Cross_validation_of_monthly_proportion_of_cases_desired_use_case.R", 
       local = Cross_validation_of_monthly_proportion_of_cases_desired_use_case)

# Extract data
annual_case_predictions <- Cross_validation_of_monthly_proportion_of_cases_desired_use_case$annual_case_predictions
iterative_monthly_case_predictions <- Cross_validation_of_monthly_proportion_of_cases_desired_use_case$iterative_monthly_case_predictions


#--------------- Assess performance predicting total seasonal cases 
source("Scripts/00_FUN_Visualise_monthly_prop_annual_case_prediction_performance_desired_use_case.R")
total_seasonal_cases_LOOCV_plots <- Visualise_monthly_prop_annual_case_prediction_performance_desired_use_case(annual_case_predictions)
acc_vs_pred_annual_cases_cor_scatterplot <- total_seasonal_cases_LOOCV_plots$cor_scatterplot
acc_vs_pred_annual_cases_cor_heatmap <- total_seasonal_cases_LOOCV_plots$cor_heatmap
acc_vs_pred_annual_cases_rmse_scatterplot <- total_seasonal_cases_LOOCV_plots$rmse_scatterplot
acc_vs_pred_annual_cases_rmse_heatmap <- total_seasonal_cases_LOOCV_plots$rmse_heatmap

#---------- Convert to incidence  
# https://human-settlement.emergency.copernicus.eu/download.php?ds=pop
Pop_raster_1990 <- rast("Data/GHS_POP_E1990_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E1990_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_1995 <- rast("Data/GHS_POP_E1995_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E1995_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2000 <- rast("Data/GHS_POP_E2000_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2000_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2005 <- rast("Data/GHS_POP_E2005_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2005_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2010 <- rast("Data/GHS_POP_E2010_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2010_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2015 <- rast("Data/GHS_POP_E2015_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2015_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2020 <- rast("Data/GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2025 <- rast("Data/GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0.tif")

source("Scripts/00_FUN_Calculate_incidence_desired_use_case.R")
annual_case_predictions_incidence <- calculate_incidence_desired_use_case(annual_case_predictions, 
                                                                          Pop_raster_1990, Pop_raster_1995, Pop_raster_2000, Pop_raster_2005, 
                                                                          Pop_raster_2010, Pop_raster_2015, Pop_raster_2020, Pop_raster_2025)

#----- Plot incidence results 
source("Scripts/00_FUN_Visualise_monthly_prop_annual_incid_prediction_performance_desired_use_case.R")
total_seasonal_incid_LOOCV_plots <- Visualise_monthly_prop_annual_incid_prediction_performance_desired_use_case(annual_case_predictions_incidence)
acc_vs_pred_annual_incid_cor_scatterplot <- total_seasonal_incid_LOOCV_plots$cor_scatterplot
acc_vs_pred_annual_incid_cor_heatmap <- total_seasonal_incid_LOOCV_plots$cor_heatmap
acc_vs_pred_annual_incid_rmse_scatterplot <- total_seasonal_incid_LOOCV_plots$rmse_scatterplot
acc_vs_pred_annual_incid_rmse_heatmap <- total_seasonal_incid_LOOCV_plots$rmse_heatmap

#--------------- Assess performance predicting monthly cases 



