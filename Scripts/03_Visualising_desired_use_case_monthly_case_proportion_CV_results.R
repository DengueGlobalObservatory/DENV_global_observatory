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

#--------------- Assess performance predicting monthly cases iteratively 

#----- Calculate incidence
source("Scripts/00_FUN_Calculate_incidence_monthly_case_prediction_desired_use_case.R")
iterative_monthly_case_predictions_incidence <- Calculate_incidence_monthly_case_prediction_desired_use_case(iterative_monthly_case_predictions, 
                                                                                                             Pop_raster_1990, Pop_raster_1995, Pop_raster_2000, Pop_raster_2005, 
                                                                                                             Pop_raster_2010, Pop_raster_2015, Pop_raster_2020, Pop_raster_2025)

#----- Plot results 
source("Scripts/00_FUN_Visualise_monthly_prop_iterative_monthly_incid_prediction_performance_desired_use_case.R")
iterative_monthly_cases_LOOCV_plots <- Visualise_monthly_prop_iterative_monthly_incid_prediction_performance_desired_use_case_fun(iterative_monthly_case_predictions_incidence)
acc_vs_pred_monthly_incid_cor_heatmap <- iterative_monthly_cases_LOOCV_plots$cor_heatmap
acc_vs_pred_monthly_cases_rmse_heatmap <- iterative_monthly_cases_LOOCV_plots$rmse_cases_heatmap
acc_vs_pred_monthly_cases_rmse_heatmap_minus_brazil <- iterative_monthly_cases_LOOCV_plots$rmse_cases_heatmap_minus_brazil
acc_vs_pred_monthly_incid_rmse_heatmap <- iterative_monthly_cases_LOOCV_plots$rmse_incid_heatmap
acc_vs_pred_monthly_incid_rmse_heatmap_minus_st_barthelemy <- iterative_monthly_cases_LOOCV_plots$rmse_incid_heatmap_minus_st_barthelemy

#--------------- Saving 

dir_to_save <- paste0("Results/03_Visualising_desired_use_case_monthly_case_proportion_CV_results/", Sys.Date())
dir.create(dir_to_save)

#----- Assess performance predicting total seasonal cases

#- Correlation
ggsave(acc_vs_pred_annual_cases_cor_scatterplot,
       filename = paste0(dir_to_save, "/Cor_of_observed_vs_monthly_iterative_annual_case_predictions_scatterplot.png"))
ggsave(acc_vs_pred_annual_cases_cor_heatmap,
  filename = paste0(dir_to_save, "/Correlation_of_observed_vs_monthly_iterative_annual_case_predictions_heatmap.png"))

#- RMSE
ggsave(acc_vs_pred_annual_cases_rmse_scatterplot,
       filename = paste0(dir_to_save, "/RMSE_of_observed_vs_monthly_iterative_annual_case_predictions_scatterplot.png"))
ggsave(acc_vs_pred_annual_cases_rmse_heatmap,
       filename = paste0(dir_to_save, "/RMSE_of_observed_vs_monthly_iterative_annual_case_predictions_heatmap.png"))

#- RMSE Incid
ggsave(acc_vs_pred_annual_incid_rmse_scatterplot,
       filename = paste0(dir_to_save, "/RMSE_of_observed_vs_monthly_iterative_annual_incidence_predictions_scatterplot.png"))
ggsave(acc_vs_pred_annual_incid_rmse_heatmap,
       filename = paste0(dir_to_save, "/RMSE_of_observed_vs_monthly_iterative_annual_incidence_predictions_heatmap.png"))

#' Cor incid should be same as cor cases therefore didn't save these plots. 

#--------------- Assess performance predicting monthly cases iteratively 

#- Correlation 
ggsave(acc_vs_pred_monthly_incid_cor_heatmap,
       filename = paste0(dir_to_save, "/Correlation_of_observed_vs_monthly_iterative_case_predictions_heatmap.png"))

ggsave(acc_vs_pred_monthly_cases_rmse_heatmap,
       filename = paste0(dir_to_save, "/RMSE_of_observed_vs_monthly_iterative_case_predictions_heatmap.png"))

ggsave(acc_vs_pred_monthly_cases_rmse_heatmap_minus_brazil,
       filename = paste0(dir_to_save, "/RMSE_of_observed_vs_monthly_iterative_case_predictions_heatmap_minus_Brazil.png"))

ggsave(acc_vs_pred_monthly_incid_rmse_heatmap,
       filename = paste0(dir_to_save, "/RMSE_of_observed_vs_monthly_iterative_incid_predictions_heatmap.png"))

ggsave(acc_vs_pred_monthly_incid_rmse_heatmap_minus_st_barthelemy,
       filename = paste0(dir_to_save, "/RMSE_of_observed_vs_monthly_iterative_incid_predictions_heatmap_minus_STM.png"))

