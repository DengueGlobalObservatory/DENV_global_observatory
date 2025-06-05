#' ---
#' title: "03_Visualising_total_seasonal_case_prediction_CV"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Visualise results from LOOCV of total seasonal actual vs predicted cases. 
#' 
#' Timeline:
#' =========
#' 22-05-2025: Prepared script.
#' 02-06-2025: Added in code to show iterative improvement in prediction of total seasonal case number with each new month of data.

library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(patchwork)
library(Metrics)
library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(exactextractr)

#--------------- Load results
seasonal_case_predictions <- read_csv("Results/Slow_processing_results/02_Cross_validation_of_monthly_proportion_of_cases_desired_use_case/Monthly_iterative_total_seasonal_case_predictions.csv")
iterative_monthly_case_predictions <- read_csv("Results/Slow_processing_results/02_Cross_validation_of_monthly_proportion_of_cases_desired_use_case/Monthly_iterative_seasonal_caseload_distribution_predictions.csv")

#--------------- Assess performance predicting total seasonal cases 
source("Scripts/00_FUN_Visualise_monthly_prop_seasonal_case_prediction_performance_desired_use_case.R")
total_seasonal_cases_LOOCV_plots <- Visualise_monthly_prop_seasonal_case_prediction_performance_desired_use_case(seasonal_case_predictions)
acc_vs_pred_annual_cases_rmse_scatterplot <- total_seasonal_cases_LOOCV_plots$rmse_scatterplot
acc_vs_pred_annual_cases_rmse_heatmap <- total_seasonal_cases_LOOCV_plots$rmse_heatmap
acc_vs_pred_annual_cases_rmse_data <- total_seasonal_cases_LOOCV_plots$rmse_data

#--------------- Assess iterative improvement in performance predicting total seasonal cases with each new month of data. 
source("Scripts/00_FUN_Visualise_iterative_improvement_in_monthly_prop_seasonal_case_prediction_performance.R")
Iterative_improvement_in_total_seasonal_case_prediction <- Visualise_iterative_improvement_in_monthly_prop_seasonal_case_prediction_performance(seasonal_case_predictions)
Iterative_improvement_total_seasonal_cases_pred_data <- Iterative_improvement_in_total_seasonal_case_prediction$Iterative_improvement_total_seasonal_cases_pred_data

#- Heatmaps
Iterative_improvement_total_seasonal_cases_pred_0.75_benchmark_heatmap <- Iterative_improvement_in_total_seasonal_case_prediction$Iterative_improvement_total_seasonal_cases_pred_0.75_benchmark_heatmap
Iterative_improvement_total_seasonal_cases_pred_0.8_benchmark_heatmap <- Iterative_improvement_in_total_seasonal_case_prediction$Iterative_improvement_total_seasonal_cases_pred_0.8_benchmark_heatmap
Iterative_improvement_total_seasonal_cases_pred_0.8_benchmark_heatmap_minus_VCT <- Iterative_improvement_in_total_seasonal_case_prediction$Iterative_improvement_total_seasonal_cases_pred_0.8_benchmark_heatmap_minus_VCT
Iterative_improvement_total_seasonal_cases_pred_0.85_benchmark_heatmap <- Iterative_improvement_in_total_seasonal_case_prediction$Iterative_improvement_total_seasonal_cases_pred_0.85_benchmark_heatmap
Iterative_improvement_total_seasonal_cases_pred_0.85_benchmark_heatmap_minus_VCT <- Iterative_improvement_in_total_seasonal_case_prediction$Iterative_improvement_total_seasonal_cases_pred_0.85_benchmark_heatmap_minus_VCT
Iterative_improvement_total_seasonal_cases_pred_0.9_benchmark_heatmap <- Iterative_improvement_in_total_seasonal_case_prediction$Iterative_improvement_total_seasonal_cases_pred_0.9_benchmark_heatmap
Iterative_improvement_total_seasonal_cases_pred_0.9_benchmark_heatmap_minus_VCT <- Iterative_improvement_in_total_seasonal_case_prediction$Iterative_improvement_total_seasonal_cases_pred_0.9_benchmark_heatmap_minus_VCT

#' St Vincent and the Grenadines skewing results. Much higher total seasonal cases in season 2020/2021, reasons unclear. Massive differences between observed and predicted cases. 
#' RMSE at month with 80, 85 and 90% threshold much smaller than monthly RMSE, hence large ratio value. 
#' Setting benchmark RMSE at 75% of cases performs well - value here is of a similar order of magnitude/ one below to the raw RMSE across countries easing plot interpretability.

#- Scatterplots 
Iterative_improvement_total_seasonal_cases_pred_0.75_benchmark_scatterplot <- Iterative_improvement_in_total_seasonal_case_prediction$Iterative_improvement_total_seasonal_cases_pred_0.75_benchmark_scatterplot
Iterative_improvement_total_seasonal_cases_pred_0.8_benchmark_scatterplot <- Iterative_improvement_in_total_seasonal_case_prediction$Iterative_improvement_total_seasonal_cases_pred_0.8_benchmark_scatterplot
Iterative_improvement_total_seasonal_cases_pred_0.85_benchmark_scatterplot <- Iterative_improvement_in_total_seasonal_case_prediction$Iterative_improvement_total_seasonal_cases_pred_0.85_benchmark_scatterplot
Iterative_improvement_total_seasonal_cases_pred_0.9_benchmark_scatterplot <- Iterative_improvement_in_total_seasonal_case_prediction$Iterative_improvement_total_seasonal_cases_pred_0.9_benchmark_scatterplot

#--------------- Assess performance predicting total seasonal incidence 

#----- Convert to incidence  
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
seasonal_case_predictions_incidence <- calculate_incidence_desired_use_case(seasonal_case_predictions, 
                                                                            Pop_raster_1990, Pop_raster_1995, Pop_raster_2000, Pop_raster_2005, 
                                                                            Pop_raster_2010, Pop_raster_2015, Pop_raster_2020, Pop_raster_2025)

#----- Plot incidence results 
source("Scripts/00_FUN_Visualise_monthly_prop_seasonal_incid_prediction_performance.R")
total_seasonal_incid_LOOCV_plots <- Visualise_monthly_prop_seasonal_incid_prediction_performance(seasonal_case_predictions_incidence)
acc_vs_pred_annual_incid_rmse_data <- total_seasonal_incid_LOOCV_plots$rmse_data
acc_vs_pred_seasonal_incid_rmse_scatterplot <- total_seasonal_incid_LOOCV_plots$rmse_scatterplot
acc_vs_pred_seasonal_incid_rmse_heatmap <- total_seasonal_incid_LOOCV_plots$rmse_heatmap

#--------------- Assess iterative improvement in performance predicting total seasonal incidence with each new month of data. 
source("Scripts/00_FUN_Visualise_iterative_improvement_in_monthly_prop_seasonal_incid_prediction_performance.R")
Iterative_improvement_in_total_seasonal_incid_prediction <- Visualise_iterative_improvement_in_monthly_prop_seasonal_incid_prediction_performance(seasonal_case_predictions_incidence)
Iterative_improvement_total_seasonal_incid_pred_data <- Iterative_improvement_in_total_seasonal_incid_prediction$Iterative_improvement_total_seasonal_incid_pred_data

#- Heatmaps
Iterative_improvement_total_seasonal_incid_pred_0.75_benchmark_heatmap <- Iterative_improvement_in_total_seasonal_incid_prediction$Iterative_improvement_total_seasonal_incid_pred_0.75_benchmark_heatmap
Iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_heatmap <- Iterative_improvement_in_total_seasonal_incid_prediction$Iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_heatmap
Iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_heatmap_minus_VCT <- Iterative_improvement_in_total_seasonal_incid_prediction$Iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_heatmap_minus_VCT
Iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_heatmap <- Iterative_improvement_in_total_seasonal_incid_prediction$Iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_heatmap
Iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_heatmap_minus_VCT <- Iterative_improvement_in_total_seasonal_incid_prediction$Iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_heatmap_minus_VCT
Iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_heatmap <- Iterative_improvement_in_total_seasonal_incid_prediction$Iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_heatmap
Iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_heatmap_minus_VCT <- Iterative_improvement_in_total_seasonal_incid_prediction$Iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_heatmap_minus_VCT

#' St Vincent and the Grenadines skewing results. Much higher total seasonal cases in season 2020/2021, reasons unclear. Massive differences between observed and predicted cases. 
#' RMSE at month with 80, 85 and 90% threshold much smaller than monthly RMSE, hence large ratio value. 
#' Setting benchmark RMSE at 75% of cases performs well - value here is of a similar order of magnitude/ one below to the raw RMSE across countries easing plot interpretability.

#- Scatterplots 
Iterative_improvement_total_seasonal_incid_pred_0.75_benchmark_scatterplot <- Iterative_improvement_in_total_seasonal_incid_prediction$Iterative_improvement_total_seasonal_incid_pred_0.75_benchmark_scatterplot
Iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_scatterplot <- Iterative_improvement_in_total_seasonal_incid_prediction$Iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_scatterplot
Iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_scatterplot <- Iterative_improvement_in_total_seasonal_incid_prediction$Iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_scatterplot
Iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_scatterplot <- Iterative_improvement_in_total_seasonal_incid_prediction$Iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_scatterplot

#--------------- Saving 

dir_to_save <- paste0("Results/03_Visualising_total_seasonal_case_prediction_CV/", Sys.Date())
dir.create(dir_to_save, recursive = TRUE)

#----- Assess performance predicting total seasonal cases

write_csv(acc_vs_pred_annual_cases_rmse_data,
          file = paste0(dir_to_save, "/RMSE_monthly_iterative_seasonal_case_prediction.csv"))
ggsave(acc_vs_pred_annual_cases_rmse_scatterplot,
       filename = paste0(dir_to_save, "/RMSE_monthly_iterative_seasonal_case_predictions_scatterplot.png"))
ggsave(acc_vs_pred_annual_cases_rmse_heatmap,
       filename = paste0(dir_to_save, "/RMSE_vs_monthly_iterative_seasonal_case_predictions_heatmap.png"))

#----- Assess iterative improvement in performance predicting total seasonal cases with each new month of data. 

write_csv(Iterative_improvement_total_seasonal_cases_pred_data, 
          file = paste0(dir_to_save, "/RMSE_ratio_iter_improvement_total_season_case_pred.csv"))

#- Heatmaps
ggsave(Iterative_improvement_total_seasonal_cases_pred_0.75_benchmark_heatmap, 
       filename = paste0(dir_to_save, "/Iter_improvement_in_total_seasonal_cases_pred_0.75_benchmark_heatmap.png"))
ggsave(Iterative_improvement_total_seasonal_cases_pred_0.8_benchmark_heatmap, 
       filename = paste0(dir_to_save, "/Iter_improvement_in_total_seasonal_cases_prediction_0.8_benchmark_heatmap.png"))
ggsave(Iterative_improvement_total_seasonal_cases_pred_0.8_benchmark_heatmap_minus_VCT, 
       filename = paste0(dir_to_save, "/Iter_improvement_in_total_seasonal_cases_prediction_0.8_benchmark_heatmap_minus_VCT.png"))
ggsave(Iterative_improvement_total_seasonal_cases_pred_0.85_benchmark_heatmap, 
       filename = paste0(dir_to_save, "/Iter_improvement_in_total_seasonal_cases_prediction_0.85_benchmark_heatmap.png"))
ggsave(Iterative_improvement_total_seasonal_cases_pred_0.85_benchmark_heatmap_minus_VCT, 
       filename = paste0(dir_to_save, "/Iter_improvement_in_total_seasonal_cases_prediction_0.85_benchmark_heatmap_minus_VCT.png"))
ggsave(Iterative_improvement_total_seasonal_cases_pred_0.9_benchmark_heatmap, 
       filename = paste0(dir_to_save, "/Iter_improvement_in_total_seasonal_cases_prediction_0.9_benchmark_heatmap.png"))
ggsave(Iterative_improvement_total_seasonal_cases_pred_0.9_benchmark_heatmap_minus_VCT, 
       filename = paste0(dir_to_save, "/Iter_improvement_in_total_seasonal_cases_prediction_0.9_benchmark_heatmap_minus_VCT.png"))

#- Scatterplot
ggsave(Iterative_improvement_total_seasonal_cases_pred_0.75_benchmark_scatterplot,
       filename = paste0(dir_to_save, "/Iter_improvement_total_seasonal_cases_pred_0.75_benchmark_scatterplot.png"))
ggsave(Iterative_improvement_total_seasonal_cases_pred_0.8_benchmark_scatterplot,
       filename = paste0(dir_to_save, "/Iter_improvement_total_seasonal_cases_pred_0.8_benchmark_scatterplot.png"))
ggsave(Iterative_improvement_total_seasonal_cases_pred_0.85_benchmark_scatterplot,
       filename = paste0(dir_to_save, "/Iter_improvement_total_seasonal_cases_pred_0.85_benchmark_scatterplot.png"))
ggsave(Iterative_improvement_total_seasonal_cases_pred_0.9_benchmark_scatterplot,
       filename = paste0(dir_to_save, "/Iter_improvement_total_seasonal_incid_pred_0.9_benchmark_scatterplot.png"))

#----- Assess performance predicting total seasonal incidence 

write_csv(acc_vs_pred_annual_incid_rmse_data,
          file = paste0(dir_to_save, "/RMSE_monthly_iterative_seasonal_incid_predictions_data.csv"))
ggsave(acc_vs_pred_seasonal_incid_rmse_scatterplot,
       filename = paste0(dir_to_save, "/RMSE_monthly_iterative_seasonal_incid_predictions_scatter.png"))
ggsave(acc_vs_pred_seasonal_incid_rmse_heatmap,
       filename = paste0(dir_to_save, "/RMSE_monthly_iterative_seasonal_incid_predictions_heatmap.png"))

#----- Assess iterative improvement in performance predicting total seasonal incidence with each new month of data. 

write_csv(Iterative_improvement_total_seasonal_incid_pred_data, 
          file = paste0(dir_to_save, "/RMSE_ratio_iter_improvement_total_season_incid_pred.csv"))

#- Heatmaps
ggsave(Iterative_improvement_total_seasonal_incid_pred_0.75_benchmark_heatmap, 
       filename = paste0(dir_to_save, "/Iter_improvement_in_total_seasonal_incid_pred_0.75_benchmark_heatmap.png"))
ggsave(Iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_heatmap, 
       filename = paste0(dir_to_save, "/Iter_improvement_in_total_seasonal_incid_pred_0.8_benchmark_heatmap.png"))
ggsave(Iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_heatmap_minus_VCT, 
       filename = paste0(dir_to_save, "/Iter_improvement_in_total_seasonal_incid_pred_0.8_benchmark_heatmap_minus_VCT.png"))
ggsave(Iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_heatmap, 
       filename = paste0(dir_to_save, "/Iter_improvement_in_total_seasonal_incid_pred_0.85_benchmark_heatmap.png"))
ggsave(Iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_heatmap_minus_VCT, 
       filename = paste0(dir_to_save, "/Iter_improvement_in_total_seasonal_incid_pred_0.85_benchmark_heatmap_minus_VCT.png"))
ggsave(Iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_heatmap, 
       filename = paste0(dir_to_save, "/Iter_improvement_in_total_seasonal_incid_pred_0.9_benchmark_heatmap.png"))
ggsave(Iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_heatmap_minus_VCT, 
       filename = paste0(dir_to_save, "/Iter_improvement_in_total_seasonal_incid_pred_0.9_benchmark_heatmap_minus_VCT.png"))

#- Scatterplot
ggsave(Iterative_improvement_total_seasonal_incid_pred_0.75_benchmark_scatterplot,
       filename = paste0(dir_to_save, "/Iter_improvement_total_seasonal_incid_pred_0.75_benchmark_scatter.png"))
ggsave(Iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_scatterplot,
       filename = paste0(dir_to_save, "/Iter_improvement_total_seasonal_incid_pred_0.8_benchmark_scatter.png"))
ggsave(Iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_scatterplot,
       filename = paste0(dir_to_save, "/Iter_improvement_total_seasonal_incid_pred_0.85_benchmark_scatter.png"))
ggsave(Iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_scatterplot,
       filename = paste0(dir_to_save, "/Iter_improvement_total_seasonal_incid_pred_0.9_benchmark_scatter.png"))



