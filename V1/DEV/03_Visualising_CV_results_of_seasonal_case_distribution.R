#' ---
#' title: "03_Visualising_CV_results_of_seasonal_case_distribution"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Visualise results from LOOCV of monthly actual vs predicted cases. 
#' 
#' Timeline:
#' =========
#' 02-06-2025: Prepared script.
#' 05-06-2025: Removed code to calculate incidence (moved to 02S_Cross_validation_of_monthly_proportion_of_cases_desired_use_case). 
#'             Split raw prediction error by month heatmaps into bands to increase interpretability (large error values previously making it difficult to distinguish smaller values). 
#'             Used same groupings to split iterative improvement in monthly case prediction. Generated combined plots to show raw error vs iterative reduction in error. 
#' 19-06-2025: Corrected sourced function filepaths.

library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(patchwork)
library(Metrics)
library(terra)
library(sf)
library(circular)
library(imputeTS)
library(rnaturalearth)
library(rnaturalearthdata)
library(exactextractr)

#--------------- Loading results 
iterative_monthly_load_predictions <- read_csv("Results/Slow_processing_results/02_Cross_validation_of_monthly_proportion_of_cases_desired_use_case/2025-06-05/Monthly_iterative_seasonal_load_dist_predictions.csv")

#--------------- Assess performance predicting monthly cases iteratively 

#' Calculate incidence then generate plots for cases and incidence distribution in a single function

#----- Add peak month + identify average
source("Scripts/Functions/00_FUN_identify_peak_month.R")
iterative_monthly_load_predictions_clean <- identify_peak_month(iterative_monthly_load_predictions)

#----- Plot results 
source("Scripts/Functions/00_FUN_Visualise_monthly_prop_iterative_monthly_incid_prediction_performance_desired_use_case.R")
iterative_monthly_load_LOOCV_plots <- Visualise_monthly_prop_iterative_monthly_incid_prediction_performance_desired_use_case_fun(iterative_monthly_load_predictions_clean)

# Data 
iterative_monthly_load_LOOCV_RMSE_MAE_data <- iterative_monthly_load_LOOCV_plots$Plot_data

# All country heatmaps
acc_vs_pred_monthly_cases_rmse_heatmap <- iterative_monthly_load_LOOCV_plots$rmse_cases_heatmap
acc_vs_pred_monthly_incid_rmse_heatmap <- iterative_monthly_load_LOOCV_plots$rmse_incid_heatmap
acc_vs_pred_monthly_cases_mae_heatmap <- iterative_monthly_load_LOOCV_plots$mae_cases_heatmap
acc_vs_pred_monthly_incid_mae_heatmap <- iterative_monthly_load_LOOCV_plots$mae_incid_heatmap

# Heatmaps split by max rmse/ mae of cases/ incid 
split_rmse_cases_heatmap <- iterative_monthly_load_LOOCV_plots$split_rmse_cases_heatmap
split_rmse_incid_heatmap <- iterative_monthly_load_LOOCV_plots$split_rmse_incid_heatmap
split_mae_cases_heatmap <- iterative_monthly_load_LOOCV_plots$split_mae_cases_heatmap
split_mae_incid_heatmap <- iterative_monthly_load_LOOCV_plots$split_mae_incid_heatmap

#--------------- Assess improvement in performance of iterative monthly cases prediction 
source("Scripts/Functions/00_FUN_Visualise_iterative_improvement_in_monthly_incid_prediction.R")
Iterative_improvement_monthly_load_pred <- Visualise_iterative_improvement_in_monthly_incid_prediction(iterative_monthly_load_LOOCV_RMSE_MAE_data)

# Data
Iterative_improvement_monthly_load_pred_plot_data <- Iterative_improvement_monthly_load_pred$Plot_data

# All country heatmaps
Iterative_improvement_in_monthly_case_prediction_heatmap_rmse <- Iterative_improvement_monthly_load_pred$Iterative_improvement_in_monthly_case_prediction_heatmap_rmse
Iterative_improvement_in_monthly_incid_prediction_heatmap_rmse <- Iterative_improvement_monthly_load_pred$Iterative_improvement_in_monthly_incid_prediction_heatmap_rmse
Iterative_improvement_in_monthly_case_prediction_heatmap_mae <- Iterative_improvement_monthly_load_pred$Iterative_improvement_in_monthly_case_prediction_heatmap_mae
Iterative_improvement_in_monthly_incid_prediction_heatmap_mae <- Iterative_improvement_monthly_load_pred$Iterative_improvement_in_monthly_incid_prediction_heatmap_mae

# Heatmaps split by max rmse/ mae of cases/ incid 
Split_iterative_improvement_in_monthly_case_prediction_heatmap_rmse <- Iterative_improvement_monthly_load_pred$Split_iterative_improvement_in_monthly_case_prediction_heatmap_rmse
Split_iterative_improvement_in_monthly_incid_prediction_heatmap_rmse <- Iterative_improvement_monthly_load_pred$Split_iterative_improvement_in_monthly_incid_prediction_heatmap_rmse
Split_iterative_improvement_in_monthly_case_prediction_heatmap_mae <- Iterative_improvement_monthly_load_pred$Split_iterative_improvement_in_monthly_case_prediction_heatmap_mae
Split_iterative_improvement_in_monthly_incid_prediction_heatmap_mae <- Iterative_improvement_monthly_load_pred$Split_iterative_improvement_in_monthly_incid_prediction_heatmap_mae

#--------------- Combine paired split plots with absolute error and reduction in error with each new month of data available  
source("Scripts/Functions/00_FUN_Combine_raw_error_and_iterative_improvement_heatmaps.R")
paired_raw_error_and_error_reduction_heatmaps <- Combine_raw_error_and_iterative_improvement_heatmaps(split_rmse_cases_heatmap, 
                                                                                                      split_rmse_incid_heatmap, 
                                                                                                      split_mae_cases_heatmap, 
                                                                                                      split_mae_incid_heatmap,
                                                                                                      Split_iterative_improvement_in_monthly_case_prediction_heatmap_rmse, 
                                                                                                      Split_iterative_improvement_in_monthly_incid_prediction_heatmap_rmse, 
                                                                                                      Split_iterative_improvement_in_monthly_case_prediction_heatmap_mae, 
                                                                                                      Split_iterative_improvement_in_monthly_incid_prediction_heatmap_mae)

RMSE_cases_combined_heatmaps <- paired_raw_error_and_error_reduction_heatmaps$RMSE_cases_combined_heatmaps
RMSE_incid_combined_heatmaps <- paired_raw_error_and_error_reduction_heatmaps$RMSE_incid_combined_heatmaps
MAE_cases_combined_heatmaps <- paired_raw_error_and_error_reduction_heatmaps$MAE_cases_combined_heatmaps
MAE_incid_combined_heatmaps <- paired_raw_error_and_error_reduction_heatmaps$MAE_incid_combined_heatmaps

#--------------- Saving 

dir_to_save <- paste0("Results/03_Visualising_CV_results_of_seasonal_case_distribution/", Sys.Date())
dir.create(dir_to_save)

#----- All country heatmaps
# RMSE
ggsave(acc_vs_pred_monthly_cases_rmse_heatmap,
       filename = paste0(dir_to_save, "/RMSE_of_observed_vs_monthly_iterative_case_predictions_heatmap.png"))
ggsave(acc_vs_pred_monthly_incid_rmse_heatmap,
       filename = paste0(dir_to_save, "/RMSE_of_observed_vs_monthly_iterative_incid_predictions_heatmap.png"))

# MAE
ggsave(acc_vs_pred_monthly_cases_mae_heatmap,
       filename = paste0(dir_to_save, "/MAE_of_observed_vs_monthly_iterative_case_predictions_heatmap.png"))

ggsave(acc_vs_pred_monthly_incid_mae_heatmap,
       filename = paste0(dir_to_save, "/MAE_of_observed_vs_monthly_iterative_incid_predictions_heatmap.png"))

#----- Combined raw error and iterative error reduction heatmaps 

# RMSE 
walk2(RMSE_cases_combined_heatmaps, names(RMSE_cases_combined_heatmaps), 
      ~ ggsave(filename = paste0(dir_to_save, "/RMSE_cases_combined_plot_", .y, ".png"), plot = .x, dpi = 300, width = 20, height = 16, units = "in"))
walk2(RMSE_incid_combined_heatmaps, names(RMSE_incid_combined_heatmaps), 
      ~ ggsave(filename = paste0(dir_to_save, "/RMSE_incid_combined_plot_", .y, ".png"), plot = .x, dpi = 300, width = 20, height = 16, units = "in"))

# MAE 
walk2(MAE_cases_combined_heatmaps, names(MAE_cases_combined_heatmaps), 
      ~ ggsave(filename = paste0(dir_to_save, "/MAE_cases_combined_plot_", .y, ".png"), plot = .x, dpi = 300, width = 20, height = 16, units = "in"))
walk2(MAE_incid_combined_heatmaps, names(MAE_incid_combined_heatmaps), 
      ~ ggsave(filename = paste0(dir_to_save, "/MAE_incid_combined_plot_", .y, ".png"), plot = .x, dpi = 300, width = 20, height = 16, units = "in"))
