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
#'             Changed saving code to just save combined figs.

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
seasonal_case_predictions <- read_csv("Results/Slow_processing_results/02_Cross_validation_of_monthly_proportion_of_cases_desired_use_case/2025-06-05/Monthly_iterative_total_seasonal_load_predictions.csv")

#--------------- Assess performance predicting total seasonal cases 
source("Scripts/00_FUN_Visualise_monthly_prop_seasonal_case_prediction_performance_desired_use_case.R")
total_seasonal_cases_LOOCV_plots <- Visualise_monthly_prop_seasonal_case_prediction_performance_desired_use_case(seasonal_case_predictions)

#- Data
acc_vs_pred_seasonal_cases_rmse_data <- total_seasonal_cases_LOOCV_plots$rmse_data

#- Plots
acc_vs_pred_seasonal_cases_rmse_scatterplot <- total_seasonal_cases_LOOCV_plots$rmse_cases_scatterplot
acc_vs_pred_seasonal_cases_rmse_heatmap <- total_seasonal_cases_LOOCV_plots$rmse_cases_heatmap
acc_vs_pred_seasonal_incid_rmse_scatterplot <- total_seasonal_cases_LOOCV_plots$rmse_incid_scatterplot
acc_vs_pred_seasonal_incid_rmse_heatmap <- total_seasonal_cases_LOOCV_plots$rmse_incid_heatmap

#- Combined plots 
Combined_seasonal_cases_pred_rmse_plots <- total_seasonal_cases_LOOCV_plots$Combined_cases_plot
Combined_seasonal_incid_pred_rmse_plots <- total_seasonal_cases_LOOCV_plots$Combined_incidence_plot

#--------------- Assess iterative improvement in performance predicting total seasonal cases with each new month of data. 
source("Scripts/00_FUN_Visualise_iterative_improvement_in_monthly_prop_seasonal_case_prediction_performance.R")
Iterative_improvement_in_total_seasonal_case_prediction <- Visualise_iterative_improvement_in_monthly_prop_seasonal_case_prediction_performance(seasonal_case_predictions)
Iterative_improvement_total_seasonal_cases_pred_data <- Iterative_improvement_in_total_seasonal_case_prediction$Iterative_improvement_total_seasonal_cases_pred_data

#- Combined plots 
iterative_improvement_0.75_benchmark_combined_plots <- Iterative_improvement_in_total_seasonal_case_prediction$iterative_improvement_0.75_benchmark_combined_plots

iterative_improvement_0.80_benchmark_combined_plots <- Iterative_improvement_in_total_seasonal_case_prediction$iterative_improvement_0.80_benchmark_combined_plots
iterative_improvement_0.80_benchmark_minus_VCT_combined_plots <- Iterative_improvement_in_total_seasonal_case_prediction$iterative_improvement_0.80_benchmark_minus_VCT_combined_plots

iterative_improvement_0.85_benchmark_combined_plots <- Iterative_improvement_in_total_seasonal_case_prediction$iterative_improvement_0.85_benchmark_combined_plots
iterative_improvement_0.85_benchmark_minus_VCT_combined_plots <- Iterative_improvement_in_total_seasonal_case_prediction$iterative_improvement_0.85_benchmark_minus_VCT_combined_plots

iterative_improvement_0.90_benchmark_combined_plots <- Iterative_improvement_in_total_seasonal_case_prediction$iterative_improvement_0.90_benchmark_combined_plots
iterative_improvement_0.90_benchmark_minus_VCT_combined_plots <- Iterative_improvement_in_total_seasonal_case_prediction$iterative_improvement_0.90_benchmark_minus_VCT_combined_plots

#' St Vincent and the Grenadines skewing results. Much higher total seasonal cases in season 2020/2021, reasons unclear. Massive differences between observed and predicted cases. 
#' RMSE at month with 80, 85 and 90% threshold much smaller than monthly RMSE, hence large ratio value. 
#' Setting benchmark RMSE at 75% of cases performs well - value here is of a similar order of magnitude/ one below to the raw RMSE across countries easing plot interpretability.

#--------------- Assess iterative improvement in performance predicting total seasonal incidence with each new month of data. 
source("Scripts/00_FUN_Visualise_iterative_improvement_in_monthly_prop_seasonal_incid_prediction_performance.R")
Iterative_improvement_in_total_seasonal_incid_prediction <- Visualise_iterative_improvement_in_monthly_prop_seasonal_incid_prediction_performance(seasonal_case_predictions)

#- Data
Iterative_improvement_total_seasonal_incid_pred_data <- Iterative_improvement_in_total_seasonal_incid_prediction$Iterative_improvement_total_seasonal_incid_pred_data

#- Combined plots
iterative_incid_improvement_0.75_benchmark_combined_plots <- Iterative_improvement_in_total_seasonal_incid_prediction$iterative_incid_improvement_0.75_benchmark_combined_plots
iterative_incid_improvement_0.80_benchmark_combined_plots <- Iterative_improvement_in_total_seasonal_incid_prediction$iterative_incid_improvement_0.80_benchmark_combined_plots
iterative_incid_improvement_0.80_benchmark_minus_VCT_combined_plots <- Iterative_improvement_in_total_seasonal_incid_prediction$iterative_incid_improvement_0.80_benchmark_minus_VCT_combined_plots
iterative_incid_improvement_0.85_benchmark_combined_plots <- Iterative_improvement_in_total_seasonal_incid_prediction$iterative_incid_improvement_0.85_benchmark_combined_plots
iterative_incid_improvement_0.85_benchmark_minus_VCT_combined_plots <- Iterative_improvement_in_total_seasonal_incid_prediction$iterative_incid_improvement_0.85_benchmark_minus_VCT_combined_plots
iterative_incid_improvement_0.90_benchmark_combined_plots <- Iterative_improvement_in_total_seasonal_incid_prediction$iterative_incid_improvement_0.90_benchmark_combined_plots
iterative_incid_improvement_0.90_benchmark_minus_VCT_combined_plots <- Iterative_improvement_in_total_seasonal_incid_prediction$iterative_incid_improvement_0.90_benchmark_minus_VCT_combined_plots

#' St Vincent and the Grenadines skewing results. Much higher total seasonal cases in season 2020/2021, reasons unclear. Massive differences between observed and predicted cases. 
#' RMSE at month with 80, 85 and 90% threshold much smaller than monthly RMSE, hence large ratio value. 
#' Setting benchmark RMSE at 75% of cases performs well - value here is of a similar order of magnitude/ one below to the raw RMSE across countries easing plot interpretability.

#--------------- Saving 

dir_to_save <- paste0("Results/03_Visualising_total_seasonal_case_prediction_CV/", Sys.Date())
dir.create(dir_to_save, recursive = TRUE)

#----- Assess performance predicting total seasonal cases

# Data
write_csv(acc_vs_pred_seasonal_cases_rmse_data,
          file = paste0(dir_to_save, "/RMSE_monthly_iterative_seasonal_load_prediction.csv"))

# Combined figures
ggsave(Combined_seasonal_cases_pred_rmse_plots,
       filename = paste0(dir_to_save, "/RMSE_monthly_iterative_seasonal_cases_predictions.png"), dpi = 300, width = 20, height = 16, units = "in")
ggsave(Combined_seasonal_incid_pred_rmse_plots,
       filename = paste0(dir_to_save, "/RMSE_monthly_iterative_seasonal_incid_predictions.png"), dpi = 300, width = 20, height = 16, units = "in")


#----- Assess iterative improvement in performance predicting total seasonal cases with each new month of data. 

# Data
write_csv(Iterative_improvement_total_seasonal_cases_pred_data, 
          file = paste0(dir_to_save, "/RMSE_ratio_iter_improvement_total_season_case_pred.csv"))

# Combined plots - 0.75
ggsave(iterative_improvement_0.75_benchmark_combined_plots,
       filename = paste0(dir_to_save, "/RMSE_monthly_improvement_seasonal_cases_predictions_0.75_benchmark.png"), dpi = 300, width = 20, height = 16, units = "in")

# Combined plots - 0.8
ggsave(iterative_improvement_0.80_benchmark_combined_plots,
       filename = paste0(dir_to_save, "/RMSE_monthly_improvement_seasonal_cases_predictions_0.80_benchmark.png"), dpi = 300, width = 20, height = 16, units = "in")

ggsave(iterative_improvement_0.80_benchmark_minus_VCT_combined_plots, 
       filename = paste0(dir_to_save, "/RMSE_monthly_improvement_seasonal_cases_predictions_0.80_benchmark_minus_VCT.png"), dpi = 300, width = 20, height = 16, units = "in")

# Combined plots - 0.85
ggsave(iterative_improvement_0.85_benchmark_combined_plots,
       filename = paste0(dir_to_save, "/RMSE_monthly_improvement_seasonal_cases_predictions_0.85_benchmark.png"), dpi = 300, width = 20, height = 16, units = "in")

ggsave(iterative_improvement_0.85_benchmark_minus_VCT_combined_plots, 
       filename = paste0(dir_to_save, "/RMSE_monthly_improvement_seasonal_cases_predictions_0.85_benchmark_minus_VCT.png"), dpi = 300, width = 20, height = 16, units = "in")

# Combined plots - 0.90
ggsave(iterative_improvement_0.90_benchmark_combined_plots,
       filename = paste0(dir_to_save, "/RMSE_monthly_improvement_seasonal_cases_predictions_0.90_benchmark.png"), dpi = 300, width = 20, height = 16, units = "in")

ggsave(iterative_improvement_0.90_benchmark_minus_VCT_combined_plots, 
       filename = paste0(dir_to_save, "/RMSE_monthly_improvement_seasonal_cases_predictions_0.90_benchmark_minus_VCT.png"), dpi = 300, width = 20, height = 16, units = "in")


#----- Assess iterative improvement in performance predicting total seasonal incidence with each new month of data. 

# Data
write_csv(Iterative_improvement_total_seasonal_incid_pred_data, 
          file = paste0(dir_to_save, "/RMSE_ratio_iter_improvement_total_season_case_pred.csv"))

# Combined plots - 0.75
ggsave(iterative_incid_improvement_0.75_benchmark_combined_plots,
       filename = paste0(dir_to_save, "/RMSE_monthly_improvement_seasonal_incid_predictions_0.75_benchmark.png"), dpi = 300, width = 20, height = 16, units = "in")

# Combined plots - 0.8
ggsave(iterative_incid_improvement_0.80_benchmark_combined_plots,
       filename = paste0(dir_to_save, "/RMSE_monthly_improvement_seasonal_incid_predictions_0.80_benchmark.png"), dpi = 300, width = 20, height = 16, units = "in")

ggsave(iterative_incid_improvement_0.80_benchmark_minus_VCT_combined_plots, 
       filename = paste0(dir_to_save, "/RMSE_monthly_improvement_seasonal_incid_predictions_0.80_benchmark_minus_VCT.png"), dpi = 300, width = 20, height = 16, units = "in")

# Combined plots - 0.85
ggsave(iterative_incid_improvement_0.85_benchmark_combined_plots,
       filename = paste0(dir_to_save, "/RMSE_monthly_improvement_seasonal_incid_predictions_0.85_benchmark.png"), dpi = 300, width = 20, height = 16, units = "in")

ggsave(iterative_incid_improvement_0.85_benchmark_minus_VCT_combined_plots, 
       filename = paste0(dir_to_save, "/RMSE_monthly_improvement_seasonal_incid_predictions_0.85_benchmark_minus_VCT.png"), dpi = 300, width = 20, height = 16, units = "in")

# Combined plots - 0.90
ggsave(iterative_incid_improvement_0.90_benchmark_combined_plots,
       filename = paste0(dir_to_save, "/RMSE_monthly_improvement_seasonal_incid_predictions_0.90_benchmark.png"), dpi = 300, width = 20, height = 16, units = "in")

ggsave(iterative_incid_improvement_0.90_benchmark_minus_VCT_combined_plots, 
       filename = paste0(dir_to_save, "/RMSE_monthly_improvement_seasonal_incid_predictions_0.90_benchmark_minus_VCT.png"), dpi = 300, width = 20, height = 16, units = "in")

