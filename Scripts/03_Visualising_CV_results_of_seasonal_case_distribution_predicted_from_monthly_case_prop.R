#' ---
#' title: "03_Visualising_CV_results_of_seasonal_case_distribution_predicted_from_monthly_case_prop"
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

#--------------- Assess performance predicting monthly cases iteratively 

#----- Calculate incidence
source("Scripts/00_FUN_Calculate_incidence_monthly_case_prediction_desired_use_case.R")
iterative_monthly_case_predictions_incidence <- Calculate_incidence_monthly_case_prediction_desired_use_case(iterative_monthly_case_predictions, 
                                                                                                             Pop_raster_1990, Pop_raster_1995, Pop_raster_2000, Pop_raster_2005, 
                                
                                                                                                             
#---------- Convert to incidence  
# https://human-settlement.emergency.copernicus.eu/download.php?ds=pop
Pop_raster_1990 <- rast("Data/GHS_POP_E1990_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E1990_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_1995 <- rast("Data/GHS_POP_E1995_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E1995_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2000 <- rast("Data/GHS_POP_E2000_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2000_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2005 <- rast("Data/GHS_POP_E2005_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2005_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2010 <- rast("Data/GHS_POP_E2010_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2010_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2015 <- rast("Data/GHS_POP_E2015_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2015_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2020 <- rast("Data/GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2025 <- rast("Data/GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0.tif")                                                                             Pop_raster_2010, Pop_raster_2015, Pop_raster_2020, Pop_raster_2025)

#----- Plot results 
source("Scripts/00_FUN_Visualise_monthly_prop_iterative_monthly_incid_prediction_performance_desired_use_case.R")
iterative_monthly_cases_LOOCV_plots <- Visualise_monthly_prop_iterative_monthly_incid_prediction_performance_desired_use_case_fun(iterative_monthly_case_predictions_incidence)
acc_vs_pred_monthly_incid_cor_heatmap <- iterative_monthly_cases_LOOCV_plots$cor_heatmap
acc_vs_pred_monthly_cases_rmse_heatmap <- iterative_monthly_cases_LOOCV_plots$rmse_cases_heatmap
acc_vs_pred_monthly_cases_rmse_heatmap_minus_brazil <- iterative_monthly_cases_LOOCV_plots$rmse_cases_heatmap_minus_brazil
acc_vs_pred_monthly_incid_rmse_heatmap <- iterative_monthly_cases_LOOCV_plots$rmse_incid_heatmap
acc_vs_pred_monthly_incid_rmse_heatmap_minus_st_barthelemy <- iterative_monthly_cases_LOOCV_plots$rmse_incid_heatmap_minus_st_barthelemy

#--------------- Assess performance predicting monthly cases iteratively 


#--------------- Saving 

dir_to_save <- paste0("Results/03_Visualising_CV_results_of_seasonal_case_distribution_predicted_from_monthly_case_prop/", Sys.Date())
dir.create(dir_to_save)

ggsave(acc_vs_pred_monthly_cases_rmse_heatmap,
       filename = paste0(dir_to_save, "/RMSE_of_observed_vs_monthly_iterative_case_predictions_heatmap.png"))

ggsave(acc_vs_pred_monthly_cases_rmse_heatmap_minus_brazil,
       filename = paste0(dir_to_save, "/RMSE_of_observed_vs_monthly_iterative_case_predictions_heatmap_minus_Brazil.png"))

ggsave(acc_vs_pred_monthly_incid_rmse_heatmap,
       filename = paste0(dir_to_save, "/RMSE_of_observed_vs_monthly_iterative_incid_predictions_heatmap.png"))

ggsave(acc_vs_pred_monthly_incid_rmse_heatmap_minus_st_barthelemy,
       filename = paste0(dir_to_save, "/RMSE_of_observed_vs_monthly_iterative_incid_predictions_heatmap_minus_STM.png"))