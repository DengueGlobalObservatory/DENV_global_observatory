#' ---
#' title: "03_Visualising_monthly_case_proportion_CV_results"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Visualise results from LOOCV and rolling CV on monthly proportion of cases
#' 
#' Timeline:
#' =========
#' 21-05-2025: Prepared script.
#' 19-06-2025: Corrected sourced function filepaths.

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

monthly_proportion_of_cases_CV <- new.env()
source("Scripts/02_Cross_validation_on_monthly_proportion_of_cases.R", local = monthly_proportion_of_cases_CV)

#--------------- Loading results 
dengue_data_LOOCV_df <- monthly_proportion_of_cases_CV$dengue_data_LOOCV_df
dengue_data_5_year_rolling_CV <- monthly_proportion_of_cases_CV$dengue_data_5_year_rolling_CV

#--------------- Are predicted cases (LOOCV) correlated with actual cases?

#----- Performance by country (correlation) - proportion (same as cases as cor)
source("Scripts/Functions/00_FUN_Visualise_monthly_pred_cases_vs_acc_cases_cor_results.R")
pred_prop_vs_acc_prop_cor_res_plots <- Visualise_monthly_pred_cases_vs_acc_cases_cor_results_fun(dengue_data_LOOCV_df)
pred_cases_vs_acc_cases_plot <- pred_prop_vs_acc_prop_cor_res_plots$Scatterplot
pred_prop_vs_acc_prop_cor_df <- pred_prop_vs_acc_prop_cor_res_plots$DataFrame
pred_prop_vs_acc_prop_cor_heatmap <- pred_prop_vs_acc_prop_cor_res_plots$Cor_heatmap
pred_prop_vs_acc_prop_cor_lat_ordered_df <- pred_prop_vs_acc_prop_cor_res_plots$Lat_ordered_cor_df
pred_prop_vs_acc_prop_cor_lat_ordered_heatmap <- pred_prop_vs_acc_prop_cor_res_plots$Lat_ordered_cor_heatmap

cor(pred_prop_vs_acc_prop_cor_lat_ordered_df$dist_from_equator, pred_prop_vs_acc_prop_cor_lat_ordered_df$Correlation)
#' Little association between distance from equator and performance of monthly proportions, cor = 0.197

#' Scatterplot
#'    Scatterplot shows decent association between actual and predicted cases. 
#'    Some countries have weak association, e.g. Antigua, Aruba, Bahamas, Bermuda, British Virgin islands, Cayman islands, French Guiana, Haiti, Jamaica, Montserrat, St Barthelemy, 
#'    St Kitts and Nevis, St Lucia, St Vincent and Grenadines, Suriname, Turks and Caicos Islands, Yemen. 
#' Pred prop vs acc prop correlation heatmap 
#'    Some countries show negative correlation, e.g. Bahamas, St Barthelemy, St Kitts and Nevis, St Vicent, Brunei, British Virgin Islands, Haiti, Aruba, Yemen and French Guiana.
#' Pred prop vs acc prop correlation heatmap - lat ordered 
#'    No clear latitude dependency 

#--------------- Performance by country (RMSE) - cases
source("Scripts/Functions/00_FUN_Visualise_monthly_pred_cases_vs_acc_cases_rmse_results.R")
pred_vs_acc_cases_rmse_res_plots <- Visualise_monthly_pred_cases_vs_acc_cases_rmse_results_fun(dengue_data_LOOCV_df)
pred_vs_acc_cases_rmse_df <- pred_vs_acc_cases_rmse_res_plots$Cases_RMSE_df
pred_vs_acc_cases_rmse_heatmap <- pred_vs_acc_cases_rmse_res_plots$Cases_RMSE_heatmap
pred_vs_acc_cases_rmse_heatmap_without_brazil <- pred_vs_acc_cases_rmse_res_plots$Cases_RMSE_heatmap_minus_brazil
pred_vs_acc_cases_rmse_lat_ordered_df <- pred_vs_acc_cases_rmse_res_plots$Lat_ordered_cases_RMSE_df
pred_vs_acc_cases_rmse_lat_ordered_heatmap <- pred_vs_acc_cases_rmse_res_plots$Lat_ordered_cases_RMSE_heatmap
pred_vs_acc_cases_rmse_lat_ordered_heatmap_without_brazil <- pred_vs_acc_cases_rmse_res_plots$Lat_ordered_cases_RMSE_heatmap_minus_brazil

#' pred_vs_acc_cases_rmse_heatmap
#'    Brazil RMSE >> other countries - skews colour palette 
#' pred_vs_acc_cases_rmse_heatmap_without_brazil
#'    Locations with smaller population have smaller RMSE and vice versa. As expected as RMSE is on cases scale rather than incidence scale. 
#' pred_vs_acc_cases_rmse_lat_ordered_df
#'  No clear latitude dependency. Brazil high RMSE masks some results. 
#' pred_vs_acc_cases_rmse_lat_ordered_heatmap_without_brazil
#'  No strong dependency. 

cor(pred_vs_acc_cases_rmse_df$RMSE, pred_vs_acc_cases_rmse_df$dist_from_equator)
#' -0.058, nearly zero association between RMSE and distance from equator. 

#--------------- Performance by country (RMSE) - incidence

#----- Calculate incidence 
# https://human-settlement.emergency.copernicus.eu/download.php?ds=pop
Pop_raster_1990 <- rast("Data/GHS_POP_E1990_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E1990_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_1995 <- rast("Data/GHS_POP_E1995_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E1995_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2000 <- rast("Data/GHS_POP_E2000_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2000_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2005 <- rast("Data/GHS_POP_E2005_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2005_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2010 <- rast("Data/GHS_POP_E2010_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2010_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2015 <- rast("Data/GHS_POP_E2015_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2015_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2020 <- rast("Data/GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0.tif")
Pop_raster_2025 <- rast("Data/GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0.tif")
source("Scripts/00_FUN_Calculate_incidence.R")
dengue_data_LOOCV_incid_df <- calculate_incidence(dengue_data_LOOCV_df, 
                                                  Pop_raster_1990, Pop_raster_1995, Pop_raster_2000, Pop_raster_2005, 
                                                  Pop_raster_2010, Pop_raster_2015, Pop_raster_2020, Pop_raster_2025)

#--- Incidence RMSE 
source("Scripts/Functions/00_FUN_Visualise_monthly_pred_vs_acc_incidence_rmse_results.R")
pred_vs_acc_incid_rmse_res_plots <- Visualise_monthly_pred_vs_acc_incidence_rmse_results_fun(dengue_data_LOOCV_incid_df)
pred_vs_acc_incid_RMSE_df <- pred_vs_acc_incid_rmse_res_plots$Incidence_RMSE_df
pred_vs_acc_incid_RMSE_heatmap <- pred_vs_acc_incid_rmse_res_plots$Incidence_RMSE_heatmap
pred_vs_acc_incid_RMSE_heatmap_minus_St_barthelemy <- pred_vs_acc_incid_rmse_res_plots$Incidence_RMSE_heatmap_minus_St_Barthelemy
pred_vs_acc_incid_rmse_lat_ordered_df <- pred_vs_acc_incid_rmse_res_plots$Lat_ordered_incidence_RMSE_df
pred_vs_acc_incid_rmse_lat_ordered_heatmap <- pred_vs_acc_incid_rmse_res_plots$Lat_ordered_ncidence_RMSE_heatmap
pred_vs_acc_incid_rmse_lat_ordered_heatmap_minus_St_Barthelemy <- pred_vs_acc_incid_rmse_res_plots$Lat_ordered_ncidence_RMSE_heatmap_minus_St_Barthelemy

#' pred_vs_acc_incid_RMSE_heatmap
#'    St Barthelemy has larger RMSE than other locations, clouds scale. 
#' pred_vs_acc_incid_RMSE_heatmap_minus_St_barthelemy
#'    Small island nations appear to have larger RMSE. 
#' pred_vs_acc_incid_rmse_lat_ordered_heatmap 
#'    No clear latitude dependency in performance
#' pred_vs_acc_incid_rmse_lat_ordered_heatmap_minus_St_Barthelemy
#'    No clear latitude dependency in performance

#--------------- Does LOOCV outperform rolling CV?

source("Scripts/Functions/00_FUN_comparing_LOOCV_vs_rolling_CV.R")
comparing_LOOCV_vs_rolling_CV <- comparing_LOOCV_vs_rolling_CV(dengue_data_LOOCV_df, dengue_data_5_year_rolling_CV)
comparing_LOOCV_vs_rolling_CV_RMSE <- comparing_LOOCV_vs_rolling_CV$RMSE_plot
comparing_LOOCV_vs_rolling_CV_cor <- comparing_LOOCV_vs_rolling_CV$Cor_plot

#' Rolling CV RMSE is higher for most countries, except for Ecuador and El Salvador. 
#' LOOCV cor is higher for most countries, barring Dominican Republic, Ecuador and Thailand.


#--------------- Is correlation between predicted proportion (LOOCV) and actual proportion correlated with population?

#----- Extract population by country
source("Scripts/Functions/00_FUN_population_by_country.R")
pop_by_country <- population_by_country(dengue_data_LOOCV_df$iso3, Pop_raster_2025)

pred_prop_vs_acc_prop_cor_pop <- pred_prop_vs_acc_prop_cor_df %>% 
  full_join(., pop_by_country, by = "iso3") 
  
acc_pred_cor_vs_pop_cor <- cor(pred_prop_vs_acc_prop_cor_pop$Correlation, pred_prop_vs_acc_prop_cor_pop$Population)
#' Correlation = 0.3237908
#' Low correlation between performance of monthly proportion of cases and population 







#' Questions
#'       
#'  How does proportion perform in the desired use case?
#'    Predicting annual cases at time point T by country (correlation coefficient)
#'      Table (rows as countries, cols as months - colour by metric)
#'      Calculate RMSE then divide by population
#'    Can we predict monthly cases given time point T
#'      Values iteratively update - consider spread as a function of time/ months observed.... 
#'    
#' 
#' Assess performance by country. Validation metrics: RMSE and MAE 
#' Questions: 
#'    Does monthly proportion perform better as a predictor for some months relative to others?
#'    Does monthly proportion perform better as a predictor for some countries relative to others?
#'    Is there a trend in performance of monthly proportion as a predictor, i.e. is seasonality becoming more or less consistent?
#'      Assess trend in RMSE/ MAE over time and whether rolling CV outperforms LOOCV.
#'    Does performance of monthly proportion change as a function of number of cases? 
#'    Plot predicted vs actuals
#' Actions
#'  RMSE by year by country (summed across months)
#'  Correlation as well to compare by country (can do incidence later on...)
      
















#---------- Does monthly proportion perform better as a predictor for some months relative to others?
source("Scripts/Functions/00_FUN_RMSE_and_MAE_by_country_and_month_fun.R")
LOOCV_performance_by_country_and_month <- RMSE_and_MAE_by_country_and_month_fun(dengue_data_LOOCV_df)
rolling_CV_performance_by_country_and_month <- RMSE_and_MAE_by_country_and_month_fun(dengue_data_5_year_rolling_CV)

#----- Adding in pop weighted centroids 
source("Scripts/Functions/00_FUN_identifying_at_risk_pop_weighted_centroids.R")
DEN_occurrence_raster <- rast("Data/Arbo_riskmaps_public-main/outputs/Rasters/DCZ_binmap_mean.tif")
Pop_raster_2025 <- rast("Data/GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0.tif") 
#' Downloaded from https://human-settlement.emergency.copernicus.eu/download.php?ds=pop

at_risk_pop_centroids <- identifying_at_risk_pop_weighted_centroids_fun(Pop_raster_2025, DEN_occurrence_raster, unique(LOOCV_performance_by_country_and_month$iso3))


LOOCV_performance_by_country_and_month_RMSE_plot <- ggplot(LOOCV_performance_by_country_and_month, mapping = aes(x = Month, y = RMSE)) +
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks = seq(from = 0, to = 12, by = 2)) + 
  facet_wrap(~Country, scales = "free_y")
  

Pop_raster_2025 <- rast("Data/GHS_raster/GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2025_GLOBE_R2023A_4326_30ss_V1_0.tif")

ggplot(dengue_data_LOOCV_df) +
  geom_point(mapping = aes(x = Predicted_cases, y = Cases_clean)) +
  facet_wrap(~ Country, scales = "free")

ggplot(dengue_data_5_year_rolling_CV) +
  geom_point(mapping = aes(x = Predicted_cases, y = Cases_clean)) +
  facet_wrap(~ Country, scales = "free")


source("Scripts/Functions/00_FUN_RMSE_and_MAE_by_country.R")
dengue_data_LOOCV_RMSE_MAE <- RMSE_and_MAE_by_country_fun(dengue_data_LOOCV_df)
dengue_data_5_year_rolling_CV_RMSE_MAE <- RMSE_and_MAE_by_country_fun(dengue_data_5_year_rolling_CV)

#--------------- Plot RMSE and MAE by country 

