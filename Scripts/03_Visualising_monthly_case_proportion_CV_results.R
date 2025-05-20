#' ---
#' title: "03 Visualising monthly case proportion CV results"
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
#' 

library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(Metrics)
library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

monthly_proportion_of_cases_CV <- new.env()
source("Scripts/02_Cross_validation_on_monthly_proportion_of_cases.R", local = monthly_proportion_of_cases_CV)

#--------------- Loading results 
dengue_data_LOOCV_df <- monthly_proportion_of_cases_CV$dengue_data_LOOCV_df
dengue_data_5_year_rolling_CV <- monthly_proportion_of_cases_CV$dengue_data_5_year_rolling_CV

#--------------- Assess performance by country
#' 
#' Questions
#'  Is predicted proportion correlated with actual proportion (LOOCV)? - Yes 
#'    Calculate corr coeff
#'    Visualise as a single column with rows as countries 
#'      Order by performance 
#'      Abs(lat) - distance from equator 
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
#'  
#'      

#---------- Does monthly proportion perform better as a predictor for some months relative to others?
source("Scripts/00_FUN_RMSE_and_MAE_by_country_and_month_fun.R")
LOOCV_performance_by_country_and_month <- RMSE_and_MAE_by_country_and_month_fun(dengue_data_LOOCV_df)
rolling_CV_performance_by_country_and_month <- RMSE_and_MAE_by_country_and_month_fun(dengue_data_5_year_rolling_CV)

#----- Adding in pop weighted centroids 
source("Scripts/00_FUN_identifying_at_risk_pop_weighted_centroids.R")
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


source("Scripts/00_FUN_RMSE_and_MAE_by_country.R")
dengue_data_LOOCV_RMSE_MAE <- RMSE_and_MAE_by_country_fun(dengue_data_LOOCV_df)
dengue_data_5_year_rolling_CV_RMSE_MAE <- RMSE_and_MAE_by_country_fun(dengue_data_5_year_rolling_CV)

#--------------- Plot RMSE and MAE by country 

