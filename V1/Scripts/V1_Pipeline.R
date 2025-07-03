#' ---
#' title: "V1_Pipeline"
#' author: "K M Susong"
#' 
#' ---
#'
#' Overview
#' ========
#' 
#' 


#------ Step 1: start log


#------ Step 2: Open data

source("V1/Scripts/data_sourcing/01_open_dengue_data.R")

#------ Step 3: Backfilling and data selection 

source("V1/Scripts/backfilling/02_PAHO_monthly_cases_and_source_selection.R")

#------ Step 4: Predictions 


