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

# create run directory 

# define log file path 

# start log process 

#------ Step 2: Open historic data and seasonal average


#------ Step 3: Open data for this S

source("V1/Scripts/data_sourcing/01_this_season_dengue_data.R")

#------ Step 4: Data selection and Backfilling  

source("V1/Scripts/backfilling/02_PAHO_monthly_cases_and_source_selection.R")

#------ Step 5: Predictions 


