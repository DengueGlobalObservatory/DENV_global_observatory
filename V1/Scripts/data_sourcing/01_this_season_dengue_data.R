#' ---
#' title: "01_dengue_data_update"
#' author: "K M Susong"
#' 
#' ---
#'
#' Overview
#' ========
#' 
#'  • There are 3 sources what can leverage for current/recent data
#'          -  WHO dashboard: https://worldhealthorg.shinyapps.io/dengue_global/
#'          
#'          - SEARO dashboard: https://worldhealthorg.shinyapps.io/searo-dengue-dashboard/
#'          
#'          - PAHO dashboard: https://www.paho.org/en/arbo-portal/dengue-data-and-analysis/dengue-analysis-country
#'          
#'  processing and updating these data files is the first step in running the dashboard script 
#'  
#'  

library(readxl)

#---- Functions
source("V1/Scripts/data_sourcing/FUNCTIONS/00_FUN_realtime_data_download.R")
#---- Universal varaibles 

today<- Sys.Date()
today_numeric <- format(today, "%Y%m%d")


current_season <- c(2024,2025)

#---- PAHO data selection, process, and save 
## !! this need to be replaced with an API download from the PAHOCrawler githb directly

# Define paths
inital <- "~/Dropbox/DMMG/PAHO_crawler/data/"
final <- "~/Dropbox/DMMG/DENV_dashboard/DENV_global_observatory/Data/PAHO"
# define the most recent download
# PAHO is downloaded daily so todays date can be used to make the path to the most recent files
# paho_files <- file.path(paste0("DL_",today_numeric))
# 
# 
# # Save single formated .csv of PAHO data
# compile_PAHO(inital, paho_files, today_numeric, final)

paho_path <- "Data/PAHO/PAHO_week_20250618.csv"
paho <- read.csv(paho_path)

#---- WHO data selection, process, and save 

##!! need to create API download 
who_path <-"Data/WHO/dengue-global-data-2025-06-24.xlsx"
who <- read_excel(who_path)


#---- SEARO data selection, process and save 

##!! need to create API download 
searo_path <- "Data/SEARO/SEARO_National_data_20250620_0818.csv"
searo <- read.csv(searo_path)
searo$country <- searo$Country
searo <- dplyr::select(searo, -c(Country))
