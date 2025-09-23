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

# WHO data
# GitHub API endpoint for the folder contents
url <- "https://api.github.com/repos/DengueGlobalObservatory/WHOGlobal-crawler/contents/Downloads"

# Get JSON listing of files
res <- GET(url)
stop_for_status(res)
files <- fromJSON(content(res, "text"))

# Extract only Excel files matching the pattern
xlsx_files <- grep("^dengue-global-data-[0-9]{4}-[0-9]{2}-[0-9]{2}\\.xlsx$", files$name, value = TRUE)

# Find the most recent by date in filename
latest_file <- xlsx_files[which.max(as.Date(gsub("dengue-global-data-|\\.xlsx", "", xlsx_files)))]

# Get the direct download URL for the latest file
download_url <- files[files$name == latest_file, "download_url"]

cat("Opening file:", latest_file, "\n")

# Read Excel directly from the GitHub raw link
temp <- tempfile(fileext = ".xlsx")
download.file(download_url, temp, mode = "wb")
who <- readxl::read_excel(temp)


#---- SEARO data selection, process and save 

##!! need to create API download 
searo_path <- "Data/SEARO/SEARO_National_data_20250620_0818.csv"
searo <- read.csv(searo_path)
searo$country <- searo$Country
searo <- dplyr::select(searo, -c(Country))

rm(files,res)
