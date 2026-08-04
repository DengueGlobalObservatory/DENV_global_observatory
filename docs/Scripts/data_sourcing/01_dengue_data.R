#' ---
#' title: "01_dengue_data_update"
#' author: "K M Susong"
#' 
#' ---
#'
#' Overview
#' ========
#' 
#'  • There are 3 sources what can leverage for current/recent data ( collected via Crawlers)
#'          -  WHO dashboard: https://worldhealthorg.shinyapps.io/dengue_global/
#'          
#'          - SEARO dashboard: https://worldhealthorg.shinyapps.io/searo-dengue-dashboard/
#'          
#'          - PAHO dashboard: https://www.paho.org/en/arbo-portal/dengue-data-and-analysis/dengue-analysis-country
#'          
#'  • and 1 source to gain larger historical context 
#'  
#'          - OpenDengue: https://opendengue.org/data.html

library(httr)
library(jsonlite)
library(dplyr)
library(readxl)
library(opendenguedata)
library(ISOweek)

if (!exists("log_message")) {
  source("Scripts/utils/logging.R")
  ensure_logger(console = TRUE)
}

#---- Functions
source("Scripts/data_sourcing/FUNCTIONS/00_FUN_realtime_data_download.R")

#---- PAHO data selection, process, and save 

paho <- compile_PAHO_github()

# add reporting delay
# Delay is weeks between download Epiweek and observation Epiweek 

paho <- paho %>%
  mutate(
    iso_week = paste0(year, "-W", str_pad(EW, 2, pad = "0")),
    onset_date = ISOweek2date(paste0(iso_week, "-1")),
    year = year(onset_date),
    d = round(as.numeric(difftime(ymd(ext_date), onset_date, units = "weeks")), 0)
  )
# add unit
paho$d_unit <- "week"


#---- WHO -API
log_message("Start WHO - API download...")

# GitHub API endpoint for the folder contents
url <- "https://api.github.com/repos/DengueGlobalObservatory/WHOGlobal-crawler/contents/Downloads"

# Get JSON listing of files
res <- github_api_request(url)  # Changed from GET(url)
files <- fromJSON(content(res, "text"))

# Extract only Excel files matching the pattern
xlsx_files <- grep("^dengue-global-data-[0-9]{4}-[0-9]{2}-[0-9]{2}\\.xlsx$", files$name, value = TRUE)

# Find the most recent by date in filename
latest_file <- xlsx_files[which.max(as.Date(gsub("dengue-global-data-|\\.xlsx", "", xlsx_files)))]

# Get the direct download URL for the latest file
download_url <- files[files$name == latest_file, "download_url"]

log_message(paste0("WHO - Opening file:", latest_file, "\n"))

# Read Excel directly from the GitHub raw link
temp <- tempfile(fileext = ".xlsx")
download.file(download_url, temp, mode = "wb")
who <- readxl::read_excel(temp)

# add reporting delay
who_download_date <- as.Date(gsub("dengue-global-data-|\\.xlsx", "", latest_file))
who_download_year <- as.integer(format(who_download_date, "%Y"))
who_download_month <- as.integer(format(who_download_date, "%m"))

who_obs_year <- as.integer(format(who$date, "%Y"))
who_obs_month <- as.integer(format(who$date, "%m"))

# Delay is months between download month and observation month.
who$d <- (who_download_year - who_obs_year) * 12 + (who_download_month - who_obs_month)
who$d_unit <- "month"



#---- SEARO - API
log_message("Start SEARO - API download...")


# GitHub API endpoint for the folder contents
url <- "https://api.github.com/repos/DengueGlobalObservatory/SEARO-crawler/contents/output"


# Get JSON listing of files
res <- github_api_request(url)  # Changed from GET(url)
files <- fromJSON(content(res, "text"))

# Extract only Excel files matching the pattern
searo_files <- grep("SEARO_National_data_[0-9]{8}_[0-9]{4}\\.csv", files$name, value = TRUE)

# Find the most recent by date in filename
# Parse datetime from filenames
timestamps <- gsub("SEARO_National_data_|\\.csv", "", searo_files)

# Convert to POSIXct (YYYYMMDD_HHMM)
dt <- as.POSIXct(timestamps, format = "%Y%m%d_%H%M", tz = "UTC")

# Pick most recent
latest_file <- searo_files[which.max(dt)]

# Get the direct download URL for the latest file
download_url <- files[files$name == latest_file, "download_url"]

log_message(paste0("SEARO - Opening file:", latest_file, "\n"))

# Read Excel directly from the GitHub raw link
temp <- tempfile(fileext = ".csv")
download.file(download_url, temp, mode = "wb")
searo <- read.csv(temp)
# format
searo$country <- searo$Country
searo <- dplyr::select(searo, -c(Country))

#add reporting delay
download_dt <- dt[which.max(dt)]
download_year <- as.integer(format(download_dt, "%Y"))
download_month <- as.integer(format(download_dt, "%m"))

obs_month <- match(tolower(substr(searo$Month, 1, 3)), tolower(month.abb))
obs_year <- as.integer(searo$Year)

# Delay is months between download month and observation month.
searo$d <- (download_year - obs_year) * 12 + (download_month - obs_month)
searo$d_unit <- "month"



#---- OpenDengue - historic data 
log_message("Open Open Dengue ")

# opens in current version from the github
OD_national <-  read.csv("Assets/Stable/OD_maps/pred_downscale_with_ci_V3.csv")

log_message("Open Dengue complete")

log_message(sprintf(
  "Loaded datasets — OD_national: %s rows, PAHO: %s rows, SEARO: %s rows, WHO: %s rows",
  nrow(OD_national), nrow(paho), nrow(searo), nrow(who)
))

log_message("Completed 01_dengue_data.R")

# remove extra envi objects 
suppressWarnings(
suppressMessages(
rm( "dir_input","download_url" ,"dt" ,"files" ,"latest_file",
    "res","searo_files","temp","timestamps", "url","xlsx_files",
    "who_download_date","who_download_month","who_download_year",
    "who_obs_month","who_obs_year")))  
