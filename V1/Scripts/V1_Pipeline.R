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

# 2.1 --- does the historic data require updating?
### for yes: it has been one year since last update 

# Define directory
historic_dir <- "V1/Output/historic_data/"

# 1. Get list of historic data files
files <- list.files(historic_dir, full.names = FALSE)

# 2. Extract dates from filenames
file_dates <- files %>%
  basename() %>%
  sub("^National_clean_data_", "", .) %>%  # remove prefix
  sub("\\.csv$", "", .) %>%               # remove suffix
  ymd()                                   # convert to Date

# 3. Find the most recent file
latest_file <- files[which.max(file_dates)]
latest_date <- max(file_dates, na.rm = TRUE)

# 4. Check if the file is older than one year
needs_update <- today() - latest_date > years(1)

## if yes, run update of WHO and OPENdengue, open, combine to single data source, recalculate average season 

if (needs_update) {
  message("Historic data is older than 1 year. Updating...")
  # open and combine seasonal data
  source("V1/Scripts/data_sourcing/01_historic_national_data.R")
  # calculate average season 
  source()
}

## open most recent existing average season file 

avg_season <- 

#------ Step 3: Open data for this season

source("V1/Scripts/data_sourcing/01_this_season_dengue_data.R")

#------ Step 4: Data selection and Backfilling  

source("V1/Scripts/backfilling/02_PAHO_monthly_cases_and_source_selection.R")

#------ Step 5: Predictions 

#needs work 
source("V1/Scripts/forecasting/03_two_month_ahead_forecasting.R")

#------ Step 6: Visualise 

