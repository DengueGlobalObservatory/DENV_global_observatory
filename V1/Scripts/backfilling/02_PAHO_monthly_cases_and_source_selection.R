#' ---
#' title: "02_PAHO_monthly_cases_and_source_selection"
#' author: "K M Susong"
#' 
#' ---
#'
#' Overview
#' ========
#' 
#' Note: This script is dependent on 01_dengue_data_update.R

# Functions: 
source("V1/Scripts/backfilling/FUNCTIONS/00_FUN_paho_data_process.R")

# ----- PAHO backfilling and monthly case calculation 

# apply backfilling and define monthly cumulative cases 
paho_monthly_cumm <- PAHO_cumm_monthly(paho)

# Calculate monthly cases:
paho_month <- PAHO_incid_monthly(paho_monthly_cumm)


# ----- Selection of data sources for each country

# create a dataframe to determine the total list of countries 
# Create data frames of countries with flags
paho_countries <- paho %>%
  distinct(country) %>%
  mutate(in_paho = TRUE)

who_countries <- who %>%
  distinct(country) %>%
  mutate(in_who = TRUE)

searo_countries <- searo %>%
  distinct(country) %>%
  mutate(in_searo = TRUE)

# Combine all and fill NAs with FALSE
all_countries <- full_join(paho_countries, who_countries, by = "country") %>%
  full_join(searo_countries, by = "country") %>%
  mutate(
    in_paho = replace_na(in_paho, FALSE),
    in_who = replace_na(in_who, FALSE),
    in_searo = replace_na(in_searo, FALSE)
  )

# make combine df

# Step 1: correct the column names in PAHO and SEARO to match WHO 
paho_add <- paho_month %>%
  dplyr::  rename(
    country = country,
    date = date,
    year = year,
    month = month,
    cases = computed_monthly_cases
  ) %>%
  mutate(
    month = month.abb[match(month, month.name)]
  ) %>%
  select(country,
         date,
         year,
         month,
         cases)

searo_add <- searo %>%
  dplyr::  rename(
    country = country,
    year = Year,
    month = Month,
    cases = Value
  ) %>%
  mutate(
    # Standardize month to lowercase
    month = case_when(
      month == "June" ~ "Jun",
      month == "July" ~ "Jul",
      TRUE ~ month),
    # Build the first-of-month date
    date = as.Date(paste(year, match(month, month.abb), "01", sep = "-"))
    ) %>%
  select(country,
         date,
         year,
         month,
         cases)
# Step 2: Combine PAHO and SEARO data
paho_searo <- bind_rows(paho_add, searo_add)

# Step 3: Identify countries already in paho_searo
existing_countries <- paho_searo %>%
  distinct(country)

# Step 4: Filter WHO for countries not in paho_searo
who_unique <- who %>%
  anti_join(existing_countries, by = "country")

# Step 5: Final combined dataset
final_data <- bind_rows(paho_searo, who_unique)

season_data <- final_data %>%
  filter( year > 2023) %>%
  select(country,
         date,
         year,
         month,
         cases)


# ------ Save output df 
dir.create(paste0("V1/Output/",format(Sys.Date(), "%Y_%m_%d")))
write_csv(season_data, file = paste0("V1/Output/", format(Sys.Date(), "%Y_%m_%d"),"/backfill_nowcast_output.csv"))  
