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

library(ggplot2)
# Functions: 
source("V1/Scripts/backfilling/FUNCTIONS/00_FUN_paho_data_process.R")

# ----- PAHO backfilling and monthly case calculation 

# apply backfilling and define monthly cumulative cases 

# apply correction
paho_correction <- apply_reporting_correction(df = paho, cases_col = "total_den",
                                             output_col = "total_corrected_cases")
# weekly cumm -> month cum -> monthly
paho_month_cumm  <- compute_monthcumm_cases(df = paho_correction)
# Calculate monthly cases:
monthly_paho <-  PAHO_incid_monthly(paho_month_cumm)

# handle negative values 
## -- room for improvement in future
monthly_paho <- monthly_paho %>%
  mutate( 
    missing_reason = case_when(computed_monthly_cases_corr < 0 ~ "replaced_with_uncor", TRUE ~ missing_reason), 
    computed_monthly_cases_corr = case_when(computed_monthly_cases_corr < 0 ~ computed_monthly_cases, TRUE ~ computed_monthly_cases_corr)
    ) %>%
  mutate(
    missing_reason = case_when(computed_monthly_cases_corr < 0 ~ "negative", TRUE ~ missing_reason),
    computed_monthly_cases_corr = case_when(computed_monthly_cases_corr < 1 ~ NA, TRUE ~ computed_monthly_cases_corr)
  )



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
paho_add <- monthly_paho %>%
  mutate(
    date = make_date(year = year, month = month_num, day = 1),
    source = "PAHO"
  ) %>%
  dplyr::  rename(
    country = country,
    year = year,
    month = month,
    cases = computed_monthly_cases_corr
  ) %>%
  mutate(
    month = month.abb[match(month, month.name)]
  ) %>%
  select(country,
         date,
         year,
         month,
         source,
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
    date = as.Date(paste(year, match(month, month.abb), "01", sep = "-")), 
    source = "SEARO"
    ) %>%
  select(country,
         date,
         year,
         month,
         source,
         cases)


# Step 2: Combine PAHO and SEARO data
paho_searo <- bind_rows(paho_add, searo_add)

# Step 3: Identify countries already in paho_searo
existing_countries <- paho_searo %>%
  distinct(country)

# Step 4: Filter WHO for countries not in paho_searo
who_unique <- who %>%
  mutate(
    source = "WHO"
  ) %>%
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
