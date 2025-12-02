#' Overview
#' ========
#' 
#' Note: This script is dependent on 01_this_season_dengue_data.R

library(ggplot2)
# Functions: 
source("V1/Scripts/backfilling/FUNCTIONS/00_FUN_paho_data_process.R")

if (!exists("log_message")) {
  source("V1/Scripts/utils/logging.R")
  ensure_logger(console = TRUE)
}

if (!exists("record_countries_at_step")) {
  source("V1/Scripts/utils/country_tracking.R")
}

log_message("Running 02_PAHO_monthly_cases_and_source_selection.")

# ----- PAHO backfilling and monthly case calculation 

# apply backfilling and define monthly cumulative cases 

# apply correction
paho_correction <- apply_reporting_correction(df = paho, cases_col = "total_den",
                                             output_col = "total_corrected_cases")
# weekly cumm -> month cum -> monthly
paho_month_cumm  <- compute_monthcumm_cases(df = paho_correction)
# Calculate monthly cases:
paho_monthly <-  PAHO_incid_monthly(paho_month_cumm)
log_message("PAHO monthly rows after correction: " %+% nrow(paho_monthly))

# handle negative values 
## -- room for improvement in future
 

paho_monthly <- paho_monthly %>%
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

paho_add <- paho_monthly %>%
  mutate(
    date = make_date(year = year, month = month_num, day = 1),
    source = "PAHO"
  ) %>%
  dplyr::  rename(
    country = country,
    Year = year,
    Month = month,
    cases = computed_monthly_cases_corr
  ) %>%
  mutate(
    Month = month.abb[match(Month, month.name)],
    iso3 = countrycode(sourcevar = country,
                       origin = "country.name",
                       destination = "iso3c")
  ) %>%
  dplyr::select(country,
                iso3,
         date,
         Year,
         Month,
         source,
         cases,
         missing_reason)

searo_add <- searo %>%
  dplyr::  rename(
    cases = Value
  ) %>%
  mutate(
    # Standardize month to lowercase
    Month = case_when(
      Month == "June" ~ "Jun",
      Month == "July" ~ "Jul",
      TRUE ~ Month),
    # Build the first-of-month date
    date = as.Date(paste(Year, match(Month, month.abb), "01", sep = "-")), 
    source = "SEARO" ,
    iso3 = countrycode(sourcevar = country,
                       origin = "country.name",
                       destination = "iso3c")
    ) %>%
  dplyr:: select(country,
                 iso3,
         date,
         Year,
         Month,
         source,
         cases)


who_add <- who %>%
  mutate(
    Year = year(date), 
    Month = month(date), 
    source = "WHO"
  ) %>%
  mutate(
    Month = format(date, "%b"),
  ) %>%
  dplyr:: select(country,
                 iso3,
                 date,
                 Year,
                 Month,
                 source,
                 cases)

# Step 2: Combine all data
combine <- bind_rows(paho_add, searo_add, who_add)
log_message("Combined country-month rows across sources: " %+% nrow(combine))


# Step 3: Keep the fewest NAs (PAHO/SEARO > WHO)
final_cases <- combine %>%
  group_by(country, iso3, date, Year, Month) %>% 
  # order first by NA status (NA last), then by source preference
  arrange(is.na(cases), source == "WHO") %>% 
  # keep the first row in each group (non-NA, non-WHO prioritized)
  slice(1) %>% 
  ungroup()

# Step 4 : Selected needed time frame and columns
current_year <- as.numeric(format(Sys.Date(), "%Y"))
season_start <-current_year -2

current_data <- final_cases %>%
  filter( Year > season_start) %>%
  dplyr::select(country,
         iso3,
         date,
         Year,
         Month,
         cases,
         source) %>%
  mutate(
    Month = match(Month, month.abb)
  )



# Step 5: ensure that all months are list ( even with NAs) for all year:countrys

current_data <- current_data %>%
  # clean up Year/Month columns
  mutate(
    Year = as.integer(Year),
    Month = as.integer(Month)
  ) %>%
  
  # generate all combinations of country, iso3, and each year present in data
  group_by(country, iso3) %>%
  tidyr::complete(
    Year = full_seq(unique(Year), 1),  # ensures all years in the dataset
    Month = 1:12,                           # ensures months 1–12 for each year
    fill = list(cases = NA)                 # only fill cases with NA
  ) %>%
  ungroup() %>%
  
  # add a proper date column
  mutate(date = as.Date(paste0(Year, "-", Month, "-01"))) %>%
  
  # reorder for clarity
  arrange(country, Year, Month)

log_message("Current data rows after completeness expansion: " %+% nrow(current_data))
log_message("Completed 02_PAHO_monthly_cases_and_source_selection.")

# Record countries in current data (Step 4: Current Data)
if (exists("record_countries_at_step")) {
  tryCatch({
    record_countries_at_step(current_data, "Step_4_Current_Data")
  }, error = function(e) {
    if (exists("log_message")) {
      log_message("Warning: Country tracking failed at Step 4: " %+% conditionMessage(e), level = "WARNING")
    }
  })
}
