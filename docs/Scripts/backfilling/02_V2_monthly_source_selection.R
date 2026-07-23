#' Temporal and Spatial Harminisation Version 2
#' 
#' Overview
#' ========
#' 
#' Note: This script is dependent on 01_this_season_dengue_data.R
#' 
#' Note: Version 2 - includes PAHO and WHO 

library(countrycode)

# Functions: 
source("Scripts/backfilling/FUNCTIONS/00_FUN_paho_data_process.R")

if (!exists("log_message")) {
  source("Scripts/utils/logging.R")
  ensure_logger(console = TRUE)
}

if (!exists("record_countries_at_step")) {
  source("Scripts/utils/country_tracking.R")
}

log_message("Running 02_V2_monthly_source_selection.")

#------ PAHO and WHO Case correction (backfilling) ------#

source("Scripts/backfilling/02_V2_backfilling.R")

#----- PAHO monthly case calculation ------#

# move to monthly cases 
# weekly cumm -> month cum
paho_month_cumm  <- compute_monthcumm_cases(df = paho_correction)

#  month cum -> monthly
paho_monthly <-  PAHO_incid_monthly(paho_month_cumm)
log_message("PAHO monthly rows after correction: " %+% nrow(paho_monthly))

# handle negative values 
# applied = corrected-or-raw (never NA for lack of RF); negatives from cumulative
# diffs fall back to the raw monthly value, then sub-1 months are set NA.
paho_monthly <- paho_monthly %>%
  mutate( 
    missing_reason = case_when(computed_monthly_cases_applied < 0 ~ "replaced_with_uncor", TRUE ~ missing_reason), 
    computed_monthly_cases_applied = case_when(computed_monthly_cases_applied < 0 ~ computed_monthly_cases, TRUE ~ computed_monthly_cases_applied)
    ) %>%
  mutate(
    missing_reason = case_when(computed_monthly_cases_applied < 0 ~ "negative", TRUE ~ missing_reason),
    computed_monthly_cases_applied = case_when(computed_monthly_cases_applied < 1 ~ NA, TRUE ~ computed_monthly_cases_applied)
  )

# Log countries without data for current year
if (exists("log_message")) {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # Get all unique countries in PAHO data
  all_paho_countries <- paho_monthly %>%
    dplyr::select(country) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      iso3 = countrycode::countrycode(country, "country.name", "iso3c")
    ) %>%
    dplyr::filter(!is.na(iso3))
  
  # Get countries with data for current year
  countries_with_current_year <- paho_monthly %>%
    dplyr::filter(year == current_year) %>%
    dplyr::select(country) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      iso3 = countrycode::countrycode(country, "country.name", "iso3c")
    ) %>%
    dplyr::filter(!is.na(iso3))
  
  # Identify countries without current year data
  countries_without_current_year <- all_paho_countries %>%
    dplyr::filter(!iso3 %in% countries_with_current_year$iso3)
  
  log_message("PAHO countries with data for " %+% current_year %+% ": " %+% 
                nrow(countries_with_current_year) %+% "/" %+% nrow(all_paho_countries))
  
  if (nrow(countries_without_current_year) > 0) {
    log_message("Warning: " %+% nrow(countries_without_current_year) %+% 
                  " PAHO countries without data for current year (" %+% current_year %+% "): " %+% 
                  paste(countries_without_current_year$iso3, collapse = ", "), level = "WARNING")
  } else {
    log_message("All PAHO countries have data for current year (" %+% current_year %+% ")")
  }}

# Record countries after PAHO negative value handling (Step 4b: PAHO After Negative Handling)
if (exists("record_countries_at_step")) {
  tryCatch({
    paho_monthly_countries <- paho_monthly %>%
      dplyr::select(country) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        iso3 = countrycode::countrycode(country, "country.name", "iso3c")
      ) %>%
      dplyr::select(country, iso3) %>%
      dplyr::filter(!is.na(iso3))
    
    record_countries_at_step(paho_monthly_countries, "Step_4b_PAHO_After_Negative_Handling")
    log_message("Recorded " %+% nrow(paho_monthly_countries) %+% " countries after PAHO negative value handling")
  }, error = function(e) {
    if (exists("log_message")) {
      log_message("Warning: Country tracking failed at Step 4b PAHO Negative Handling: " %+% conditionMessage(e), level = "WARNING")
    }
  })
}


#----- Selection of data sources for each country -----#

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

# == PAHO == #
paho_add <- paho_monthly %>%
  mutate(
    date = make_date(year = year, month = month_num, day = 1),
    source = "PAHO"
  ) %>%
  dplyr::  rename(
    country = country,
    Year = year,
    Month = month,
  # maintain raw, corrected, and (applied) cases in a way that matches for WHO and SEARO
    cases = computed_monthly_cases_applied,
    raw_cases = computed_monthly_cases, 
    corrected_cases = computed_monthly_cases_corr, 
  ) %>%
  mutate(
    Month = month.abb[match(Month, month.name)],
    iso3 = countrycode(sourcevar = country,
                       origin = "country.name",
                       destination = "iso3c"),
    iso3 = ifelse(country == "Saint Martin", "MAF", iso3)
  ) %>%
  dplyr::select(country,
                iso3,
         date,
         Year,
         Month,
         source,
         cases,
         raw_cases,
         corrected_cases,
         d, rf,
         correction_applied,
         correction_reason,
         missing_reason)

# == SEARO == #

# as of June 2026 SEARO does not have rf correction
# this needs to be notes

if (!"rf" %in% names(searo)) {
  searo <- searo %>%
    mutate(
      rf = NA_real_, 
      correction_applied = FALSE,
      correction_reason = "no_lookup_match"
      )
}

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
                       destination = "iso3c"),
    raw_cases = cases,
    corrected_cases = NA_real_
    ) %>%
  dplyr:: select(country,
                 iso3,
                 date,
                 Year,
                 Month,
                 source,
                 cases,
                 raw_cases,
                 corrected_cases,
                 d, rf,
                 correction_applied,
                 correction_reason)

# SEARO does not distinguish 0 from no data; treat 0 as NA so other sources are preferred when available
searo_add <- searo_add %>%
  dplyr::mutate(cases = dplyr::if_else(cases == 0, NA_real_, cases))

# Indonesia was removed from SEARO in June 2025; ignore SEARO for Indonesia from that date onward
searo_add <- searo_add %>%
  dplyr::filter(!(iso3 == "IDN" & date >= as.Date("2025-06-01")))


# == WHO == #
# use the corrected WHO series (who_correction from 02_V2_backfilling.R); applied = corrected-or-raw
who_add <- who_correction %>%
  mutate(
    Year = year(date), 
    Month = format(date, "%b"), 
    source = "WHO",
    # maintain raw, corrected, and (applied) cases in a way that matches for WHO and SEARO
    raw_cases = cases, 
    cases = cases_applied,
    corrected_cases = cases_corrected, 
  ) %>%
  dplyr:: select(country,
                 iso3,
                 date,
                 Year,
                 Month,
                 source,
                 cases,
                 raw_cases,
                 corrected_cases,
                 d, rf,
                 correction_applied,
                 correction_reason)

# Step 2: Combine all data
combine <- bind_rows(paho_add, searo_add, who_add)
log_message("Combined country-month rows across sources: " %+% nrow(combine))


# Step 3: Keep the fewest NAs, then by source preference (Indonesia: prefer WHO; others: prefer PAHO/SEARO over WHO)
final_cases <- combine %>%
  mutate( 
    Month_num = match(Month, month.abb),
    iso3 = ifelse( country == "Saint Martin", "MAF", iso3),
    country = ifelse(country == "Saint Martin (French part)", "Saint Martin", country),
    # Indonesia moved out of SEARO: prefer WHO for IDN; for others prefer PAHO/SEARO over WHO
    source_priority = dplyr::case_when(
      iso3 == "IDN" & source == "WHO"   ~ 1L,
      iso3 == "IDN"                     ~ 2L,
      source == "WHO"                   ~ 2L,
      TRUE                              ~ 1L
    )
  ) %>%
  group_by(iso3, Year, Month_num) %>% 
  arrange(is.na(cases), source_priority) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(-source_priority) %>%
  mutate(
    Month = month.name[Month_num],
  )

# Normalize country names to canonical before completeness expansion
# Must happen here (not later) because tidyr::complete groups by country, iso3
# This prevents duplicate Year x Month grids when the same iso3 has different country names
final_cases <- final_cases %>%
  dplyr::mutate(
    country_canonical = countrycode::countrycode(iso3, "iso3c", "country.name",
                                                  custom_match = c("MDR" = "Autonomous Region of Madeira",
                                                                   "MAF" = "Saint Martin")),
    country = dplyr::if_else(is.na(country_canonical), country, country_canonical)
  ) %>%
  dplyr::select(-country_canonical)

log_message("Country names normalized to canonical names per iso3 (before completeness expansion)")

# Step 4 : Selected needed time frame and columns
current_year <- as.numeric(format(Sys.Date(), "%Y"))
season_start <- current_year - 2

current_data <- final_cases %>%
  filter(Year > season_start) %>%
  dplyr::select(country,
                iso3,
                source,
                date,
                Year,
                Month,
                cases,
                raw_cases,
                corrected_cases,
                d, rf,
                correction_applied,
                correction_reason, 
                missing_reason
                ) %>%
  mutate(
    # Convert Month to numeric more safely
    Month = month(as.POSIXlt(date, format="%d/%m/%Y"))
  )

# Log what years are present before completeness expansion
if (exists("log_message")) {
  years_present <- unique(current_data$Year)
  log_message("Years present in data after filtering (Year > " %+% season_start %+% "): " %+% 
                paste(sort(years_present), collapse = ", "))
}


# Step 5: ensure that all months are listed (even with NAs) for all year:countries
# Also ensure current year is always included even if no data exists
current_data <- current_data %>%
  mutate(
    Year = as.integer(Year),
    Month = as.integer(Month)
  ) %>%
  filter(!is.na(Year) & !is.na(Month)) %>%
  
  group_by(country, iso3) %>%
  tidyr::complete(
    # Create explicit year sequence: from season_start+1 to current_year
    Year = (season_start + 1):current_year,  # Explicit range that always includes current_year
    Month = 1:12,         # ensures months 1–12 for each year
    fill = list(cases = NA, source = NA)  # Fill both cases and source with NA for missing months
  ) %>%
  ungroup() %>%
  
  # add a proper date column
  mutate(date = as.Date(paste0(Year, "-", Month, "-01"))) %>%
  
  # reorder for clarity
  arrange(country, Year, Month)

log_message("Current data rows after completeness expansion: " %+% nrow(current_data))

# Record canonical Step 4 country presence from finalized current_data
if (exists("record_countries_at_step")) {
  tryCatch({
    record_countries_at_step(current_data, "Step_4_Current_Data")
  }, error = function(e) {
    if (exists("log_message")) {
      log_message("Warning: Country tracking failed at Step 4 Current Data: " %+% conditionMessage(e), level = "WARNING")
    }
  })
}

# Verify current year is present for all countries
if (exists("log_message")) {
  all_countries <- current_data %>%
    dplyr::select(country, iso3) %>%
    dplyr::distinct()
  
  countries_with_current_year <- current_data %>%
    dplyr::filter(Year == current_year) %>%
    dplyr::select(country, iso3) %>%
    dplyr::distinct()
  
  if (nrow(countries_with_current_year) == nrow(all_countries)) {
    log_message("All " %+% nrow(all_countries) %+% " countries have current year (" %+% current_year %+% ") included")
  } else {
    log_message("Warning: Only " %+% nrow(countries_with_current_year) %+% "/" %+% nrow(all_countries) %+% 
                  " countries have current year (" %+% current_year %+% ") data", level = "WARNING")
  }
}
