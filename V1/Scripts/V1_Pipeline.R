#' ---
#' title: "V1_Pipeline"
#' author: "K M Susong"
#' 
#' ---
#'

library(dplyr)
library(lubridate)
library(purrr)
source("V1/Scripts/utils/logging.R")
source("V1/Scripts/utils/country_tracking.R")

#------ Step 1: start log

run_dir <- file.path("V1", "Output", format(Sys.Date(), "%Y_%m_%d"))

run_dir_created <- FALSE
if (!dir.exists(run_dir)) {
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  run_dir_created <- TRUE
}

log_file <- start_logging(log_dir = run_dir, log_prefix = "pipeline_log", console = TRUE)

on.exit({
  log_message("Pipeline run completed.")
  close_logger()
}, add = TRUE)

log_message("=============================================")
if (run_dir_created) {
  log_message("Created run directory: " %+% run_dir)
} else {
  log_message("Run directory already existed: " %+% run_dir)
}
if (!is.null(log_file)) {
  log_message("Log file: " %+% log_file)
}
log_message("R version: " %+% R.version.string)
log_message("Working directory: " %+% getwd())
log_message("=============================================")

#------ Step 2: Open Data

log_message("Step 2: Opening dengue-related data sources...")

## this opens all opendengue, paho, searo and who data
source("V1/Scripts/data_sourcing/01_dengue_data.R")

# Check that expected objects were created (adjust names as needed)
expected_objects <- c("OD_national", "paho", "searo", "who")
missing_objs <- expected_objects[!expected_objects %in% ls()]

if (length(missing_objs) > 0) {
  log_message(paste("Missing expected data objects:", paste(missing_objs, collapse = ", ")), level = "ERROR")
  stop("Critical data objects missing: ", paste(missing_objs, collapse = ", "))
} else {
  log_message("All expected data objects are present.")
  log_message(sprintf(
    "Object sizes — OD_national: %s rows, paho: %s rows, searo: %s rows, who: %s rows",
    nrow(OD_national), nrow(paho), nrow(searo), nrow(who)
  ))

  # Initialize country tracking after Step 2 (only if data loaded successfully)
  # Combine all countries from initial data sources
  all_initial_countries_list <- list()
  
  if (exists("OD_national") && nrow(OD_national) > 0) {
    if ("ISO_A0" %in% names(OD_national) && "adm_0_name" %in% names(OD_national)) {
      all_initial_countries_list[[length(all_initial_countries_list) + 1]] <- 
        OD_national %>% dplyr::select(country = adm_0_name, iso3 = ISO_A0) %>% dplyr::distinct()
    }
  }
  
  if (exists("paho") && nrow(paho) > 0 && "country" %in% names(paho) && "iso3" %in% names(paho)) {
    all_initial_countries_list[[length(all_initial_countries_list) + 1]] <- 
      paho %>% dplyr::select(country, iso3) %>% dplyr::distinct()
  }
  
  if (exists("searo") && nrow(searo) > 0 && "country" %in% names(searo) && "iso3" %in% names(searo)) {
    all_initial_countries_list[[length(all_initial_countries_list) + 1]] <- 
      searo %>% dplyr::select(country, iso3) %>% dplyr::distinct()
  }
  
  if (exists("who") && nrow(who) > 0 && "country" %in% names(who) && "iso3" %in% names(who)) {
    all_initial_countries_list[[length(all_initial_countries_list) + 1]] <- 
      who %>% dplyr::select(country, iso3) %>% dplyr::distinct()
  }
  
  if (length(all_initial_countries_list) > 0) {
    all_initial_countries <- dplyr::bind_rows(all_initial_countries_list) %>% dplyr::distinct()
    initialize_country_tracking(all_initial_countries)
    log_message("Country tracking initialized with " %+% nrow(all_initial_countries) %+% " countries")
  } else {
    log_message("Warning: Could not initialize country tracking - no valid country data found", level = "WARNING")
  }
}


# ----- Step 3: determine historic average and baseline seasonality

log_message( "Step 3: selected and combine historic data")

# Select the source of historic data
source("V1/Scripts/data_sourcing/01_select_historic_data.R")

log_message( "Step 3: define average season")

# calculate average season 
source("V1/Scripts/seasonal_baseline/02_identify_seasonal_baseline.R") 


#------ Step 4: Data selection and Backfilling  
log_message( "Step 4: select current season data and backfill")


source("V1/Scripts/backfilling/02_PAHO_monthly_cases_and_source_selection.R")

#------ Step 5: Nowcasting to Date 
log_message( "Step 5: Nowcast missing recent months")


source("V1/Scripts/nowcasting/03_proportion_nowcast.R")

# Filter out countries without seasonal averages (Ave_season_monthly_cases)
# These countries cannot have plots or analysis completed
if (exists("data") && "Ave_season_monthly_cases" %in% names(data)) {
  countries_before_seasonal_filter <- data %>%
    dplyr::select(country, iso3) %>%
    dplyr::distinct()
  
  # Keep only countries with at least one non-NA Ave_season_monthly_cases value
  data <- data %>%
    dplyr::group_by(iso3) %>%
    dplyr::filter(any(!is.na(Ave_season_monthly_cases))) %>%
    dplyr::ungroup()
  
  countries_after_seasonal_filter <- data %>%
    dplyr::select(country, iso3) %>%
    dplyr::distinct()
  
  excluded_countries <- countries_before_seasonal_filter %>%
    dplyr::filter(!iso3 %in% countries_after_seasonal_filter$iso3)
  
  if (nrow(excluded_countries) > 0) {
    log_message("Excluded " %+% nrow(excluded_countries) %+% " countries without seasonal averages from final output")
    log_message("Excluded countries: " %+% paste(excluded_countries$iso3, collapse = ", "))
    
    # Record excluded countries in tracking if available
    if (exists("record_countries_at_step")) {
      tryCatch({
        excluded_drop_reasons <- excluded_countries %>%
          dplyr::mutate(drop_reason = "Excluded: no seasonal baseline (Ave_season_monthly_cases)") %>%
          dplyr::select(iso3, drop_reason)
        
        # Record these as dropped at a new step
        record_countries_at_step(data, "Step_5_After_Seasonal_Filter",
                                 drop_reason = excluded_drop_reasons)
      }, error = function(e) {
        log_message("Warning: Could not record excluded countries: " %+% conditionMessage(e), level = "WARNING")
      })
    }
  } else {
    log_message("All countries have seasonal averages")
  }
} else {
  log_message("Warning: Ave_season_monthly_cases column not found in data - cannot filter", level = "WARNING")
}

#------ Step 6: Post-processing 
log_message( "Step 6: post processing")


# ---- Regional monthly summary ----

# add region back in:

region_map <- OD_national %>%
  mutate(
    country = str_to_title(adm_0_name), # fix ALL CAP
    iso3 = ISO_A0,
    Region = od_region  # Use od_region column from source data
  ) %>%
  dplyr::select(country, iso3, Region) %>%
  unique()

# Merge using iso3 for more reliable matching
data <- merge(data, region_map, by = "iso3", all.x = TRUE)

# If country names don't match, prefer the one from region_map
if ("country.x" %in% names(data) && "country.y" %in% names(data)) {
  data <- data %>%
    dplyr::mutate(
      country = dplyr::coalesce(country.y, country.x)
    ) %>%
    dplyr::select(-country.x, -country.y)
} else if ("country.x" %in% names(data)) {
  data <- data %>% dplyr::rename(country = country.x)
} else if ("country.y" %in% names(data)) {
  data <- data %>% dplyr::rename(country = country.y)
}

# If region names don't match, prefer the one from region_map
if ("Region.x" %in% names(data) && "Region.y" %in% names(data)) {
  data <- data %>%
    dplyr::mutate(
      Region = dplyr::coalesce(Region.x, Region.y)
    ) %>%
    dplyr::select(-Region.x, -Region.y)
} else if ("Region.x" %in% names(data)) {
  data <- data %>% dplyr::rename(Region = Region.x)
} else if ("Region.y" %in% names(data)) {
  data <- data %>% dplyr::rename(Region = Region.y)
}


# Check for countries without region assignments
countries_without_region <- data %>%
  dplyr::filter(is.na(Region)) %>%
  dplyr::select(country, iso3) %>%
  dplyr::distinct()

if (nrow(countries_without_region) > 0) {
  log_message("Warning: " %+% nrow(countries_without_region) %+% " countries without region assignment: " %+% 
              paste(countries_without_region$iso3, collapse = ", "), level = "WARNING")
  
  # Try to assign regions using the get_od_regions function if available
  if (file.exists("OD_maps/fn_OD_region.R")) {
    source("OD_maps/fn_OD_region.R")
    if (exists("get_od_regions")) {
      missing_regions <- get_od_regions(countries_without_region$iso3)
      if (nrow(missing_regions) > 0) {
        # Update data with missing regions
        data <- data %>%
          dplyr::left_join(
            missing_regions %>% dplyr::select(ISO_A0, od_region) %>% dplyr::rename(iso3 = ISO_A0, Region = od_region),
            by = "iso3",
            suffix = c("", ".new")
          ) %>%
          dplyr::mutate(
            Region = dplyr::coalesce(Region, Region.new)
          ) %>%
          dplyr::select(-Region.new)
        
        log_message("Assigned regions to " %+% sum(!is.na(data$Region[data$iso3 %in% countries_without_region$iso3])) %+% " previously missing countries")
      }
    }
  }
}

region_levels <- c(
  "South America",
  "Caribbean",
  "Pacific Islands",
  "South Asia",
  "North & Central America",
  "Sub-Saharan Africa",
  "East & Southeast Asia",
  "Europe, Middle East & North Africa"
)

data <- data %>%
  mutate(Region = factor(Region, levels = region_levels))

# Record countries in final data (Step 6) - only if tracking was initialized
if (exists("record_countries_at_step")) {
  tryCatch({
    record_countries_at_step(data, "Step_6_Final")
  }, error = function(e) {
    log_message("Warning: Could not record countries at Step 6: " %+% conditionMessage(e), level = "WARNING")
  })
}


# #  Regional Summary 
# 
# region_summary <- data %>%
#   group_by(Region, Year, Month) %>%
#   reframe(
#     cases = sum(cases, na.rm = TRUE),
#     Ave_season_monthly_cases = sum(Ave_season_monthly_cases, na.rm = TRUE),
#     Ave_season_monthly_cum_cases = sum(Ave_season_monthly_cum_cases, na.rm = TRUE),
#     Predicted_total_seasonal_cases = sum(Predicted_total_seasonal_cases, na.rm = TRUE),
#     # Optionally include averages of proportions or percentiles if needed
#     Ave_cum_monthly_proportion = mean(Ave_cum_monthly_proportion, na.rm = TRUE),
#     Ave_monthly_proportion = mean(Ave_monthly_proportion, na.rm = TRUE),
#     percentile_most_recent = mean(percentile_most_recent, na.rm = TRUE),
#     n_countries = n_distinct(iso3)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     date = as.Date(paste0(Year, "-", Month, "-01")))
# 
# log_message("Region summary rows: " %+% nrow(region_summary))

#------ Step 7 : monthly severity ( based on nb distribution)

# Calculate real-time severity based on cumulative cases to date
# This updates monthly as the season progresses
data_sev <- data %>%
  group_by(country, iso3, season_nMonth) %>%
  dplyr::mutate(
    # Get NB parameters for cumulative cases at this season_nMonth
    nb_size_cum_at_month = dplyr::first(nb_size_cum),
    nb_mean_cum_at_month = dplyr::first(nb_mean_cum),
    
    # Calculate percentile of current cumulative cases vs. historical distribution
    # at this same point in the season
    percentile_cumulative = ifelse(
      !is.na(cum_todate_cases_season) & 
        !is.na(nb_size_cum_at_month) & 
        !is.na(nb_mean_cum_at_month),
      pnbinom(
        q = cum_todate_cases_season,
        size = nb_size_cum_at_month,
        mu = nb_mean_cum_at_month
      ) * 100,
      NA_real_
    ),
    
    # Classify severity based on percentile
    severity = dplyr::case_when(
      is.na(percentile_cumulative) ~ "Unknown",
      percentile_cumulative <= 5 ~ "Extremely Low",
      percentile_cumulative <= 25 ~ "Low",
      percentile_cumulative < 75 ~ "Normal",
      percentile_cumulative < 95 ~ "High",
      percentile_cumulative >= 95 ~ "Extremely High",
      TRUE ~ "Unknown"
    ),
    
    severity_interpretation = dplyr::case_when(
      severity == "Extremely Low" ~ "Rare good event - unusually low cases to date",
      severity == "Low" ~ "Below average - fewer cases than typical at this point",
      severity == "Normal" ~ "Average - typical case load at this point",
      severity == "High" ~ "Above average - more cases than typical at this point",
      severity == "Extremely High" ~ "Rare bad event - unusually high cases to date",
      TRUE ~ "Cannot determine"
    ),
    
    # Also calculate how many standard deviations from mean (alternative metric)
    z_score_cumulative = ifelse(
      !is.na(cum_todate_cases_season) & 
        !is.na(nb_mean_cum_at_month) & 
        !is.na(nb_size_cum_at_month),
      # Approximate SD from NB parameters: sqrt(mu + mu^2/size)
      (cum_todate_cases_season - nb_mean_cum_at_month) / 
        sqrt(nb_mean_cum_at_month + (nb_mean_cum_at_month^2 / nb_size_cum_at_month)),
      NA_real_
    )
  ) %>%
  dplyr::ungroup()

# For the most recent month, extract the current season severity
data_sev <- data_sev %>%
  group_by(country, iso3, Year) %>%
  dplyr::mutate(
    # Get most recent month's severity for this season
    current_season_severity = dplyr::last(severity[!is.na(severity)]),
    current_season_percentile = dplyr::last(percentile_cumulative[!is.na(percentile_cumulative)]),
    current_season_interpretation = dplyr::last(severity_interpretation[!is.na(severity_interpretation)])
  ) %>%
  dplyr::ungroup() %>%
  mutate(
    Country = country
  )


#----- Step 8: Save outputs 
# ------ Save output df 


current_data_path <- file.path(run_dir, "DENV_cases_backfill_output.csv")
nowcast_path <- file.path(run_dir, "DENV_cases_nowcast_output.csv")
season_path <- file.path(run_dir, "DENV_average_season.csv")
tracking_path <- file.path(run_dir, "country_tracking.csv")

log_message("Saving outputs to " %+% run_dir)
write.csv(current_data, file = current_data_path)
log_message("Saved backfill output: " %+% current_data_path)
write.csv(data_sev, file = nowcast_path, row.names = FALSE)
log_message("Saved nowcast output: " %+% nowcast_path)
write.csv(full_data_average_season, file = season_path, row.names = FALSE)
log_message("Saved average season output: " %+% season_path)

# Export country tracking (if it was initialized)
if (exists("export_country_tracking")) {
  tryCatch({
    export_country_tracking(tracking_path)
  }, error = function(e) {
    log_message("Warning: Could not export country tracking: " %+% conditionMessage(e), level = "WARNING")
  })
}


