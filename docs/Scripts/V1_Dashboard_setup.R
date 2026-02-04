# ------- Libraries

library(DT)
library(htmltools)
library(dplyr)
library(purrr)
library(cropcircles)
library(magick)

# ------- Helpers

get_latest_dataset_info <- function(output_root = "Output") {
  output_dirs <- list.dirs(output_root, recursive = FALSE, full.names = TRUE)
  nowcast_dirs <- output_dirs[file.exists(file.path(output_dirs, "DENV_cases_nowcast_output.csv"))]
  
  if (length(nowcast_dirs) == 0) {
    return(list(label = "Not available", dir = NA_character_, file = NA_character_))
  }
  
  folder_dates <- as.Date(gsub("_", "-", basename(nowcast_dirs)))
  valid_dates <- !is.na(folder_dates)
  
  if (!any(valid_dates)) {
    return(list(label = "Not available", dir = NA_character_, file = NA_character_))
  }
  
  folder_dates <- folder_dates[valid_dates]
  nowcast_dirs <- nowcast_dirs[valid_dates]
  latest_idx <- which.max(folder_dates)
  latest_update_date <- folder_dates[latest_idx]
  latest_dir <- nowcast_dirs[latest_idx]
  
  list(
    label = format(latest_update_date, "%d %B %Y"),
    dir = latest_dir,
    file = file.path(latest_dir, "DENV_cases_nowcast_output.csv")
  )
}


latest_dataset_info <- get_latest_dataset_info()
latest_update_label <- latest_dataset_info$label
latest_nowcast_dir <- latest_dataset_info$dir

# ------- Functions

# snapshot/clockface plot
source("Scripts/figures/Radial.R")
source("Scripts/figures/FUN_utility.R")


# Function to get data status for countries
get_country_data_status <- function(data_df, country_col_name = NULL) {
  if (is.null(country_col_name)) {
    country_col_name <- if ("Country" %in% names(data_df)) "Country" else "country"
  }
  
  recent_months_data <- data_df %>%
    dplyr::filter(
      Year == current_year,
      Month <= recent_month,
      Month >= max(1, recent_month - 2)  # Last 3 months
    ) %>%
    dplyr::group_by(.data[[country_col_name]]) %>%
    dplyr::summarise(
      has_estimated = any(source == "Estimates", na.rm = TRUE),
      has_observed = any(source != "Estimates" & !is.na(source), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      data_status_message = dplyr::case_when(
        has_estimated & has_observed ~ "Some recent months contain estimated data",
        has_estimated ~ "Recent months contain estimated data",
        TRUE ~ "Recent months contain observed data only"
      )
    ) %>%
    dplyr::rename(country_name = .data[[country_col_name]])
  
  return(recent_months_data)
}


# ------- Universal Objects

current_year <- as.integer(format(Sys.Date(), "%Y"))
# current_year <- 2025

# Use the system date to define "current" and "recent" months ---
current_month <- as.integer(format(Sys.Date(), "%m"))
# current_month <- 1
recent_month <- current_month - 1
if (recent_month == 0) recent_month <- 12  # handle January wrap-around

# ------- Data 

# Run pipeline, open output data
# source("Scripts/V1_Pipeline.R")

# Get the most recent data file
data_path <- if (!is.na(latest_dataset_info$file) && file.exists(latest_dataset_info$file)) {
  latest_dataset_info$file
} else {
  # Fallback to default if no recent file found
  "Output/2025_10_12/DENV_cases_nowcast_output.csv"
}

data <- read.csv(data_path, check.names = FALSE)

# data <- read.csv("V1/Output/2026_01_14/DENV_cases_nowcast_output.csv")

# Remove unnamed first column if it exists (often created by row numbers in CSV)
col_names <- names(data)
if (length(col_names) > 0 && (col_names[1] == "" || col_names[1] == "X" || col_names[1] == "X.")) {
  data <- data %>% dplyr::select(-1)
  message("Removed unnamed first column from data")
}

# ----- Corrections and checks

# Correct case data to NA for dates after the current month 

data <- data %>%
  mutate(
    is_future = (Year > current_year) | (Year == current_year & Month > recent_month),
    cases = if_else(is_future, NA_real_, cases),
    cum_todate_cases_year = if_else(is_future, NA_real_, cum_todate_cases_year),
    cum_todate_cases_season   = if_else(is_future, NA_real_, cum_todate_cases_season)
  ) %>%
  dplyr::select(-is_future)


# ----- Build regional Summary

region_summary <- data %>%
  group_by(Region, Year, Month) %>%
  reframe(
    cases = sum(cases),
    Ave_season_monthly_cases = sum(Ave_season_monthly_cases, na.rm = TRUE),
    Ave_season_monthly_cum_cases = sum(Ave_season_monthly_cum_cases, na.rm = TRUE),
    Predicted_total_seasonal_cases = sum(Predicted_total_seasonal_cases, na.rm = TRUE),
    # Optionally include averages of proportions or percentiles if needed
    Ave_cum_monthly_proportion = mean(Ave_cum_monthly_proportion),
    Ave_monthly_proportion = mean(Ave_monthly_proportion),
    # percentile_most_recent = mean(percentile_most_recent, na.rm = TRUE),
    n_countries = n_distinct(iso3)
  ) %>%
  ungroup() %>%
  mutate(
    date = as.Date(paste0(Year, "-", sprintf("%02d", Month), "-01"))
  ) %>%
  # Remove any rows with invalid dates
  filter(!is.na(date))


# ------- Regional helper assets & callouts

format_big_number <- function(x) {
  vapply(
    x,
    FUN.VALUE = character(1),
    function(value) {
      if (is.na(value)) {
        "data pending"
      } else {
        format(round(value), big.mark = ",", trim = TRUE, scientific = FALSE)
      }
    }
  )
}


# ----- Build Text phrases for auto text 

ratio_phrase <- function(ratio, baseline_phrase = "the seasonal baseline") {
  len <- length(ratio)
  if (len == 0) return(character(0))
  baseline_phrase <- rep_len(baseline_phrase, len)
  output <- character(len)
  
  na_idx <- is.na(ratio)
  output[na_idx] <- paste("tracking close to", baseline_phrase[na_idx])
  
  if (any(!na_idx)) {
    descriptor <- dplyr::case_when(
      ratio >= 1.3 ~ "running well above",
      ratio >= 1.1 ~ "running slightly above",
      ratio <= 0.7 ~ "running well below",
      ratio <= 0.9 ~ "running slightly below",
      TRUE ~ "tracking near"
    )
    output[!na_idx] <- paste(descriptor[!na_idx], baseline_phrase[!na_idx])
  }
  output
}

season_badge_label_text <- function(ratio) {
  len <- length(ratio)
  if (len == 0) return(character(0))
  
  output <- rep("Season tracking near average", len)
  valid_idx <- !is.na(ratio)
  if (any(valid_idx)) {
    output[valid_idx] <- dplyr::case_when(
      ratio >= 1.2 ~ "Season running high",
      ratio <= 0.85 ~ "Season running low",
      TRUE ~ "Season near baseline"
    )[valid_idx]
  }
  output
}

season_badge_state_class <- function(ratio) {
  len <- length(ratio)
  if (len == 0) return(character(0))
  
  output <- rep("is-neutral", len)
  valid_idx <- !is.na(ratio)
  if (any(valid_idx)) {
    output[valid_idx] <- dplyr::case_when(
      ratio >= 1.2 ~ "is-above",
      ratio <= 0.85 ~ "is-below",
      TRUE ~ "is-neutral"
    )[valid_idx]
  }
  output
}

region_map_lookup <- c(
  "South America" = "../Assets/Stable/OD_maps/temp_maps/southamerica.png",
  "Caribbean" = "../Assets/Stable/OD_maps/temp_maps/caribbean.png",
  "Pacific Islands" = "../Assets/Stable/OD_maps/temp_maps/pacific.png",
  "South Asia" = "../Assets/Stable/OD_maps/temp_maps/southasia.png",
  "North & Central America" = "../Assets/Stable/OD_maps/temp_maps/mexico_centralamerica.png",
  "Sub-Saharan Africa" = "../Assets/Stable/OD_maps/temp_maps/africa.png",
  "East & Southeast Asia" = "../Assets/Stable/OD_maps/temp_maps/southeast_east_asia.png",
  "Europe, Middle East & North Africa" = "../Assets/Stable/OD_maps/temp_maps/europe_middleeast.png"
)

region_callouts <- {
  latest_rows <- region_summary %>%
    filter(Year == current_year, !is.na(date)) %>%
    mutate(
      monthly_ratio = if_else(Ave_season_monthly_cases > 0, cases / Ave_season_monthly_cases, NA_real_)
    ) %>%
    filter(!is.na(cases)) %>%
    group_by(Region) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  region_ytd <- region_summary %>%
    filter(Year == current_year, Month <= recent_month) %>%
    group_by(Region) %>%
    dplyr::summarise(
      ytd_cases = sum(cases, na.rm = TRUE),
      ytd_expected = sum(Ave_season_monthly_cases, na.rm = TRUE),
       .groups = "drop"
    )
  
  latest_rows %>%
    left_join(region_ytd, by = "Region") %>%
    mutate(
      latest_month_label = format(date, "%B %Y"),
      latest_cases_label = format_big_number(cases),
      monthly_ratio_phrase = ratio_phrase(monthly_ratio),
      ytd_cases_label = dplyr::if_else(
        !is.na(ytd_cases) & ytd_cases > 0,
        format_big_number(ytd_cases),
        NA_character_
      ),
      ytd_ratio = if_else(ytd_expected > 0, ytd_cases / ytd_expected, NA_real_),
      ytd_ratio_phrase = ratio_phrase(ytd_ratio, "the expected burden"),
      latest_sentence = glue::glue("{Region} logged {latest_cases_label} cases in {latest_month_label}, {monthly_ratio_phrase}."),
      ytd_sentence = ifelse(
        !is.na(ytd_cases_label),
        glue::glue("Year-to-date totals sit at {ytd_cases_label}, {ytd_ratio_phrase}."),
        "Year-to-date totals are still being compiled."
      ),
      season_badge_label = season_badge_label_text(ytd_ratio),
      season_badge_state = season_badge_state_class(ytd_ratio)
    )
}


# ------- All country Plots


# 1) split into a named list of data frames (one entry per country)
split_by_country <- split(data, data$country)  # names = country names

# 2) generate a named list of plots (same order and names as split_by_country)
# Wrap in tryCatch to prevent one failure from breaking dashboard
all_country_plots <- lapply(names(split_by_country), function(country_name) {
  df_country <- split_by_country[[country_name]]
  tryCatch({
    plot_result <- make_radial_plot(df_country, current_year, current_month)
    if (is.null(plot_result)) {
      if (exists("log_message")) {
        log_message("Warning: Failed to generate plot for " %+% country_name %+% " (returned NULL)", level = "WARNING")
      }
    }
    return(plot_result)
  }, error = function(e) {
    if (exists("log_message")) {
      log_message("Error generating plot for " %+% country_name %+% ": " %+% conditionMessage(e), level = "ERROR")
    }
    return(NULL)
  })
})

# 3) Name the list with country names
names(all_country_plots) <- names(split_by_country)

# 4) Filter out NULL entries
all_country_plots <- all_country_plots[!sapply(all_country_plots, is.null)]



# ------ Regional Plots

# 1️⃣ Create the list of region plots
region_plot_list <- region_summary %>%
  split(.$Region) %>%
  purrr::map(~ make_radial_plot(.x, current_year, current_month))



# ------ World Plot

world_summary <- data %>%
  group_by(Year, Month) %>%
  reframe(
    cases = sum(cases),
    Ave_season_monthly_cases = sum(Ave_season_monthly_cases, na.rm = TRUE),
    Ave_season_monthly_cum_cases = sum(Ave_season_monthly_cum_cases, na.rm = TRUE),
    Predicted_total_seasonal_cases = sum(Predicted_total_seasonal_cases, na.rm = TRUE),
    # Optionally include averages of proportions or percentiles if needed
    Ave_cum_monthly_proportion = mean(Ave_cum_monthly_proportion),
    Ave_monthly_proportion = mean(Ave_monthly_proportion),
    # percentile_most_recent = mean(percentile_most_recent, na.rm = TRUE),
    n_countries = n_distinct(iso3)
  ) %>%
  ungroup() %>%
  mutate(
    date = as.Date(paste0(Year, "-", Month, "-01")))

world_plot <- make_radial_plot(world_summary, current_year, current_month)

world_summary_text <- world_summary %>%
  # keep only the focal year (you supply current_year)
  filter(Year == current_year) %>%
  arrange(Month) %>%
  mutate(
    # rename / compute the easy-to-read helpers
    low_speed_raw  = Ave_season_monthly_cases,
    high_speed_raw = cases,
    RecentRatio    = round(high_speed_raw / low_speed_raw, 2),
    cappedRatio    = pmin(pmax(RecentRatio, 0.5), 2) 
  ) %>%
  # compute cumulative sums up to (and including) recent_month
  mutate(
    cum_low  = sum(low_speed_raw[Month <= recent_month], na.rm = TRUE),
    cum_high = sum(high_speed_raw[Month <= recent_month], na.rm = TRUE),
    # handle division by zero safely
    cum_ratio = if_else(cum_low == 0, NA_real_, cum_high / cum_low),
    # cap ratio between 0.5 and 2
    cum_ratio_capped = pmin(pmax(cum_ratio, 0.5), 2),
    # seasonal status based on recent ratio
    SeasonStatus = case_when(
      RecentRatio > 1.2 ~ "above",
      RecentRatio < 0.8 ~ "below",
      TRUE              ~ "near"   # covers 0.8 <= RecentRatio <= 1.2 and NA-safe
    ),
    # cases for the (single) most recent month (may be NA if not present)
    # CasesLastMonth = cases
  ) %>%
  # take just the row(s) for the most recent month for each country
  # filter(Month == recent_month -1) %>%
  ungroup()



#------ country summary 


country_summary_df <- data %>%
  # keep only the focal year (you supply current_year)
  filter(Year == current_year) %>%
  group_by(country) %>%            # compute cumulatives per country
  arrange(Month) %>%
  mutate(
    # rename / compute the easy-to-read helpers
    low_speed_raw  = Ave_season_monthly_cases,
    high_speed_raw = cases,
    RecentRatio    = round(high_speed_raw / low_speed_raw, 2),
    cappedRatio    = pmin(pmax(RecentRatio, 0.5), 2) 
  ) %>%
  # compute cumulative sums up to (and including) recent_month
  mutate(
    cum_low  = sum(low_speed_raw[Month <= recent_month], na.rm = TRUE),
    cum_high = sum(high_speed_raw[Month <= recent_month], na.rm = TRUE),
    # handle division by zero safely
    cum_ratio = if_else(cum_low == 0, NA_real_, cum_high / cum_low),
    # cap ratio between 0.5 and 2
    cum_ratio_capped = pmin(pmax(cum_ratio, 0.5), 2),
    # seasonal status based on recent ratio
    SeasonStatus = case_when(
      RecentRatio > 1.2 ~ "above",
      RecentRatio < 0.8 ~ "below",
      TRUE              ~ "near"   # covers 0.8 <= RecentRatio <= 1.2 and NA-safe
    ),
    # cases for the (single) most recent month (may be NA if not present)
    CasesLastMonth = cases
  ) %>%
  # take just the row(s) for the most recent month for each country
  # filter(Month == recent_month -1) %>%
  ungroup()


# ------- High severity country snapshot -------

top_severity_countries <- dplyr::tibble()
top_severity_plots <- list()

country_name_col <- dplyr::case_when(
  "country" %in% names(country_summary_df) ~ "country",
  "Country" %in% names(country_summary_df) ~ "Country",
  TRUE ~ NA_character_
)

if (!is.na(country_name_col)) {
  severity_base <- country_summary_df %>%
    dplyr::mutate(country_label = .data[[country_name_col]]) %>%
    dplyr::filter(
      !is.na(country_label) & country_label != "",
      !is.na(Month),
      Month <= recent_month
    ) %>%
    dplyr::group_by(country_label) %>%
    dplyr::arrange(Month, .by_group = TRUE) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      region_label = dplyr::coalesce(.data[["Region"]], ""),
      region_slug = if_else(
        nzchar(region_label),
        tolower(gsub(" & |, | ", "", region_label)),
        NA_character_
      ),
      region_href = if_else(
        !is.na(region_slug),
        paste0(region_slug, ".html"),
        NA_character_
      )
    )
  
  severity_selection <- severity_base %>%
    dplyr::filter(!is.na(cum_ratio) & is.finite(cum_ratio)) %>%
    dplyr::arrange(dplyr::desc(cum_ratio)) %>%
    dplyr::slice_head(n = 5) %>%
    dplyr::mutate(
      severity_ratio_label = sprintf("%.1f", cum_ratio),
      severity_cases_label = format_big_number(cum_high),
      severity_blurb = severity_country_blurb(
        country_label,
        cum_ratio,
        cum_high,
        region = region_label,
        region_href = region_href
      )
    )
  
  if (nrow(severity_selection) > 0) {
    top_severity_countries <- severity_selection
    top_severity_plots <- purrr::map(top_severity_countries$country_label, function(country_name) {
      all_country_plots[[country_name]]
    })
    names(top_severity_plots) <- top_severity_countries$country_label
    top_severity_plots <- purrr::compact(top_severity_plots)
  }
}

# ------- Country data status (for reuse across pages) -------

# Create country data status lookup
country_data_status <- get_country_data_status(data)

