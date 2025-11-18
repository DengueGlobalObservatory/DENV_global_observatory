# ------- Libraries

library(DT)
library(htmltools)
library(dplyr)
library(purrr)
library(cropcircles)
library(magick)

# ------- Functions

# snapshot/clockface plot
source("V1/Scripts/figures/Radial.R")
source("V1/Scripts/figures/FUN_utility.R")



# ------- Data 

# Run pipeline, open output data
# source("V1/Scripts/V1_Pipeline.R")

data <- read.csv("V1/Output/2025_10_12/DENV_cases_nowcast_output.csv")

region_summary <- data %>%
  group_by(Region, Year, Month) %>%
  reframe(
    cases = sum(cases, na.rm = TRUE),
    Ave_season_monthly_cases = sum(Ave_season_monthly_cases, na.rm = TRUE),
    Ave_season_monthly_cum_cases = sum(Ave_season_monthly_cum_cases, na.rm = TRUE),
    Predicted_total_seasonal_cases = sum(Predicted_total_seasonal_cases, na.rm = TRUE),
    # Optionally include averages of proportions or percentiles if needed
    Ave_cum_monthly_proportion = mean(Ave_cum_monthly_proportion, na.rm = TRUE),
    Ave_monthly_proportion = mean(Ave_monthly_proportion, na.rm = TRUE),
    percentile_most_recent = mean(percentile_most_recent, na.rm = TRUE),
    n_countries = n_distinct(iso3)
  ) %>%
  ungroup() %>%
  mutate(
    date = as.Date(paste0(Year, "-", Month, "-01")))

# ------- All country Plots

current_year <- as.integer(format(Sys.Date(), "%Y"))

# Use the system date to define "current" and "recent" months ---
current_month <- as.integer(format(Sys.Date(), "%m"))
recent_month <- current_month - 1
if (recent_month == 0) recent_month <- 12  # handle January wrap-around


# Create named list of all countries’ most recent data
data_latest <- data %>%
  filter(Year == current_year)

# 2) split into a named list of data frames (one entry per country)
split_by_country <- split(data_latest, data_latest$Country)  # names = country names

# 3) generate a named list of plots (same order and names as split_by_country)
all_country_plots <- lapply(split_by_country, function(df_country) {
  make_country_plot(df_country)   # your plotting function that takes a country's df
})



# ------ Regional Plots

# 1️⃣ Create the list of region plots
region_plot_list <- region_summary %>%
  filter(Year == current_year) %>%
  split(.$Region) %>%
  purrr::map(make_region_plot)


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
    # CasesLastMonth = cases
  ) %>%
  # take just the row(s) for the most recent month for each country
  # filter(Month == recent_month -1) %>%
  ungroup()


