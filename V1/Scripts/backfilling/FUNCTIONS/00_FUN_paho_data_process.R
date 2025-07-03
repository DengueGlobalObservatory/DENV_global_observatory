#' ---
#' title: "00_FUN_paho_data_process"
#' author: "K M Susong"
#' ---
#'
#' Included functions
#' ==================
#'
#' - PAHO_cumm_monthly()
#' - PAHO_incid_monthly()
#'

# Required Libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(ISOweek)
library(purrr)

#' **PAHO_cumm_monthly()**
#'
#' This function estimates corrected dengue case counts by applying a reporting delay factor (`rf`) to weekly PAHO data.
#' It converts weekly onset to dates, computes delay in reporting, merges in empirical correction factors,
#' and summarizes corrected and raw monthly totals per country.
#'
#' @param df Data frame from the PAHO dengue dashboard. Must include columns: `year`, `EW` (epidemiological week),
#' `ext_date` (extraction date), `total_den`, and `country`.
#'
#' @return A data frame containing corrected and raw monthly dengue case totals:
#' `country`, `year`, `month`, `total_monthly_cases_corr` (corrected), and `total_monthly_cases` (raw).
#'
PAHO_cumm_monthly <- function(df){
  
  df <- df %>%
    mutate(
      iso_week = paste0(year, "-W", stringr::str_pad(EW, 2, pad = "0")),
      onset_date = ISOweek2date(paste0(iso_week, "-1"))
    ) %>%
    mutate(
      d = round(as.numeric(difftime(ymd(ext_date), onset_date, units = "weeks")), 0)
    )
  
  rf_df <- read.csv("Data/emp_est_PAHO_report_factor.csv") %>%
    select(country, d, rf) %>%
    mutate(rf = replace_na(rf, 1.000001))
  
  df_combine <- merge(df, rf_df, all.x = TRUE) %>%
    mutate(rf = replace_na(rf, 1.000001))
  
  df_monthly <- df_combine %>%
    mutate(
      total_den_corrected = total_den * rf,
      month = format(onset_date, "%B")
    ) %>%
    group_by(country, year, month) %>%
    slice_max(order_by = EW, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(
      country, year, month,
      total_monthly_cases_corr = total_den_corrected,
      total_monthly_cases = total_den
    ) %>%
    distinct()
  
  return(df_monthly)
}

#' **PAHO_incid_monthly()**
#'
#' This function derives monthly *incidence* estimates from cumulative case counts.
#' It calculates monthly incident dengue cases by taking the difference between successive monthly cumulative totals.
#' Handles missing data and labels reason for missing incidence values.
#'
#' @param df A data frame returned by `PAHO_cumm_monthly()` with columns: `country`, `year`, `month`, `total_monthly_cases_corr`.
#'
#' @return A data frame including:
#' `country`, `date` (first of month), `year`, `month`, `total_monthly_cases_corr`,
#' `computed_monthly_cases` (monthly incident cases), and `missing_reason` (data availability flag).
#'
PAHO_incid_monthly <- function(df) {
    df <- df %>%
      mutate(
        month_num = match(month, month.name),
        date = make_date(year, month_num, 1)
      ) %>%
      select(country, date, total_monthly_cases_corr)
    
    full_df <- df %>%
      group_by(country) %>%
      reframe(
        min_date = min(date, na.rm = TRUE),
        max_date = max(date, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        date_seq = map2(min_date, max_date, ~ seq.Date(.x, .y, by = "month"))
      ) %>%
      select(country, date_seq) %>%
      unnest(date_seq, names_repair = "minimal") %>%
      rename(date = date_seq)
    
    joined_df <- full_df %>%
      left_join(df, by = c("country", "date")) %>%
      arrange(country, date) %>%
      mutate(
        year = year(date),
        month_num = month(date),
        month = month.name[month_num]
      ) %>%
      group_by(country) %>%
      mutate(
        lag_cum = lag(total_monthly_cases_corr),
        computed_monthly_cases = case_when(
          month_num == 1 ~ total_monthly_cases_corr,
          TRUE ~ total_monthly_cases_corr - lag_cum
        ),
        missing_reason = case_when(
          is.na(total_monthly_cases_corr) ~ "current_month_missing",
          is.na(lag_cum) ~ "previous_month_missing",
          TRUE ~ "not_missing"
        )
      ) %>%
      ungroup()
    
    return(joined_df)
  }