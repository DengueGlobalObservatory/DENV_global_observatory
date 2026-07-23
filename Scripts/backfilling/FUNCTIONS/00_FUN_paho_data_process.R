# ---
# title: "00_FUN_paho_data_process"
# author: "K M Susong"
# ---
  
# Included functions
# ==================

# - apply_reporting_correction()
# - compute_monthcumm_cases()
# - PAHO_incid_monthly()

# Required Libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(ISOweek)
library(purrr)
library(stringr) # required for str_pad

# ============================================================

#' **apply_reporting_correction()**
#'
#' Applies empirically derived reporting delay correction factors to PAHO weekly dengue data.
#'
#' @description
#' Reads a CSV of reporting factors (`Data/emp_est_PAHO_report_factor.csv`), matches them to each observation's
#' reporting delay in weeks (`d`), and multiplies the case counts by the factor.
#' Correction factors that are `NA`, `0`, greater than `3`, or less than `0.9` are replaced with `1.000001` to indicate no adjustment.
#'
#' @param df Data frame of weekly PAHO data. Must include:
#'   - `year`: numeric epidemiological year
#'   - `EW`: epidemiological week number
#'   - `ext_date`: extraction date (YYYY-MM-DD)
#'   - `country`: country name
#'   - A weekly case count column (default `"total_den"`)
#' @param cases_col String. Name of the column with raw weekly case counts. Default `"total_den"`.
#' @param output_col String. Name of the column for corrected case counts. Default `"total_corrected_cases"`.
#'
#' @return Data frame with:
#' - `onset_date`: Monday of the epidemiological week
#' - `d`: reporting delay (in weeks)
#' - `rf`: correction factor
#' - Corrected case counts in `output_col`
#' - `correction_applied`: TRUE if `rf` != 1.000001, FALSE otherwise
#'
#' @details
#' The `correction_applied` column allows filtering to identify where adjustments were made.
#'

apply_reporting_correction <- function(df, 
                                       cases_col = "total_den",
                                       output_col = "total_corrected_cases") {
  
  # Calculate onset date (Monday of the EW) and reporting delay only if needed
  if (!"onset_date" %in% names(df)) {
    df <- df %>%
      mutate(
        iso_week = paste0(year, "-W", str_pad(EW, 2, pad = "0")),
        onset_date = ISOweek2date(paste0(iso_week, "-1")),
        year = year(onset_date),
        d = round(as.numeric(difftime(ymd(ext_date), onset_date, units = "weeks")), 0)
      )
  }
  
  # Load and clean correction factors
  rf_df <- read.csv("Assets/Stable/emp_est_PAHO_report_factor.csv") %>%
    dplyr::select(country, d, rf) %>%
    mutate(
      rf = case_when(
        is.na(rf) ~ 1.000001,
        rf == 0 ~ 1.000001,
        rf > 3 ~ 1.000001,
        rf < 0.9 ~ 1.000001,
        TRUE ~ rf
      )
    )
  
  # Join reporting factors to data
  df_combine <- merge(df, rf_df, all.x = TRUE) %>%
    mutate(
      rf = replace_na(rf, 1.000001)
    )
  
  # Apply correction
  df_combine[[output_col]] <- round(df_combine[[cases_col]] * df_combine[["rf"]], 0)
  
  # Flag where a correction was actually applied
  df_combine$correction_applied <- df_combine$rf != 1.000001
  
  # Select final output columns
  df_combine <- df_combine %>%
    dplyr::select(
      country, year, ext_date, EW, onset_date, 
      !!sym(cases_col), !!sym(output_col), correction_applied
    )
  
  return(df_combine)
}

# ============================================================
#' **compute_monthcumm_cases()**
#'
#' Produces monthly cumulative case counts from weekly PAHO data.
#'
#' @description
#' For each country-year-month, selects the final epidemiological week’s cumulative total.
#' Handles cases where early January is mistakenly reported as December of the previous year.
#'
#' @param df Data frame with at least:
#'   - `country`
#'   - `onset_date`
#'   - `EW`
#'   - `total_den`
#'   - `total_corrected_cases`
#'   - `correction_applied`
#'
#' @return Data frame with one row per country-month, containing cumulative case counts.
#'


compute_monthcumm_cases <- function(df) {
  
  
  # Calculate onset date (Monday of the EW) and reporting delay
  df <- df %>%
    mutate(
      iso_week = paste0(year, "-W", str_pad(EW, 2, pad = "0")),
      onset_date = ISOweek2date(paste0(iso_week, "-1")),
      year = year(onset_date),
      d = round(as.numeric(difftime(ymd(ext_date), onset_date, units = "weeks")), 0)
    )
  
  df_monthcumm <- df %>%
    mutate(
      month = format(onset_date, "%B"), # add text month 
      month_num = month(onset_date),    # add number month
      year = year(onset_date)           # correct year lable to accuratly reflect the of onset date
    ) %>%
    # select cumm month value
    group_by(country, year, month) %>% 
    slice_max(order_by = EW, n = 1, with_ties = FALSE) %>% # take the final value of each month ( assumed as max EW in each month)
    # because of missing reporting the beginning of the next year is sometimes marked as dec of the previous year
    # this replaces those miss assignments as NAs
    mutate(
      total_den = case_when(
        EW == 1 & month_num == 12 ~ NA,
        TRUE ~ total_den
      )
    ) %>%
    ungroup()
  
  if ("total_corrected_cases" %in% names(df_monthcumm)) {
    df_monthcumm <- df_monthcumm %>%
    # select cumm month value
    group_by(country, year, month) %>% 
    slice_max(order_by = EW, n = 1, with_ties = FALSE) %>%
      mutate(
        total_corrected_cases = case_when(
          EW == 1 & month_num == 12 ~ NA,
          TRUE ~ total_corrected_cases
        ),
        correction_applied = case_when(
          EW == 1 & month_num == 12 ~ NA,
          TRUE ~ correction_applied
        )
      )
  }

  # applied = corrected-or-raw (V2 only); apply the same Dec/Jan mislabel rule
  if ("total_applied_cases" %in% names(df_monthcumm)) {
    df_monthcumm <- df_monthcumm %>%
      mutate(
        total_applied_cases = case_when(
          EW == 1 & month_num == 12 ~ NA,
          TRUE ~ total_applied_cases
        )
      )
  }

  return(df_monthcumm)
}


# ============================================================
#' **PAHO_incid_monthly()**
#'
#' Calculates monthly *incident* dengue cases from cumulative monthly totals.
#'
#' @description
#' For each country, creates a complete sequence of months between the earliest and latest data,
#' merges in cumulative monthly counts, and computes incident cases as the difference between successive months.
#' Handles both corrected and uncorrected case counts.
#'
#' @param df Data frame from `compute_monthcumm_cases()` with:
#'   - `country`
#'   - `onset_date`
#'   - `EW`
#'   - `total_corrected_cases`
#'   - `total_den`
#'   - `correction_applied`
#'
#' @return Data frame with:
#' - Cumulative corrected and uncorrected cases
#' - Computed monthly cases (corrected and uncorrected)
#' - Flags for missing data sources
#'

PAHO_incid_monthly <- function(df) {
   
   # add date column
   df <- df %>%
      mutate(
        date = make_date(year, month_num, 1)
      ) 
    
   # Create complete monthly sequence for each country
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
     dplyr::select(country, date_seq) %>%
     unnest(date_seq, names_repair = "minimal") %>%
     dplyr:: rename(date = date_seq)
   
   # Merge observed data into complete timeline
    joined_df <- full_df %>%
      left_join(df, by = c("country", "date")) %>%
      arrange(country, date) %>%
      mutate(
        year = year(date),
        month_num = month(date),
        month = month.name[month_num]
      ) %>%
      group_by(country) %>%
      # always calculate uncorrected monthly cases
      mutate(
        lag_cum = lag(total_den),
        computed_monthly_cases = case_when(
          month_num == 1 ~ total_den,
          TRUE ~ total_den - lag_cum
        )
      ) %>%
      ungroup()
    
    if ("total_corrected_cases" %in% names(joined_df)) {
      joined_df <- joined_df %>%
        group_by(country) %>%
        mutate(
          lag_cum_corr = lag(total_corrected_cases),
          computed_monthly_cases_corr = case_when(
            month_num == 1 ~ total_corrected_cases,
            TRUE ~ total_corrected_cases - lag_cum_corr
          ),
          missing_reason = case_when(
            is.na(total_corrected_cases) ~ "current_month_missing",
            is.na(lag_cum_corr) ~ "previous_month_missing",
            TRUE ~ "not_missing"
          )
        ) %>%
        ungroup()
    } else {
      joined_df <- joined_df %>%
        mutate(
          total_corrected_cases = NA_real_,
          correction_applied = if ("correction_applied" %in% names(joined_df)) correction_applied else NA,
          lag_cum_corr = NA_real_,
          computed_monthly_cases_corr = NA_real_,
          missing_reason = NA_character_
        )
    }

    # applied monthly series (V2 only): never NA for lack of RF, so no lag-NA cascade
    if ("total_applied_cases" %in% names(joined_df)) {
      joined_df <- joined_df %>%
        group_by(country) %>%
        mutate(
          lag_cum_applied = lag(total_applied_cases),
          computed_monthly_cases_applied = case_when(
            month_num == 1 ~ total_applied_cases,
            TRUE ~ total_applied_cases - lag_cum_applied
          )
        ) %>%
        ungroup()
    }

    joined_df <- joined_df %>%
      distinct()

    return(joined_df)
}

