# Create summary dataframe for troubleshooting
country_summary <- data %>%
  dplyr::group_by(country, iso3) %>%
  dplyr::summarise(
    # Check if seasonal baseline exists for ALL 12 months
    # A country has a baseline only if all 12 season_nMonth values have Ave_season_monthly_cases
    has_seasonal_baseline = if (any(!is.na(season_nMonth))) {
      # Get distinct season_nMonth values that have non-NA baseline
      months_with_baseline <- unique(season_nMonth[!is.na(Ave_season_monthly_cases)])
      # Check if we have all 12 months (1-12)
      length(months_with_baseline) == 12 && all(1:12 %in% months_with_baseline)
    } else {
      FALSE
    },
    
    # Count of months with baseline (for diagnostics)
    n_months_with_baseline = length(unique(season_nMonth[!is.na(Ave_season_monthly_cases)])),
    
    # Which season_nMonth values are missing baseline (for troubleshooting)
    missing_baseline_months = if (any(!is.na(season_nMonth))) {
      all_months <- 1:12
      months_with_baseline <- unique(season_nMonth[!is.na(Ave_season_monthly_cases)])
      missing <- setdiff(all_months, months_with_baseline)
      if (length(missing) > 0) {
        paste(missing, collapse = ", ")
      } else {
        "None"
      }
    } else {
      "No season_nMonth"
    },
    
    # Count months with case data (non-NA cases)
    n_months_with_data = sum(!is.na(cases)),
    
    # Total months in dataset (including NA months)
    total_months_in_data = n(),
    
    # Last month with case data
    last_month_with_data = if (any(!is.na(cases))) {
      Month[max(which(!is.na(cases)))]
    } else {
      NA_integer_
    },
    
    # Last year with case data
    last_year_with_data = if (any(!is.na(cases))) {
      Year[max(which(!is.na(cases)))]
    } else {
      NA_integer_
    },
    
    # Last date with case data (for easier reading) - ensure consistent Date type
    last_date_with_data = if (any(!is.na(cases))) {
      date_val <- date[max(which(!is.na(cases)))]
      # Ensure it's a Date type
      if (inherits(date_val, "Date")) {
        date_val
      } else {
        as.Date(date_val)
      }
    } else {
      as.Date(NA)
    },
    
    # First month with case data (optional - helps see data range)
    first_month_with_data = if (any(!is.na(cases))) {
      Month[min(which(!is.na(cases)))]
    } else {
      NA_integer_
    },
    
    # First year with case data
    first_year_with_data = if (any(!is.na(cases))) {
      Year[min(which(!is.na(cases)))]
    } else {
      NA_integer_
    },
    
    # Count of months with estimated cases (source == "Estimates")
    n_months_estimated = sum(source == "Estimates", na.rm = TRUE),
    
    # Count of months with observed cases (source != "Estimates" and not NA)
    n_months_observed = sum(!is.na(source) & source != "Estimates" & !is.na(cases)),
    
    # Most recent source type
    most_recent_source = if (any(!is.na(cases))) {
      source_val <- source[max(which(!is.na(cases)))]
      if (is.na(source_val)) NA_character_ else as.character(source_val)
    } else {
      NA_character_
    },
    
    .groups = "drop"
  ) %>%
  dplyr::arrange(country)

# Countries with incomplete baseline (shows which months are missing)
country_summary %>% 
  filter(last_month_with_data != 1 & last_year_with_data != 2026) %>%
  dplyr::select(country, iso3, n_months_with_data, last_month_with_data, last_year_with_data, most_recent_source)




# Diagnostic: Check why estimates aren't being created for problem countries
problem_countries <- c("ATG", "CIV", "GUY", "MYS", "FSM", "REU", "KNA", "TTO", "TCA", "VGB", "WLF")

# Check what last_month_date is
cat("Current date:", Sys.Date(), "\n")
cat("Current year:", current_year, "\n")
cat("Last month data:", last_month_data, "\n")
cat("Last month date:", last_month_date, "\n\n")

# Check these countries for months that should be estimated
diagnostic <- data %>%
  dplyr::filter(iso3 %in% problem_countries) %>%
  dplyr::arrange(iso3, Year, Month) %>%
  dplyr::group_by(iso3, Year) %>%
  dplyr::mutate(
    group_predicted_total = dplyr::first(Predicted_total_seasonal_cases[!is.na(Predicted_total_seasonal_cases)])
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    # Check each condition
    has_cases = !is.na(cases),
    has_date = !is.na(date),
    date_is_recent = date <= last_month_date,
    has_predicted_total = !is.na(group_predicted_total),
    has_proportion = !is.na(Ave_monthly_proportion),
    
    # Should this row get an estimate?
    should_estimate = is.na(cases) & 
      has_date & 
      date_is_recent & 
      has_predicted_total & 
      has_proportion,
    
    # Why isn't it being estimated?
    reason_no_estimate = dplyr::case_when(
      !is.na(cases) ~ "Has observed data",
      is.na(date) ~ "Missing date",
      !date_is_recent ~ paste("Date", date, ">", last_month_date),
      is.na(group_predicted_total) ~ "Missing predicted total",
      is.na(Ave_monthly_proportion) ~ "Missing Ave_monthly_proportion",
      TRUE ~ "Should estimate"
    )
  ) %>%
  dplyr::filter(
    # Focus on months that should be estimated but aren't
    (should_estimate | (!is.na(cases) & Year >= 2025 & Month >= 10))
  ) %>%
  dplyr::select(
    country, iso3, Year, Month, date, season_nMonth, season,
    cases, source,
    group_predicted_total, Ave_monthly_proportion,
    should_estimate, reason_no_estimate,
    has_date, date_is_recent, has_predicted_total, has_proportion
  )

# View the diagnostic
print(diagnostic, n = 100)

# Summary by country
diagnostic_summary <- diagnostic %>%
  dplyr::filter(Year >= 2025, Month >= 10) %>%
  dplyr::group_by(country, iso3) %>%
  dplyr::summarise(
    n_rows = n(),
    n_with_cases = sum(!is.na(cases)),
    n_should_estimate = sum(should_estimate, na.rm = TRUE),
    n_missing_date = sum(is.na(date)),
    n_date_too_future = sum(!is.na(date) & !date_is_recent),
    n_missing_predicted_total = sum(is.na(group_predicted_total)),
    n_missing_proportion = sum(is.na(Ave_monthly_proportion)),
    months_missing_proportion = paste(Month[is.na(Ave_monthly_proportion) & Year >= 2025], collapse = ", "),
    .groups = "drop"
  )

print(diagnostic_summary)




# 1) Proportion check (as before)
proportion_check <- data %>%
  dplyr::group_by(country, iso3) %>%
  dplyr::summarise(
    n_season_months_with_cum_prop = length(unique(season_nMonth[!is.na(Ave_cum_monthly_proportion)])),
    n_season_months_with_mon_prop = length(unique(season_nMonth[!is.na(Ave_monthly_proportion)])),
    has_all_cum_prop = n_season_months_with_cum_prop == 12,
    has_all_mon_prop = n_season_months_with_mon_prop == 12,
    missing_cum_prop_months = if (n_season_months_with_cum_prop < 12) {
      all_months <- 1:12
      months_with_prop <- unique(season_nMonth[!is.na(Ave_cum_monthly_proportion)])
      missing <- setdiff(all_months, months_with_prop)
      paste(missing, collapse = ", ")
    } else {
      "None"
    },
    missing_mon_prop_months = if (n_season_months_with_mon_prop < 12) {
      all_months <- 1:12
      months_with_prop <- unique(season_nMonth[!is.na(Ave_monthly_proportion)])
      missing <- setdiff(all_months, months_with_prop)
      paste(missing, collapse = ", ")
    } else {
      "None"
    },
    .groups = "drop"
  ) %>%
  dplyr::arrange(country)

# 2) Get country tracking (if pipeline was run in this session)
tracking_df <- NULL
if (exists("get_tracking_df")) {
  tracking_df <- get_tracking_df()
}

# 3) If tracking exists, join and add seasonal-profile / exclusion info
if (!is.null(tracking_df) && is.data.frame(tracking_df) && nrow(tracking_df) > 0) {
  # Select tracking columns relevant to seasonal profile
  tracking_sub <- tracking_df %>%
    dplyr::select(
      iso3,
      # Present at seasonal steps
      step_3c_seasonal_before_filter,
      step_3c_seasonal_after_filter,
      # Why excluded (if any)
      dropped_at_step,
      drop_reason
    ) %>%
    dplyr::distinct(iso3, .keep_all = TRUE)
  
  proportion_check <- proportion_check %>%
    dplyr::left_join(tracking_sub, by = "iso3") %>%
    dplyr::mutate(
      # Interpret: in seasonal profile = present at Step_3c_After_Filter
      in_seasonal_profile = dplyr::coalesce(step_3c_seasonal_after_filter, FALSE),
      # Excluded from seasonal profile = was at Before but not at After
      excluded_from_seasonal_profile = dplyr::coalesce(step_3c_seasonal_before_filter, FALSE) &
        !dplyr::coalesce(step_3c_seasonal_after_filter, FALSE),
      # Human-readable reason (prefer drop_reason, else inferred)
      seasonal_exclusion_reason = dplyr::case_when(
        excluded_from_seasonal_profile & !is.na(drop_reason) ~ drop_reason,
        excluded_from_seasonal_profile & !is.na(dropped_at_step) ~ paste0("Dropped at: ", dropped_at_step),
        excluded_from_seasonal_profile ~ "Dropped during seasonal filtering (reason not recorded)",
        !dplyr::coalesce(step_3c_seasonal_before_filter, FALSE) ~ "Never reached seasonal filtering",
        TRUE ~ NA_character_
      )
    )
} else {
  # No tracking: add placeholder columns
  proportion_check <- proportion_check %>%
    dplyr::mutate(
      step_3c_seasonal_before_filter = NA,
      step_3c_seasonal_after_filter = NA,
      dropped_at_step = NA_character_,
      drop_reason = NA_character_,
      in_seasonal_profile = NA,
      excluded_from_seasonal_profile = NA,
      seasonal_exclusion_reason = NA_character_
    )
}

# 4) View
print(proportion_check)

# 5) Countries missing proportions and/or excluded from seasonal profile
countries_issues <- proportion_check %>%
  dplyr::filter(!has_all_cum_prop | !has_all_mon_prop | excluded_from_seasonal_profile) %>%
  dplyr::select(
    country, iso3,
    has_all_cum_prop, has_all_mon_prop,
    missing_cum_prop_months, missing_mon_prop_months,
    in_seasonal_profile, excluded_from_seasonal_profile,
    seasonal_exclusion_reason, drop_reason, dropped_at_step
  )

print(countries_issues)





# Diagnostic: Check why predicted totals are NA for problem countries
problem_countries <- c("ATG", "CIV", "GUY", "MYS", "FSM", "REU", "KNA", "TTO", "TCA", "VGB", "WLF")

# Check predicted total calculation step by step
predicted_total_diagnostic <- data %>%
  dplyr::filter(iso3 %in% problem_countries) %>%
  dplyr::arrange(iso3, season, season_nMonth) %>%
  dplyr::group_by(iso3, season) %>%
  dplyr::mutate(
    # Check if there are any cases in this season
    has_any_cases = any(!is.na(cases)),
    
    # Count rows with cases in this season
    n_rows_with_cases = sum(!is.na(cases)),
    
    # Check if cum_todate_cases_season exists
    has_cum_season = !is.na(cum_todate_cases_season),
    
    # Check if Ave_cum_monthly_proportion exists
    has_cum_prop = !is.na(Ave_cum_monthly_proportion)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(iso3, season) %>%
  dplyr::mutate(
    # Find last observed index (only calculate if has_any_cases is TRUE for this group)
    last_obs_idx = if (dplyr::first(has_any_cases)) {
      max(which(!is.na(cases)))
    } else {
      NA_integer_
    },
    
    # Check what values we'd use
    last_cum_cases_check = if (!is.na(dplyr::first(last_obs_idx))) {
      cum_todate_cases_season[dplyr::first(last_obs_idx)]
    } else {
      NA_real_
    },
    
    last_cum_prop_check = if (!is.na(dplyr::first(last_obs_idx))) {
      Ave_cum_monthly_proportion[dplyr::first(last_obs_idx)]
    } else {
      NA_real_
    },
    
    # Check if predicted total would be calculated
    would_calculate = !is.na(dplyr::first(last_cum_cases_check)) & 
      !is.na(dplyr::first(last_cum_prop_check)) & 
      dplyr::first(last_cum_prop_check) > 0
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    # Show rows for the most recent seasons with data
    season %in% c("2024/2025", "2025/2026")
  ) %>%
  dplyr::select(
    country, iso3, season, Year, Month, season_nMonth,
    cases, cum_todate_cases_season, Ave_cum_monthly_proportion,
    has_any_cases, last_obs_idx, last_cum_cases_check, last_cum_prop_check,
    would_calculate, n_rows_with_cases, has_cum_season, has_cum_prop,
    Predicted_total_seasonal_cases
  ) %>%
  dplyr::arrange(iso3, season, season_nMonth)

# View the diagnostic
print(predicted_total_diagnostic, n = 100)

# Summary by country-season (simpler approach)
predicted_summary <- data %>%
  dplyr::filter(iso3 %in% problem_countries, season %in% c("2024/2025", "2025/2026")) %>%
  dplyr::group_by(country, iso3, season) %>%
  dplyr::summarise(
    n_rows = n(),
    n_with_cases = sum(!is.na(cases)),
    has_any_cases = any(!is.na(cases)),
    last_month_with_cases = if (any(!is.na(cases))) {
      Month[max(which(!is.na(cases)))]
    } else {
      NA_integer_
    },
    last_season_nMonth_with_cases = if (any(!is.na(cases))) {
      season_nMonth[max(which(!is.na(cases)))]
    } else {
      NA_real_
    },
    cum_at_last_obs = if (any(!is.na(cases))) {
      cum_todate_cases_season[max(which(!is.na(cases)))]
    } else {
      NA_real_
    },
    prop_at_last_obs = if (any(!is.na(cases))) {
      Ave_cum_monthly_proportion[max(which(!is.na(cases)))]
    } else {
      NA_real_
    },
    predicted_total = dplyr::first(Predicted_total_seasonal_cases),
    n_rows_missing_cum_season = sum(is.na(cum_todate_cases_season)),
    n_rows_missing_cum_prop = sum(is.na(Ave_cum_monthly_proportion)),
    .groups = "drop"
  )

print(predicted_summary)
