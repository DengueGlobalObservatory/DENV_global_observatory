# Legacy validation functions (full moving-window + NB + snapshot helpers).
# Used only by Scripts/validation/04_nowcast_validation_BRA_test.R.
# Main validation workflow uses Scripts/validation/FUNCTIONS/00_FUN_validation_metrics.R instead.

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(readr)
library(stringr)
library(lubridate)
library(MASS)
library(sf)
library(rnaturalearth)

###-----------------###
safe_divide <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}

###-----------------###
prepare_validation_dataset <- function() {
# check for related pipeline log in action 
  if (!exists("log_message")) {
    source("Scripts/utils/logging.R")
    ensure_logger(console = TRUE)
  }
# load scripts
## if the pipeline has fully run this may not be needed
  source("Scripts/data_sourcing/01_dengue_data.R")
  source("Scripts/data_sourcing/01_select_historic_data.R")
  source("Scripts/seasonal_baseline/02_identify_seasonal_baseline.R")

# check if needed file is in environment 
  if (!exists("full_data_season_monthly_proportions")) {
    stop("Expected object `full_data_season_monthly_proportions` not found.")
  }
  
# extract only the needed variable from the full_data to the validation set
  validation_data <- full_data_season_monthly_proportions %>%
    dplyr::select(
      country, iso3, Year, season, season_nMonth, Month, cases,
      Actual_monthly_proportion, Actual_cum_monthly_proportion
    ) %>%
    dplyr::arrange(iso3, season, season_nMonth)

# create country metadata particularly region   
  country_meta <- OD_national %>%
    dplyr::transmute(
      iso3 = ISO_A0,
      country = stringr::str_to_title(adm_0_name),
      Region = od_region
    ) %>%
    dplyr::distinct(iso3, .keep_all = TRUE)

# Join validation data and country metadata
  validation_data <- validation_data %>%
    dplyr::left_join(country_meta, by = "iso3") %>%
    dplyr::mutate(country = dplyr::coalesce(country.x, country.y)) %>%
    dplyr::select(-country.x, -country.y)

# return a list of the validation and full data  
  list(
    validation_data = validation_data,
    full_data = full_data_season_monthly_proportions
  )
}

###-----------------###
fit_baseline_profile <- function(train_df) {
# calculate the mean monthly proportion ( i.e seasonal baseline) 
# does not handle the country grouping within the function 
  train_df %>%
    dplyr::group_by(season_nMonth) %>%
    dplyr::summarise(
      Ave_monthly_proportion = mean(Actual_monthly_proportion, na.rm = TRUE),
      Ave_cum_monthly_proportion = mean(Actual_cum_monthly_proportion, na.rm = TRUE),
      .groups = "drop"
    ) %>%
  # maintain seasonal prospective 
    dplyr::arrange(season_nMonth)
}

###-----------------###
fit_nb_month_distribution <- function(train_df) {
  # Fast moment-based NB approximation (avoids repeated glm.nb fits).
  # For var <= mean we fall back to a near-Poisson NB (large size).
  train_df %>%
    dplyr::group_by(season_nMonth) %>%
    dplyr::summarise(
      nb_mu = {
        x <- cases[!is.na(cases) & cases >= 0]
        if (length(x) < 3 || sum(x) == 0) NA_real_ else mean(x)
      },
      nb_var = {
        x <- cases[!is.na(cases) & cases >= 0]
        if (length(x) < 3 || sum(x) == 0) NA_real_ else stats::var(x)
      },
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      nb_size = dplyr::case_when(
        is.na(nb_mu) | is.na(nb_var) ~ NA_real_,
        nb_mu <= 0 ~ NA_real_,
        nb_var <= nb_mu ~ 1e6,
        TRUE ~ (nb_mu^2) / (nb_var - nb_mu)
      )
    ) %>%
    dplyr::select(-nb_var)
}

###-----------------###
compute_nowcast_at_cutoff <- function(country_df, test_season, cutoff_month) {
# defined test df from test season 
  test_df <- country_df %>% dplyr::filter(season == test_season)
# define train df - exclude test season 
  train_df <- country_df %>% dplyr::filter(season != test_season)

# fit baseline on train data
  baseline <- fit_baseline_profile(train_df)
# fit distribution on train data
  nb_dist <- fit_nb_month_distribution(train_df)

# from the test data create the cutoff data
  ## this simulates the reality of predicting months ay different time of the season 
  observed <- test_df %>% dplyr::filter(season_nMonth <= cutoff_month)
  cum_observed <- sum(observed$cases, na.rm = TRUE)
  cum_prop_at_cutoff <- baseline %>%
    dplyr::filter(season_nMonth == cutoff_month) %>%
    dplyr::pull(Ave_cum_monthly_proportion)

  
  if (length(cum_prop_at_cutoff) == 0 || is.na(cum_prop_at_cutoff) || cum_prop_at_cutoff <= 0) {
    return(tibble::tibble())
  }

  # predict the seasonal total given the cumulative total at cutoff
  predicted_total <- round(cum_observed / cum_prop_at_cutoff, 0)

  # prediction for all season_months after the cutoff month
  pred_df <- baseline %>%
    dplyr::filter(season_nMonth > cutoff_month) %>%
    dplyr::left_join(test_df %>% dplyr::select(season_nMonth, Month, actual_cases = cases), by = "season_nMonth") %>%
    dplyr::left_join(nb_dist, by = "season_nMonth") %>%
    dplyr::mutate(
      predicted_total = predicted_total,
      predicted_cases = round(predicted_total * Ave_monthly_proportion, 0),
      cutoff_month = cutoff_month,
      horizon = season_nMonth - cutoff_month,
      # 
      lower_95_nb = ifelse(!is.na(nb_size) & !is.na(nb_mu), qnbinom(0.025, size = nb_size, mu = nb_mu), NA_real_),
      upper_95_nb = ifelse(!is.na(nb_size) & !is.na(nb_mu), qnbinom(0.975, size = nb_size, mu = nb_mu), NA_real_),
      lower_50_nb = ifelse(!is.na(nb_size) & !is.na(nb_mu), qnbinom(0.25, size = nb_size, mu = nb_mu), NA_real_),
      upper_50_nb = ifelse(!is.na(nb_size) & !is.na(nb_mu), qnbinom(0.75, size = nb_size, mu = nb_mu), NA_real_),
      logscore = ifelse(
        !is.na(nb_size) & !is.na(nb_mu) & !is.na(actual_cases),
        dnbinom(actual_cases, size = nb_size, mu = nb_mu, log = TRUE),
        NA_real_
      )
    )

  pred_df
}

###-----------------###
run_moving_window_validation <- function(validation_data) {
  # metadata ids
  work_ids <- validation_data %>%
    dplyr::distinct(iso3, country, season, Region)
  
  all_results <- purrr::map_dfr(seq_len(nrow(work_ids)), function(i) { # use map_drf to apply function by metadata groups
    id <- work_ids[i, ]
    country_df <- validation_data %>% dplyr::filter(iso3 == id$iso3)
    seasons <- sort(unique(country_df$season))
    if (length(seasons) < 3) { # if there are lesson that 3 seasons return blank tbl
      return(tibble::tibble())
    }
    # use seasons to split training and test data
    purrr::map_dfr(seasons, function(s) {
      test_df <- country_df %>% dplyr::filter(season == s)
      train_df <- country_df %>% dplyr::filter(season != s)
      if (n_distinct(train_df$season) < 2) {
        return(tibble::tibble())
      }
      # define baseline
      baseline <- fit_baseline_profile(train_df)
      nb_dist <- fit_nb_month_distribution(train_df)
      # loop through cutoff months 
      purrr::map_dfr(1:11, function(cutoff) {
        observed <- test_df %>% dplyr::filter(season_nMonth <= cutoff)
        cum_observed <- sum(observed$cases, na.rm = TRUE)
        cum_prop_at_cutoff <- baseline %>%
          dplyr::filter(season_nMonth == cutoff) %>%
          dplyr::pull(Ave_cum_monthly_proportion)
        if (length(cum_prop_at_cutoff) == 0 || is.na(cum_prop_at_cutoff) || cum_prop_at_cutoff <= 0) {
          return(tibble::tibble())
        }
      # predict the total   
        predicted_total <- round(cum_observed / cum_prop_at_cutoff, 0)
      # use baseline to calculate the predicted months after the cutoff
        baseline %>%
          dplyr::filter(season_nMonth > cutoff) %>%
          dplyr::left_join(test_df %>% dplyr::select(season_nMonth, Month, actual_cases = cases), by = "season_nMonth") %>%
          dplyr::left_join(nb_dist, by = "season_nMonth") %>%
          dplyr::mutate(
            predicted_total = predicted_total,
            predicted_cases = round(predicted_total * Ave_monthly_proportion, 0),
            cutoff_month = cutoff,
            horizon = season_nMonth - cutoff,
            lower_95_nb = ifelse(!is.na(nb_size) & !is.na(nb_mu), qnbinom(0.025, size = nb_size, mu = nb_mu), NA_real_),
            upper_95_nb = ifelse(!is.na(nb_size) & !is.na(nb_mu), qnbinom(0.975, size = nb_size, mu = nb_mu), NA_real_),
            lower_50_nb = ifelse(!is.na(nb_size) & !is.na(nb_mu), qnbinom(0.25, size = nb_size, mu = nb_mu), NA_real_),
            upper_50_nb = ifelse(!is.na(nb_size) & !is.na(nb_mu), qnbinom(0.75, size = nb_size, mu = nb_mu), NA_real_),
            logscore = ifelse(!is.na(nb_size) & !is.na(nb_mu) & !is.na(actual_cases), dnbinom(actual_cases, size = nb_size, mu = nb_mu, log = TRUE), NA_real_),
            iso3 = id$iso3,
            country = id$country,
            Region = id$Region,
            season = s
          )
      })
    })
  })
# calculate result error values 
  all_results %>%
    dplyr::mutate(
      absolute_error = predicted_cases - actual_cases,
      abs_error = abs(absolute_error),
      sq_error = absolute_error^2,
      ape = ifelse(actual_cases > 0, abs((predicted_cases - actual_cases) / actual_cases), NA_real_),
      relative_error = ifelse(actual_cases > 0, (predicted_cases - actual_cases) / actual_cases, NA_real_)
    )
}

###-----------------###
compute_error_metrics <- function(validation_results) {
  # result are grouped by country
  by_country <- validation_results %>%
    dplyr::group_by(iso3, country, Region) %>% # country grouping
    # summarise for results 
    dplyr::summarise(
      n_predictions = n(),
      MAE = mean(abs_error, na.rm = TRUE),
      RMSE = sqrt(mean(sq_error, na.rm = TRUE)),
      RMSPE = sqrt(mean(relative_error^2, na.rm = TRUE)),
      coverage_95 = mean(actual_cases >= lower_95_nb & actual_cases <= upper_95_nb, na.rm = TRUE),
      coverage_50 = mean(actual_cases >= lower_50_nb & actual_cases <= upper_50_nb, na.rm = TRUE),
      logscore = mean(logscore, na.rm = TRUE),
      n_seasons = n_distinct(season),
      .groups = "drop"
    )
  # results by cutoff
  by_cutoff <- validation_results %>%
    dplyr::group_by(cutoff_month) %>%
    dplyr::summarise(
      n_country_seasons = n_distinct(paste(iso3, season)),
      median_MAE = median(abs_error, na.rm = TRUE),
      median_RMSE = sqrt(median(sq_error, na.rm = TRUE)),
      median_RMSPE = median(ape, na.rm = TRUE),
      coverage_95 = mean(actual_cases >= lower_95_nb & actual_cases <= upper_95_nb, na.rm = TRUE),
      coverage_50 = mean(actual_cases >= lower_50_nb & actual_cases <= upper_50_nb, na.rm = TRUE),
      .groups = "drop"
    )
  # results by country and cutoff --- this is probably the most important for uncertainty 
  by_country_cutoff <- validation_results %>%
    dplyr::group_by(iso3, country, Region, cutoff_month) %>%
    dplyr::summarise(
      n_predictions = n(),
      MAE = mean(abs_error, na.rm = TRUE),
      RMSE = sqrt(mean(sq_error, na.rm = TRUE)),
      RMSPE = sqrt(mean(relative_error^2, na.rm = TRUE)),
      coverage_95 = mean(actual_cases >= lower_95_nb & actual_cases <= upper_95_nb, na.rm = TRUE),
      coverage_50 = mean(actual_cases >= lower_50_nb & actual_cases <= upper_50_nb, na.rm = TRUE),
      logscore = mean(logscore, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    by_country = by_country,
    by_cutoff = by_cutoff,
    by_country_cutoff = by_country_cutoff
  )
}

classify_performance <- function(country_metrics, validation_data) {
  tertiles <- quantile(country_metrics$RMSPE, probs = c(1 / 3, 2 / 3), na.rm = TRUE)

  profile_cv <- validation_data %>%
    dplyr::group_by(iso3, season_nMonth) %>%
    dplyr::summarise(avg_prop = mean(Actual_monthly_proportion, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(iso3) %>%
    dplyr::summarise(
      seasonal_profile_cv = sd(avg_prop, na.rm = TRUE) / mean(avg_prop, na.rm = TRUE),
      .groups = "drop"
    )

  burden <- validation_data %>%
    dplyr::group_by(iso3) %>%
    dplyr::summarise(mean_monthly_cases = mean(cases, na.rm = TRUE), .groups = "drop")

  country_metrics %>%
    dplyr::left_join(profile_cv, by = "iso3") %>%
    dplyr::left_join(burden, by = "iso3") %>%
    dplyr::mutate(
      burden_group = ifelse(mean_monthly_cases >= median(mean_monthly_cases, na.rm = TRUE), "High", "Low"),
      performance_tier = dplyr::case_when(
        RMSPE <= tertiles[1] ~ "Good",
        RMSPE <= tertiles[2] ~ "Moderate",
        TRUE ~ "Poor"
      )
    )
}

season_month_accuracy_analysis <- function(by_cutoff, threshold = 0.25) {
  reliable_horizon <- by_cutoff %>%
    dplyr::arrange(cutoff_month) %>%
    dplyr::filter(median_RMSPE < threshold) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::pull(cutoff_month)

  list(
    by_cutoff = by_cutoff,
    reliable_horizon = ifelse(length(reliable_horizon) == 0, NA_integer_, reliable_horizon)
  )
}

build_prediction_interval_lookup <- function(validation_results, min_obs = 5) {
  country_lookup <- validation_results %>%
    dplyr::filter(is.finite(relative_error)) %>%
    dplyr::group_by(iso3, Region, cutoff_month, horizon) %>%
    dplyr::summarise(
      n_obs = n(),
      q025_rel = quantile(relative_error, 0.025, na.rm = TRUE),
      q25_rel = quantile(relative_error, 0.25, na.rm = TRUE),
      q75_rel = quantile(relative_error, 0.75, na.rm = TRUE),
      q975_rel = quantile(relative_error, 0.975, na.rm = TRUE),
      .groups = "drop"
    )

  region_lookup <- validation_results %>%
    dplyr::filter(is.finite(relative_error)) %>%
    dplyr::group_by(Region, cutoff_month, horizon) %>%
    dplyr::summarise(
      n_obs_region = n(),
      rq025_rel = quantile(relative_error, 0.025, na.rm = TRUE),
      rq25_rel = quantile(relative_error, 0.25, na.rm = TRUE),
      rq75_rel = quantile(relative_error, 0.75, na.rm = TRUE),
      rq975_rel = quantile(relative_error, 0.975, na.rm = TRUE),
      .groups = "drop"
    )

  global_lookup <- validation_results %>%
    dplyr::filter(is.finite(relative_error)) %>%
    dplyr::group_by(cutoff_month, horizon) %>%
    dplyr::summarise(
      n_obs_global = n(),
      gq025_rel = quantile(relative_error, 0.025, na.rm = TRUE),
      gq25_rel = quantile(relative_error, 0.25, na.rm = TRUE),
      gq75_rel = quantile(relative_error, 0.75, na.rm = TRUE),
      gq975_rel = quantile(relative_error, 0.975, na.rm = TRUE),
      .groups = "drop"
    )

  country_lookup %>%
    dplyr::left_join(region_lookup, by = c("Region", "cutoff_month", "horizon")) %>%
    dplyr::left_join(global_lookup, by = c("cutoff_month", "horizon")) %>%
    dplyr::mutate(
      q025_rel = ifelse(n_obs >= min_obs, q025_rel, dplyr::coalesce(rq025_rel, gq025_rel)),
      q25_rel = ifelse(n_obs >= min_obs, q25_rel, dplyr::coalesce(rq25_rel, gq25_rel)),
      q75_rel = ifelse(n_obs >= min_obs, q75_rel, dplyr::coalesce(rq75_rel, gq75_rel)),
      q975_rel = ifelse(n_obs >= min_obs, q975_rel, dplyr::coalesce(rq975_rel, gq975_rel))
    ) %>%
    dplyr::select(iso3, cutoff_month, horizon, n_obs, q025_rel, q25_rel, q75_rel, q975_rel) %>%
    dplyr::arrange(iso3, cutoff_month, horizon)
}

apply_prediction_intervals <- function(df, lookup) {
  df %>%
    dplyr::left_join(lookup, by = c("iso3", "cutoff_month", "horizon")) %>%
    dplyr::mutate(
      lower_95 = pmax(0, predicted_cases * (1 + q025_rel)),
      upper_95 = pmax(0, predicted_cases * (1 + q975_rel)),
      lower_50 = pmax(0, predicted_cases * (1 + q25_rel)),
      upper_50 = pmax(0, predicted_cases * (1 + q75_rel))
    )
}

coverage_calibration <- function(validation_results, lookup) {
  calibrated <- apply_prediction_intervals(validation_results, lookup)
  cov95 <- mean(calibrated$actual_cases >= calibrated$lower_95 & calibrated$actual_cases <= calibrated$upper_95, na.rm = TRUE)
  cov50 <- mean(calibrated$actual_cases >= calibrated$lower_50 & calibrated$actual_cases <= calibrated$upper_50, na.rm = TRUE)
  tibble::tibble(interval = c("95", "50"), nominal = c(0.95, 0.50), empirical = c(cov95, cov50))
}

load_snapshot_nowcast <- function(snapshot_dir) {
  f <- file.path(snapshot_dir, "DENV_cases_nowcast_output.csv")
  if (!file.exists(f)) {
    return(tibble::tibble())
  }
  readr::read_csv(f, show_col_types = FALSE) %>%
    dplyr::filter(Data_status == "Unobserved", source == "Estimates") %>%
    dplyr::transmute(
      snapshot_date = basename(snapshot_dir),
      iso3, country, Year, Month, cases_nowcast = cases
    )
}

compute_snapshot_convergence <- function(output_root = "Output") {
  snapshot_dirs <- list.dirs(output_root, full.names = TRUE, recursive = FALSE)
  snapshot_dirs <- snapshot_dirs[str_detect(basename(snapshot_dirs), "^\\d{4}_\\d{2}_\\d{2}$")]
  snapshot_dirs <- sort(snapshot_dirs)

  snap_df <- purrr::map_dfr(snapshot_dirs, load_snapshot_nowcast)
  if (nrow(snap_df) == 0) {
    return(list(detail = tibble::tibble(), summary = tibble::tibble()))
  }

  latest <- snap_df %>%
    dplyr::group_by(iso3, Year, Month) %>%
    dplyr::slice_max(order_by = snapshot_date, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::rename(final_nowcast = cases_nowcast)

  detail <- snap_df %>%
    dplyr::left_join(latest %>% dplyr::select(iso3, Year, Month, final_nowcast), by = c("iso3", "Year", "Month")) %>%
    dplyr::mutate(
      abs_diff_to_final = abs(cases_nowcast - final_nowcast),
      pct_diff_to_final = safe_divide(cases_nowcast - final_nowcast, final_nowcast)
    )

  summary <- detail %>%
    dplyr::group_by(iso3, country, Year, Month) %>%
    dplyr::arrange(snapshot_date, .by_group = TRUE) %>%
    dplyr::summarise(
      n_snapshots = n(),
      first_snapshot = first(snapshot_date),
      last_snapshot = last(snapshot_date),
      final_nowcast = dplyr::last(final_nowcast),
      first_abs_diff = first(abs_diff_to_final),
      last_abs_diff = last(abs_diff_to_final),
      stabilized_snapshot = dplyr::first(snapshot_date[abs_diff_to_final <= 1]),
      .groups = "drop"
    )

  list(detail = detail, summary = summary)
}

compute_backfill_assessment <- function(output_root = "Output", paho_iso3 = NULL) {
  snapshot_dirs <- list.dirs(output_root, full.names = TRUE, recursive = FALSE)
  snapshot_dirs <- snapshot_dirs[str_detect(basename(snapshot_dirs), "^\\d{4}_\\d{2}_\\d{2}$")]
  snapshot_dirs <- sort(snapshot_dirs)

  backfill <- purrr::map_dfr(snapshot_dirs, function(d) {
    f <- file.path(d, "DENV_cases_backfill_output.csv")
    if (!file.exists(f)) {
      return(tibble::tibble())
    }
    readr::read_csv(f, show_col_types = FALSE) %>%
      dplyr::transmute(snapshot_date = basename(d), iso3, country, Year, Month, cases, source)
  })

  if (nrow(backfill) == 0) {
    return(tibble::tibble())
  }

  if (is.null(paho_iso3)) {
    paho_iso3 <- backfill %>% dplyr::filter(source == "PAHO") %>% dplyr::pull(iso3) %>% unique()
  }

  latest_truth <- backfill %>%
    dplyr::group_by(iso3, Year, Month) %>%
    dplyr::slice_max(order_by = snapshot_date, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::rename(final_cases = cases)

  backfill %>%
    dplyr::filter(iso3 %in% paho_iso3) %>%
    dplyr::left_join(latest_truth %>% dplyr::select(iso3, Year, Month, final_cases), by = c("iso3", "Year", "Month")) %>%
    dplyr::mutate(abs_err = abs(cases - final_cases)) %>%
    dplyr::group_by(source) %>%
    dplyr::summarise(
      n = n(),
      MAE_vs_final = mean(abs_err, na.rm = TRUE),
      RMSE_vs_final = sqrt(mean((cases - final_cases)^2, na.rm = TRUE)),
      .groups = "drop"
    )
}

build_world_sf <- function() {
  rnaturalearth::ne_countries(scale = 50, type = "countries", returnclass = "sf") %>%
    dplyr::select(iso_a3, geometry)
}
