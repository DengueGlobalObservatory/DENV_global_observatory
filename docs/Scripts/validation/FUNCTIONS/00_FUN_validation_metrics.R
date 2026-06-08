#' Validation helpers — proportion-based LOSO nowcast
#'
#' LOSO nowcast helpers plus row-level error columns. Summaries, quantiles, and
#' figures live in the action scripts.

library(dplyr)

#' Mean observed cases by season month (burden reference for unitless metrics)
#'
#' Used as the denominator for uMAE / uRMSE / uMAPE: errors are scaled by the
#' average actual monthly burden in the month being predicted (`prediction_month`
#' / `season_nMonth`), not by the row's own `actual_cases`.
#'
#' @param df Long validation table with `season_nMonth` and `cases` or
#'   `actual_cases`.
#' @param month_col Column identifying the predicted calendar position (default
#'   `season_nMonth`).
#' @param cases_col Observed monthly cases (default `cases`; use `actual_cases`
#'   on prediction-row tables).
#' @param by_country When TRUE, compute means within each `iso3` × month
#'   stratum (country- and month-dependent burden reference).
#' @return Tibble with `prediction_month` and `mean_actual_predicted_month`
#'   (and `iso3` when `by_country = TRUE`).
mean_actual_by_prediction_month <- function(df,
                                            month_col = "season_nMonth",
                                            cases_col = "cases",
                                            by_country = FALSE) {
  group_cols <- if (isTRUE(by_country)) {
    c("iso3", month_col)
  } else {
    month_col
  }

  out <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      mean_actual_predicted_month = mean(.data[[cases_col]], na.rm = TRUE),
      .groups = "drop"
    )
  names(out)[names(out) == month_col] <- "prediction_month"
  out
}

#' Add row-level error components and burden-normalized (u*) columns
#'
#' Standard metrics use raw case errors. Unitless metrics divide by
#' `mean_actual_predicted_month` for the target season month:
#'   - uMAE  = mean(|error| / mu)
#'   - uRMSE = sqrt(mean(error^2 / mu^2)) = RMSE / mu
#'   - uMAPE = mean(|error| / mu)  (burden-normalized; differs from MAPE when
#'     actual != mu)
#'
#' @param detail Prediction-level tibble with `predicted_cases`, `actual_cases`,
#'   `prediction_month`.
#' @param burden_ref Optional output of [mean_actual_by_prediction_month()];
#'   computed from `detail` when NULL.
#' @param by_country When TRUE (default), join burden by `iso3` and
#'   `prediction_month`; when FALSE, by `prediction_month` only.
#' @return `detail` with error columns joined.
add_validation_error_columns <- function(detail,
                                         burden_ref = NULL,
                                         by_country = TRUE) {
  if (is.null(burden_ref)) {
    burden_ref <- mean_actual_by_prediction_month(
      detail,
      month_col = "prediction_month",
      cases_col = "actual_cases",
      by_country = by_country
    )
  }

  join_by <- if (isTRUE(by_country) && "iso3" %in% names(burden_ref)) {
    c("iso3", "prediction_month")
  } else {
    "prediction_month"
  }

  detail %>%
    dplyr::left_join(burden_ref, by = join_by) %>%
    dplyr::mutate(
      absolute_error = predicted_cases - actual_cases,
      squared_error = absolute_error^2,
      relative_error = dplyr::if_else(
        actual_cases > 0,
        absolute_error / actual_cases,
        NA_real_
      ),
      absolute_percent_error = dplyr::if_else(
        actual_cases > 0,
        abs(absolute_error) / actual_cases,
        NA_real_
      ),
      scaled_absolute_error = dplyr::if_else(
        mean_actual_predicted_month > 0,
        abs(absolute_error) / mean_actual_predicted_month,
        NA_real_
      ),
      scaled_absolute_percent_error = dplyr::if_else(
        mean_actual_predicted_month > 0,
        abs(absolute_error) / mean_actual_predicted_month,
        NA_real_
      ),
      scaled_squared_error = dplyr::if_else(
        mean_actual_predicted_month > 0,
        squared_error / mean_actual_predicted_month^2,
        NA_real_
      ),
      unitless_percent_error = dplyr::if_else(
        mean_actual_predicted_month > 0,
        abs(absolute_error) / mean_actual_predicted_month,
        NA_real_
      )
    )
}

#' Mean monthly and cumulative seasonal proportions by `season_nMonth`
#'
#' @param train_df Training rows for one country (multiple seasons); must
#'   contain `season_nMonth`, `Actual_monthly_proportion`,
#'   `Actual_cum_monthly_proportion`.
#' @return One row per `season_nMonth`, sorted 1..12.
fit_baseline_profile <- function(train_df) {
  train_df %>%
    dplyr::group_by(season_nMonth) %>%
    dplyr::summarise(
      # Average share of the season’s total cases in this calendar position
      Ave_monthly_proportion = mean(Actual_monthly_proportion, na.rm = TRUE),
      # Average cumulative share up to and including this season month
      Ave_cum_monthly_proportion = mean(Actual_cum_monthly_proportion, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(season_nMonth)
}

#' Point nowcast for months strictly after cutoff (one test season, one k)
#'
#' @param test_df Rows for a single held-out `season` (12 months).
#' @param baseline Output of [fit_baseline_profile()] on training seasons.
#' @param cutoff_k Last observed season month (1..11); months > k are predicted.
#' @return Tibble with one row per future month; empty if baseline cumulative
#'   proportion at k is unusable.
nowcast_one_cutoff <- function(test_df, baseline, cutoff_k) {
  # Cumulative observed cases in the test season up to and including month k
  C_le_k <- test_df %>%
    dplyr::filter(season_nMonth <= cutoff_k) %>%
    dplyr::pull(cases) %>%
    sum(na.rm = TRUE)

  # Mean cumulative proportion at k from training seasons (denominator for total-season estimate)
  P_le_k <- baseline %>%
    dplyr::filter(season_nMonth == cutoff_k) %>%
    dplyr::pull(Ave_cum_monthly_proportion)

  if (length(P_le_k) == 0 || is.na(P_le_k) || P_le_k <= 0) {
    return(tibble::tibble())
  }

  # Methods: predicted seasonal total = observed cumulative / mean cumulative proportion at k
  predicted_total <- C_le_k / P_le_k

  baseline %>%
    dplyr::filter(season_nMonth > cutoff_k) %>%
    dplyr::left_join(
      test_df %>% dplyr::select(season_nMonth, Month, actual_cases = cases),
      by = "season_nMonth"
    ) %>%
    dplyr::transmute(
      cutoff_month = cutoff_k,
      prediction_month = season_nMonth,
      Month,
      predicted_total = predicted_total,
      # Allocate total to future months using mean monthly proportions (methods)
      predicted_cases = round(predicted_total * Ave_monthly_proportion, 0),
      actual_cases
    )
}

# =============================================================================
# GAM prototype helpers
# =============================================================================
# These functions support the direct negative-binomial GAM nowcast prototype
# implemented in 03_GAMnowcast_validation_ind.R. They mirror the leave-one-
# season-out (LOSO) data construction used by the empirical baseline so that
# both methods are evaluated on identical (country, season, cutoff, prediction
# month) rows. Functions are pure; no I/O.

#' Build long-format prediction rows for one (country, held-out season)
#'
#' Each row is a single (cutoff_k, prediction_month_m) pair for one held-out
#' season `s`. Predictors are constructed strictly from data observable at
#' nowcast time: cumulative cases through `k` and last observed cases at `k`
#' come from the held-out season itself; the seasonal proportions `P_k` and
#' `p_m` are leave-one-season-out averages over the country's other seasons.
#'
#' @param country_df Data for one country across multiple seasons. Must contain
#'   `iso3`, `country`, `Region`, `season`, `season_nMonth`, `Month`, `cases`,
#'   `Actual_monthly_proportion`, `Actual_cum_monthly_proportion`.
#' @param holdout_season The season to treat as held-out (predictors derived
#'   from this season's observations; `P_k` / `p_m` exclude this season).
#' @param min_train_seasons Minimum training seasons required after holdout.
#' @return Tibble with one row per (cutoff_k, prediction_month_m); empty if
#'   training is insufficient or the LOSO baseline at `k` is unusable.
build_gam_rows_one_holdout <- function(country_df,
                                       holdout_season,
                                       min_train_seasons = 2L) {
  test_df <- country_df %>% dplyr::filter(season == holdout_season)
  train_df <- country_df %>% dplyr::filter(season != holdout_season)

  if (dplyr::n_distinct(train_df$season) < min_train_seasons) {
    return(tibble::tibble())
  }

  baseline <- fit_baseline_profile(train_df)

  iso3_i <- dplyr::first(country_df$iso3)
  country_i <- dplyr::first(country_df$country)
  region_i <- dplyr::first(country_df$Region)

  test_lookup <- test_df %>%
    dplyr::arrange(season_nMonth) %>%
    dplyr::transmute(
      season_nMonth,
      Month,
      cases,
      cum_cases = cumsum(dplyr::coalesce(cases, 0))
    )

  purrr::map_dfr(1:11, function(k) {
    P_k_row <- baseline %>% dplyr::filter(season_nMonth == k)
    if (nrow(P_k_row) == 0) return(tibble::tibble())

    P_k <- P_k_row$Ave_cum_monthly_proportion
    if (length(P_k) == 0 || is.na(P_k) || P_k <= 0) return(tibble::tibble())

    cum_through_k_row <- test_lookup %>% dplyr::filter(season_nMonth == k)
    if (nrow(cum_through_k_row) == 0) return(tibble::tibble())

    C_k <- cum_through_k_row$cum_cases
    L_k <- cum_through_k_row$cases

    baseline %>%
      dplyr::filter(season_nMonth > k) %>%
      dplyr::left_join(
        test_lookup %>% dplyr::select(season_nMonth, Month, cases),
        by = "season_nMonth"
      ) %>%
      dplyr::transmute(
        iso3 = iso3_i,
        country = country_i,
        Region = region_i,
        season = holdout_season,
        cutoff_month = k,
        prediction_month = season_nMonth,
        Month,
        lead_time = season_nMonth - k,
        observed_cumulative_cases_at_cutoff = C_k,
        last_observed_cases = L_k,
        Ave_cum_monthly_proportion_at_cutoff = P_k,
        Ave_monthly_proportion_missing = Ave_monthly_proportion,
        actual_cases = cases
      )
  })
}

#' Build LOSO prediction rows across all eligible (country, season) pairs
#'
#' Combines per-country LOSO row construction across the full validation
#' dataset. Used to populate both training (rows from non-held-out fold) and
#' test (rows from held-out fold) data for the GAM model.
#'
#' @param validation_data Long table with all countries × seasons × months;
#'   same input as the empirical script.
#' @param min_seasons Minimum distinct seasons per country to be included.
#' @param min_train_seasons Minimum training seasons after each holdout.
#' @return Tibble of prediction rows with predictors and `actual_cases`.
build_gam_rows_all <- function(validation_data,
                               min_seasons = 3L,
                               min_train_seasons = 2L) {
  validation_data %>%
    dplyr::group_by(iso3) %>%
    dplyr::filter(dplyr::n_distinct(season) >= min_seasons) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(function(country_df) {
      seasons <- sort(unique(country_df$season))
      purrr::map_dfr(seasons, function(s) {
        build_gam_rows_one_holdout(country_df, s, min_train_seasons)
      })
    })
}

#' Add modelling-friendly transformed columns to a prediction-rows tibble.
#'
#' Keeps `season_month` separate from the calendar `Month` so the GAM can
#' express both season-aligned and calendar-month effects (which differ in
#' countries whose seasonal low month is not January).
#'
#' @param df Prediction rows from [build_gam_rows_all()] / [build_gam_rows_one_holdout()].
#' @return Same rows with added numeric / factor predictor columns.
add_gam_features <- function(df) {
  df %>%
    dplyr::mutate(
      log_last_cases = log1p(pmax(last_observed_cases, 0)),
      log_cum_cases = log1p(pmax(observed_cumulative_cases_at_cutoff, 0)),
      P_cutoff = Ave_cum_monthly_proportion_at_cutoff,
      p_target = Ave_monthly_proportion_missing,
      season_month = prediction_month,
      cutoff_month_num = cutoff_month,
      prediction_calendar_month = Month,
      Region_factor = factor(
        ifelse(is.na(Region), "Unknown", Region)
      ),
      iso3_factor = factor(iso3)
    )
}

#' Fit the negative-binomial GAM described in the prototype plan.
#'
#' Uses `mgcv::bam()` with `family = mgcv::nb()` for fast scalable fitting on
#' pooled cross-country training data. Cyclic cubic regression splines are used
#' for season_month, prediction_calendar_month, and cutoff_month with period
#' 12 (knots at 1 and 13).
#'
#' Country structure is controlled by `iso3_term`:
#'   - "none"   : no country term (cross-country pooled model only).
#'   - "re"     : add `s(iso3_factor, bs = "re")` random intercept (only valid
#'                under leave-one-season-out, never leave-one-country-out).
#'   - "smooth" : replace the global `s(season_month, bs = "cc")` with a
#'                factor-smooth interaction
#'                `s(season_month, iso3_factor, bs = "fs", xt = list(bs = "cc"))`
#'                that gives each country its own cyclic seasonal shape, all
#'                shrunk toward a shared shape via a single smoothing parameter.
#'                The factor-smooth includes the per-country intercept so an
#'                additional `re` term is not needed.
#'
#' Returns a fitted bam object on success or NULL on failure.
#'
#' @param train_df Training rows with features (output of [add_gam_features()]).
#' @param with_iso3_re Deprecated convenience flag; equivalent to
#'   `iso3_term = "re"` when TRUE. Ignored when `iso3_term` is supplied.
#' @param iso3_term One of "none", "re", "smooth" (see Description).
#' @param k_smooth Knot count for non-cyclic numeric smooths.
#' @param k_cyclic Knot count for cyclic month smooths.
#' @param nthreads bam threads (1..parallel::detectCores()).
#' @return Fitted `bam` object or NULL.
fit_gam_nowcast <- function(train_df,
                            with_iso3_re = FALSE,
                            iso3_term = NULL,
                            k_smooth = 6L,
                            k_cyclic = 6L,
                            nthreads = 1L) {
  if (is.null(iso3_term)) {
    iso3_term <- if (isTRUE(with_iso3_re)) "re" else "none"
  }
  iso3_term <- match.arg(iso3_term, c("none", "re", "smooth"))
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package mgcv is required for fit_gam_nowcast().")
  }

  train_df <- train_df %>%
    dplyr::filter(
      !is.na(actual_cases),
      is.finite(P_cutoff),
      is.finite(p_target),
      is.finite(log_cum_cases),
      is.finite(log_last_cases),
      is.finite(lead_time)
    )

  if (nrow(train_df) < 200) {
    return(NULL)
  }

  if (iso3_term != "none" && !is.factor(train_df$iso3_factor)) {
    train_df$iso3_factor <- factor(train_df$iso3)
  }

  shared_terms <- "s(lead_time, k = k_smooth) +
    s(log_last_cases, k = k_smooth) +
    s(log_cum_cases, k = k_smooth) +
    s(P_cutoff, k = k_smooth) +
    s(p_target, k = k_smooth) +
    s(prediction_calendar_month, bs = \"cc\", k = k_cyclic) +
    s(cutoff_month_num, bs = \"cc\", k = k_cyclic) +
    Region_factor"

  # season_month enters either as a global cyclic smooth, or — under
  # iso3_term == "smooth" — as a factor-smooth interaction giving each
  # country its own cyclic seasonal shape (with shared smoothing).
  season_term <- if (iso3_term == "smooth") {
    "s(season_month, iso3_factor, bs = \"fs\", xt = list(bs = \"cc\"), k = k_cyclic)"
  } else {
    "s(season_month, bs = \"cc\", k = k_cyclic)"
  }

  re_term <- if (iso3_term == "re") {
    " + s(iso3_factor, bs = \"re\")"
  } else {
    ""
  }

  formula_obj <- stats::as.formula(
    paste("actual_cases ~", shared_terms, "+", season_term, re_term)
  )

  shared_terms_factor <- "s(lead_time, k = k_smooth) +
    s(log_last_cases, k = k_smooth) +
    s(log_cum_cases, k = k_smooth) +
    s(P_cutoff, k = k_smooth) +
    s(p_target, k = k_smooth) +
    factor(prediction_calendar_month) +
    factor(cutoff_month_num) +
    Region_factor"

  season_term_factor <- if (iso3_term == "smooth") {
    "s(season_month, iso3_factor, bs = \"fs\", xt = list(bs = \"cc\"), k = k_cyclic)"
  } else {
    "factor(season_month)"
  }

  formula_factor <- stats::as.formula(
    paste("actual_cases ~", shared_terms_factor, "+", season_term_factor, re_term)
  )

  knot_list <- list(
    season_month = c(1, 13),
    prediction_calendar_month = c(1, 13),
    cutoff_month_num = c(1, 13)
  )

  fit <- tryCatch(
    mgcv::bam(
      formula_obj,
      family = mgcv::nb(),
      data = train_df,
      method = "fREML",
      discrete = TRUE,
      nthreads = nthreads,
      knots = knot_list
    ),
    error = function(e) {
      message("bam() failed: ", conditionMessage(e), " — falling back to factor-month structure.")
      tryCatch(
        mgcv::bam(
          formula_factor,
          family = mgcv::nb(),
          data = train_df,
          method = "fREML",
          discrete = TRUE,
          nthreads = nthreads
        ),
        error = function(e2) {
          message("Fallback bam() also failed: ", conditionMessage(e2))
          NULL
        }
      )
    }
  )

  fit
}

#' Predict held-out monthly cases from a fitted GAM nowcast model.
#'
#' Predictions are clamped at zero and rounded to integers to match the
#' rounding convention of the empirical baseline (`predicted_cases`).
#'
#' @param fit Output of [fit_gam_nowcast()] or NULL.
#' @param test_df Test rows with features (output of [add_gam_features()]).
#' @return `test_df` augmented with `predicted_cases` (NA when fit is NULL or
#'   prediction fails).
predict_gam_nowcast <- function(fit, test_df) {
  test_df$predicted_cases <- NA_real_
  if (is.null(fit) || nrow(test_df) == 0) {
    return(test_df)
  }

  iso3_levels <- tryCatch(levels(fit$model$iso3_factor), error = function(e) NULL)
  if (!is.null(iso3_levels)) {
    new_iso <- as.character(test_df$iso3_factor)
    test_df$iso3_factor <- factor(new_iso, levels = iso3_levels)
  }

  pred <- tryCatch(
    as.numeric(predict(fit, newdata = test_df, type = "response")),
    error = function(e) {
      message("predict() failed: ", conditionMessage(e))
      rep(NA_real_, nrow(test_df))
    }
  )
  test_df$predicted_cases <- round(pmax(0, pred), 0)
  test_df
}
