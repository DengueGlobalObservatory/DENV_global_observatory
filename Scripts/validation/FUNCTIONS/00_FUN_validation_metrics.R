#' Validation helpers — proportion-based LOSO nowcast
#'
#' Two small functions used inside the leave-one-season-out loop. All summaries,
#' quantiles, and figures live in the action scripts for readability.

library(dplyr)

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
