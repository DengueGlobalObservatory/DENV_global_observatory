# ---
# title: "00_FUN_apply_delay_correction"
# author: "K M Susong"
# ---

# Included functions
# ==================

# - load_rf_lookup()           helper, read + validate a stable RF lookup
# - select_rf_from_lookup()    helper, apply RF selection / exclusion rules
# - apply_delay_correction()   main, source-aware (PAHO or WHO)

# Required Libraries
library(dplyr)
library(countrycode) # only used as an iso3 fallback when iso3 is absent

# ============================================================
# Note on reporting delay (d)
# ----------------------------
# This function does NOT calculate the reporting delay. `d` is expected to be
# pre-computed upstream in Scripts/data_sourcing/01_dengue_data.R:
#   - PAHO: weekly delay (matches paho_rf_lookup.csv)
#   - WHO : monthly delay (matches who_rf_lookup.csv)
# The correction only joins the appropriate lookup on (iso3, d) and applies the
# agreed selection / exclusion rules.

# As of 080-June-2026 SEARO does not have a look up table or correction
# ============================================================


# ============================================================
#' **load_rf_lookup()**
#'
#' Reads and validates a stable reporting-factor (RF) lookup table.
#'
#' @description
#' Loads `paho_rf_lookup.csv` or `who_rf_lookup.csv` from `lookup_dir` and keeps
#' only the columns needed for correction. Stops with a clear message if the file
#' or any required column is missing.
#'
#' @param source String, either "PAHO" or "WHO".
#' @param lookup_dir Directory holding the stable lookups. Default "Assets/Stable".
#'
#' @return Data frame with columns: `iso3`, `d`, `u_rf`, `med_rf`, `sd_rf`, `n_rf`.
#'         `d` is coerced to integer so it joins cleanly to the data delay column.

load_rf_lookup <- function(source, lookup_dir = "Assets/Stable") {
  #selection file name for listed source
  file <- switch(
    source,
    PAHO = file.path(lookup_dir, "paho_rf_lookup.csv"),
    WHO  = file.path(lookup_dir, "who_rf_lookup.csv"),
    SEARO = file.path(lookup_dir, "searo_rf_lookup.csv"),
    stop("Unknown source '", source, "'. Expected 'PAHO' or 'WHO'.")
  )

  if (!file.exists(file)) {
    stop("RF lookup not found: ", file)
  }
  # read lookup table 
  lookup <- read.csv(file, stringsAsFactors = FALSE)

  # Confirm the columns the rules depend on are present
  required <- c("iso3", "d", "u_rf", "med_rf", "sd_rf", "n_rf")
  missing <- setdiff(required, names(lookup))
  if (length(missing) > 0) {
    stop("RF lookup ", basename(file), " is missing columns: ",
         paste(missing, collapse = ", "))
  }

  lookup %>%
    dplyr::select(iso3, d, u_rf, med_rf, sd_rf, n_rf) %>%
    dplyr::mutate(d = as.integer(d))

}


# ============================================================
#' **select_rf_from_lookup()**
#'
#' Applies the RF selection and exclusion rules to data already joined to a lookup.
#'
#' @description
#' Operates on a data frame that has been left-joined to a lookup (so it carries
#' `u_rf`, `med_rf`, `sd_rf`, `n_rf`). Decides, per row, which factor to use and
#' whether the row should be excluded from correction. Rules are applied in order:
#' 
#'   1. Country on the exclusion list               -> "country_excluded"
#'   2. No matching lookup row (n_rf is NA)         -> "no_lookup_match"
#'   3. Too few observations behind the factor      -> "insufficient_n_rf"
#'   4. Chosen factor missing                       -> "rf_missing"
#'   5. Chosen factor above the cap                 -> "rf_exceeds_max"
#'   6. Otherwise apply: median when sd is high, else mean
#'      ("applied_median" / "applied_mean")
#'      
#' The base factor is `u_rf`, switched to `med_rf` when `sd_rf` exceeds
#' `sd_switch_threshold` as the median is more robust when spread is large.
#'
#' @param df Data frame already joined to the lookup columns.
#' @param min_n_rf Minimum n_rf required to correct; rows below are excluded.
#' @param max_rf Maximum allowed factor; rows above are excluded.
#' @param sd_switch_threshold sd_rf above which med_rf is used instead of u_rf.
#' @param excluded_iso3 Character vector of iso3 codes to never correct.
#'
#' @return `df` with added columns `rf`, `correction_excluded`, `correction_reason`.

select_rf_from_lookup <- function(df,
                                  min_n_rf,
                                  max_rf,
                                  sd_switch_threshold,
                                  excluded_iso3 = character(0)) {

  df %>%
    dplyr::mutate(
      # Base factor: mean, switched to median where spread is large
      .rf_base = dplyr::if_else(
        !is.na(sd_rf) & sd_rf > sd_switch_threshold, med_rf,
        u_rf
      ),
      # Single ordered decision describing the outcome for each row
      correction_reason = dplyr::case_when(
        iso3 %in% excluded_iso3                     ~ "country_excluded",
        is.na(n_rf)                                 ~ "no_lookup_match",
        n_rf < min_n_rf                             ~ "insufficient_n_rf",
        is.na(.rf_base)                             ~ "rf_missing",
        .rf_base > max_rf                           ~ "rf_exceeds_max",
        !is.na(sd_rf) & sd_rf > sd_switch_threshold ~ "applied_median",
        TRUE                                        ~ "applied_mean"
      ),
      correction_excluded = correction_reason %in% c(
        "country_excluded", "no_lookup_match",
        "insufficient_n_rf", "rf_missing", "rf_exceeds_max"
      ),
      # Only carry a usable factor forward where the row is not excluded
      rf = dplyr::if_else(correction_excluded, NA_real_, .rf_base)
    ) %>%
    dplyr::select(-.rf_base)
}


# ============================================================
#' **apply_delay_correction()**
#'
#' Applies empirically derived reporting-delay correction factors to PAHO or WHO
#' dengue case counts, using the stable RF lookups in `Assets/Stable`.
#'
#' @description
#' 
#'   1. Joins the source-specific lookup on (`iso3`, `d`)
#'   2. selects a factor per the agreed rules (see `select_rf_from_lookup()`)
#'   3. multiplies the case count.
#'   
#' Three case columns are returned so no value is ever dropped for lack of an RF:
#'   - raw     (`cases_col`)   : observed, untouched
#'   - corrected (`output_col`): RF-adjusted, NA where excluded/no match (honest)
#'   - applied (`applied_col`) : coalesce(corrected, raw) - the column downstream uses
#' `correction_excluded` / `correction_reason` record intent.
#'
#' Reporting delay `d` must already exist on `df` (added in 01_dengue_data.R).
#'
#' @param df Data frame of weekly PAHO or monthly WHO data. Must include `d` and
#'   the case count column, plus `iso3` (derived from `country` if absent).
#' @param source String, "PAHO" or "WHO".
#' @param cases_col Name of the raw case column. Default "total_den" (PAHO) /
#'   "cases" (WHO).
#' @param output_col Name of the corrected case column. Default
#'   "total_corrected_cases" (PAHO) / "cases_corrected" (WHO).
#' @param applied_col Name of the applied (corrected-or-raw) column. Default
#'   "total_applied_cases" (PAHO) / "cases_applied" (WHO).
#' @param lookup_dir Directory with the stable lookups. Default "Assets/Stable".
#' @param min_n_rf Minimum observations behind a factor. Default 20 (PAHO) / 3 (WHO).
#' @param max_rf Maximum allowed factor. Default 5.
#' @param sd_switch_threshold sd_rf above which med_rf replaces u_rf. Default 1.5.
#' @param paho_excluded_iso3 iso3 codes excluded for PAHO. Default "BLZ" (Belize).
#'
#' @return `df` with raw, corrected and applied case columns plus audit columns:
#'   `rf`, `correction_applied`, `correction_excluded`, `correction_reason`.

apply_delay_correction <- function(df,
                                   source = c("PAHO", "WHO"),
                                   cases_col = NULL,
                                   output_col = NULL,
                                   applied_col = NULL,
                                   lookup_dir = "Assets/Stable",
                                   min_n_rf = NULL,
                                   max_rf = 5,
                                   sd_switch_threshold = 1.5,
                                   paho_excluded_iso3 = "BLZ") {

  source <- match.arg(source)

  # ---- Source-specific defaults ----
  if (is.null(cases_col))   cases_col   <- if (source == "PAHO") "total_den" else "cases"
  if (is.null(output_col))  output_col  <- if (source == "PAHO") "total_corrected_cases" else "cases_corrected"
  if (is.null(applied_col)) applied_col <- if (source == "PAHO") "total_applied_cases" else "cases_applied"
  if (is.null(min_n_rf))    min_n_rf    <- if (source == "PAHO") 20 else 3
  excluded_iso3 <- if (source == "PAHO") paho_excluded_iso3 else character(0)

  # ---- Input checks ----
  # d is computed upstream; fail early and clearly if it is missing.
  if (!"d" %in% names(df)) {
    stop("Column 'd' not found. Run 01_dengue_data.R first so reporting delay is set for ", source, ".")
  }
  if (!cases_col %in% names(df)) {
    stop("Case column '", cases_col, "' not found in ", source, " data.")
  }

  # iso3 is the join key. PAHO/WHO normally carry it; derive as a fallback only.
  if (!"iso3" %in% names(df)) {
    df$iso3 <- countrycode::countrycode(
      df$country, "country.name", "iso3c",
      custom_match = c("Saint Martin" = "MAF")
    )
  }

  # ---- Join the stable lookup on (iso3, d) ----
  lookup <- load_rf_lookup(source, lookup_dir)

  df <- df %>%
    dplyr::mutate(d = as.integer(round(d))) %>% # integer key to match the lookup
    dplyr::left_join(lookup, by = c("iso3", "d"))

  # ---- Select factor / exclusion outcome, then apply ----
  df <- select_rf_from_lookup(
    df,
    min_n_rf = min_n_rf,
    max_rf = max_rf,
    sd_switch_threshold = sd_switch_threshold,
    excluded_iso3 = excluded_iso3
  )

  # corrected: NA where excluded (honest); applied: corrected, else raw (never NA for lack of RF)
  df[[output_col]] <- dplyr::if_else(
    df$correction_excluded,
    NA_real_,
    round(df[[cases_col]] * df$rf, 0)
  )
  df[[applied_col]] <- dplyr::coalesce(df[[output_col]], as.numeric(df[[cases_col]]))

  # correction_applied: TRUE only where a corrected value was actually produced
  df$correction_applied <- !is.na(df[[output_col]])

  # ---- Optional run summary ----
  if (exists("log_message")) {
    n_total    <- nrow(df)
    n_applied  <- sum(df$correction_applied)
    n_excluded <- sum(df$correction_excluded)
    log_message(source %+% " delay correction: " %+% n_applied %+% "/" %+% n_total %+%
                  " rows corrected, " %+% n_excluded %+% " excluded")
    reason_tab <- sort(table(df$correction_reason), decreasing = TRUE)
    log_message(source %+% " correction reasons: " %+%
                  paste(names(reason_tab), reason_tab, sep = "=", collapse = ", "))
  }

  # ---- Tidy, source-appropriate output ----
  # PAHO keeps the fields compute_monthcumm_cases() needs (it recomputes onset/d);
  # WHO keeps the fields source selection needs. any_of() tolerates absent columns.
  audit_cols <- c("rf", "correction_applied", "correction_excluded", "correction_reason")

  if (source == "PAHO") {
    df <- df %>%
      dplyr::select(dplyr::any_of(c(
        "country", "iso3", "year", "ext_date", "EW", "onset_date",
        cases_col, output_col, applied_col, "d", audit_cols
      )))
  } else {
    df <- df %>%
      dplyr::select(dplyr::any_of(c(
        "country", "iso3", "date",
        cases_col, output_col, applied_col, "d", "d_unit", audit_cols
      )))
  }

  return(df)
}
