#!/usr/bin/env Rscript

#' ---
#' title: "01_prepare_training_data"
#' author: "K M Susong"
#' ---
#'
#' Overview:
#' ========
#' Build the analysis panel the forecast layer trains on, by merging the two
#' relevant files from the pinned pipeline snapshot:
#'
#'   full_data_season_monthly_proportions.csv  - deep historical record
#'       (complete dengue seasons only, OpenDengue history + WHO fallback,
#'        season-aligned, with actual monthly / cumulative proportions)
#'   DENV_cases_nowcast_output.csv              - current period
#'       (reporting-delay-corrected observed months + proportion-estimated
#'        recent months, with Data_status)
#'
#' The nowcast view wins on any overlapping country-month, so the panel extends
#' as far forward as data exists and reflects the post-nowcast "ground truth".
#'
#' Output is one row per country-month over each country's covered span, with:
#'   - `cases`        merged monthly count (NA only on internal gap months)
#'   - `data_status`  observed | corrected | estimated | gap
#'   - `source`       PAHO / SEARO / WHO / OD_national / Estimates
#'   - the 8-region label
#'   - model-ready features: log cases, calendar-month harmonics, 12-month lag,
#'     a within-country time index
#'   - season alignment + actual seasonal proportions (for the nowcast baseline)
#'
#' Cases are never imputed here. The historical file drops all-zero years, so a
#' country can have internal gaps; those are kept as explicit `gap` rows so lags
#' and rolling windows stay well defined.
#'
#' Input : Output/<snapshot_date>/{full_data_season_monthly_proportions,DENV_cases_nowcast_output}.csv
#' Output: Output/forecasting/training_data/training_panel_<snapshot_date>.csv
#'         Output/forecasting/training_data/country_region_lookup.csv
#'
#' Timeline:
#' ========
#' 02-09-2026: Created. Stage 0 scaffold.
#' 02-09-2026: Merge nowcast output with the historical file; add `data_status`.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(cli)
})

# ---- Define project configs ----------------------------------------
source("Scripts/forecasting/00_config.R")

for (f in c(training_snapshot, nowcast_snapshot)) {
  if (!file.exists(f)) {
    cli::cli_abort(c(
      "Pinned snapshot file not found: {.path {f}}.",
      "i" = "Set {.code snapshot_date} in {.path Scripts/forecasting/00_config.R} to an existing Output/<date> run."
    ))
  }
}

out_dir <- file.path(forecast_out, "training_data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

data_status_levels <- c("observed", "corrected", "estimated", "gap")

# ---- Historical record --------------------------------------------
history <- read_csv(training_snapshot, show_col_types = FALSE) %>%
  dplyr::transmute(
    iso3           = str_to_upper(iso3),
    country,
    date           = as.Date(date),
    hist_cases     = cases,
    hist_source    = source,
    hist_season    = season,
    hist_season_nMonth = season_nMonth,
    hist_mean_low_month = mean_low_month,
    Actual_monthly_proportion,
    Actual_cum_monthly_proportion
  )

# ---- Current period (nowcast output) -----------------------------
# Keep months that carry a value; drop the trailing unobservable future months.
nowcast <- read_csv(nowcast_snapshot, show_col_types = FALSE) %>%
  dplyr::filter(!is.na(cases)) %>%
  dplyr::transmute(
    iso3               = str_to_upper(iso3),
    country            = Country,
    date               = as.Date(date),
    nc_cases           = cases,
    nc_source          = source,
    raw_cases,
    corrected_cases,
    correction_applied = tidyr::replace_na(correction_applied, FALSE),
    delay_months       = d,
    rf,
    nc_season          = season,
    nc_season_nMonth    = season_nMonth,
    nc_mean_low_month   = mean_low_month,
    nc_data_status     = dplyr::case_when(
      source == "Estimates"        ~ "estimated",
      correction_applied %in% TRUE ~ "corrected",
      TRUE                         ~ "observed"
    )
  )

# ---- Merge: nowcast wins on overlap ------------------------------
merged <- dplyr::full_join(nowcast, history, by = c("iso3", "date")) %>%
  dplyr::mutate(
    from_nowcast   = !is.na(nc_cases),
    country        = dplyr::coalesce(country.x, country.y),
    cases          = dplyr::coalesce(nc_cases, hist_cases),
    source         = dplyr::coalesce(nc_source, hist_source),
    raw_cases      = dplyr::coalesce(raw_cases, hist_cases),
    correction_applied = dplyr::coalesce(correction_applied, FALSE),
    data_status    = dplyr::coalesce(
      nc_data_status,
      dplyr::if_else(!is.na(hist_cases), "observed", NA_character_)
    ),
    season         = dplyr::coalesce(nc_season, hist_season),
    season_nMonth  = dplyr::coalesce(nc_season_nMonth, hist_season_nMonth),
    mean_low_month = dplyr::coalesce(nc_mean_low_month, hist_mean_low_month)
  ) %>%
  dplyr::select(
    iso3, country, date, cases, data_status, source,
    raw_cases, corrected_cases, correction_applied, delay_months, rf,
    season, season_nMonth, mean_low_month,
    Actual_monthly_proportion, Actual_cum_monthly_proportion, from_nowcast
  )

cli::cli_inform(c(
  "Merged snapshot:",
  "*" = "{dplyr::n_distinct(merged$iso3)} countries, {format(min(merged$date))} to {format(max(merged$date))}",
  "*" = "{sum(merged$from_nowcast)} country-months from the nowcast output, {sum(!merged$from_nowcast)} from history"
))

# Negative monthly counts are a cumulative-to-monthly revision artefact
# (documented pipeline pitfall). Blank them so they read as internal gaps.
neg_cases <- merged %>% dplyr::filter(!is.na(cases), cases < 0)
if (nrow(neg_cases) > 0) {
  cli::cli_warn(c(
    "{nrow(neg_cases)} country-month(s) with negative counts set to NA (revision artefact):",
    stats::setNames(
      sprintf("%s  %s  cases = %s", neg_cases$iso3, format(neg_cases$date), neg_cases$cases),
      rep("*", nrow(neg_cases))
    )
  ))
  merged <- merged %>%
    dplyr::mutate(cases = dplyr::if_else(!is.na(cases) & cases < 0, NA_real_, cases))
}

# ---- Attach the 8-region label --------------------------------------
region_ref <- read_csv(included_countries_ref, show_col_types = FALSE) %>%
  dplyr::transmute(iso3 = str_to_upper(iso3), region = od_region) %>%
  dplyr::distinct(iso3, .keep_all = TRUE)

merged <- merged %>% dplyr::left_join(region_ref, by = "iso3")

missing_region <- merged %>%
  dplyr::filter(is.na(region)) %>%
  dplyr::distinct(iso3) %>%
  dplyr::pull(iso3)

if (length(missing_region) > 0) {
  source(region_resolver_fn)
  resolved <- get_od_regions(missing_region) %>%
    dplyr::transmute(iso3 = str_to_upper(ISO_A0), region_fallback = od_region)
  merged <- merged %>%
    dplyr::left_join(resolved, by = "iso3") %>%
    dplyr::mutate(region = dplyr::coalesce(region, region_fallback)) %>%
    dplyr::select(-region_fallback)
  cli::cli_inform("Region resolver filled {length(missing_region)} country(ies): {.val {missing_region}}.")
}

still_missing <- merged %>%
  dplyr::filter(is.na(region) | region == "Other") %>%
  dplyr::distinct(iso3) %>%
  dplyr::pull(iso3)
if (length(still_missing) > 0) {
  cli::cli_warn("No canonical region for: {.val {still_missing}} - left as-is for review.")
}

# ---- Fill each country's monthly grid ------------------------------
# Contiguous months from a country's first record to its last valued month.
country_const <- merged %>%
  dplyr::group_by(iso3) %>%
  dplyr::summarise(
    country        = dplyr::first(country),
    region         = dplyr::first(region),
    mean_low_month = dplyr::first(mean_low_month[!is.na(mean_low_month)]),
    date_min       = min(date),
    date_max       = max(date[!is.na(cases)]),
    .groups        = "drop"
  )

grid <- country_const %>%
  dplyr::mutate(date = map2(date_min, date_max, ~ seq(.x, .y, by = "month"))) %>%
  tidyr::unnest(date) %>%
  dplyr::select(iso3, country, region, mean_low_month, date)

panel <- grid %>%
  dplyr::left_join(
    merged %>%
      dplyr::select(
        iso3, date, cases, data_status, source,
        raw_cases, corrected_cases, correction_applied, delay_months, rf,
        season, season_nMonth,
        Actual_monthly_proportion, Actual_cum_monthly_proportion
      ),
    by = c("iso3", "date")
  ) %>%
  dplyr::mutate(
    Year        = lubridate::year(date),
    Month       = lubridate::month(date),
    data_status = dplyr::if_else(is.na(cases), "gap", data_status),
    data_status = factor(data_status, levels = data_status_levels)
  )

# ---- Model-ready features ----------------------------------------
panel <- panel %>%
  dplyr::arrange(iso3, date) %>%
  dplyr::group_by(iso3) %>%
  dplyr::mutate(
    time_index  = dplyr::row_number() - 1L,
    cases_lag12 = dplyr::lag(cases, 12L),
    log_cases   = log1p(pmax(cases, 0))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    sin1 = sin(2 * pi * Month / 12),
    cos1 = cos(2 * pi * Month / 12),
    sin2 = sin(4 * pi * Month / 12),
    cos2 = cos(4 * pi * Month / 12)
  )

panel <- panel %>%
  dplyr::select(
    iso3, country, region, date, Year, Month,
    cases, log_cases, data_status, source,
    raw_cases, corrected_cases, correction_applied, delay_months, rf,
    season, season_nMonth, mean_low_month,
    Actual_monthly_proportion, Actual_cum_monthly_proportion,
    cases_lag12, time_index, sin1, cos1, sin2, cos2
  ) %>%
  dplyr::arrange(iso3, date)

# ---- Write ----------------------------------------------------
panel_path  <- file.path(out_dir, paste0("training_panel_", snapshot_date, ".csv"))
lookup_path <- file.path(out_dir, "country_region_lookup.csv")

write_csv(panel, panel_path)

panel %>%
  dplyr::distinct(iso3, country, region) %>%
  dplyr::arrange(region, country) %>%
  write_csv(lookup_path)

# ---- Console summary -----------------------------------------
cli::cli_h2("Training panel written")
cli::cli_inform(c(
  "*" = "{.path {panel_path}}",
  "*" = "{dplyr::n_distinct(panel$iso3)} countries, {nrow(panel)} country-months, {format(min(panel$date))} to {format(max(panel$date))}"
))

cli::cli_h3("data_status")
panel %>%
  dplyr::count(data_status, name = "n_country_months") %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cli::cli_h3("countries per region")
panel %>%
  dplyr::distinct(iso3, region) %>%
  dplyr::count(region, name = "n_countries") %>%
  dplyr::arrange(dplyr::desc(n_countries)) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
