#' ---
#' title: "00_config"
#' author: "K M Susong"
#' ---
#'
#' Overview:
#' ========
#' Single source of truth for the forecasting evaluation pipeline. Every
#' evaluation dimension (training snapshot, forecast horizons, rolling-window
#' lengths, origin range, provisional strata thresholds) is a named constant
#' here, so a whole re-run is changed by editing one line.
#'
#' Sourced by every `Scripts/forecasting/` script. Defines constants only —
#' no libraries, no side effects, no file I/O.
#'
#' Timeline:
#' ========
#' 02-09-2026: Created. Stage 0 scaffold.
#' 10-09-2026: Added country geometry ref + country-size scenario strata
#'   (size_breaks / size_labels) for the area sensitivity split.

# ---- Pinned training snapshot ----------------------------------------------
# The forecast layer trains and backtests against a frozen pipeline run, not a
# live re-source. 01_prepare_training_data.R merges two files from this run:
#   - full_data_season_monthly_proportions.csv : the deep historical record
#     (complete seasons, OpenDengue history + WHO fallback, season-aligned)
#
#   - DENV_cases_nowcast_output.csv            : the current period, carrying
#     reporting-delay-corrected and proportion-estimated recent months
# The nowcast view wins on any overlapping country-month, so the panel is the
# most up-to-date post-nowcast series available (the "ground truth" of the plan).

snapshot_date     <- "2026_08_01"
snapshot_dir      <- file.path("Output", snapshot_date)
training_snapshot <- file.path(snapshot_dir, "full_data_season_monthly_proportions.csv")
nowcast_snapshot  <- file.path(snapshot_dir, "DENV_cases_nowcast_output.csv")
seasonal_profile  <- file.path(snapshot_dir, "DENV_average_season.csv")

# Country -> region reference (canonical 8-region scheme). fn_OD_region.R is the
# fallback resolver for any iso3 not listed here.
included_countries_ref <- "Assets/Stable/OD_maps/gdo_included_countries_by_od_region.csv"
region_resolver_fn     <- "Assets/Stable/OD_maps/fn_OD_region.R"

# Country outline geometry (iso_a3 + polygon). 02_data_summary.R reads this to
# derive land area (km^2) for the country-size scenario stratum.
country_geo_ref <- "Assets/Stable/OD_maps/base_map.gpkg"

# ---- Output ---------------------------------------------------------------
forecast_out <- "Output/forecasting"

# ---- Forecast evaluation dimensions --------------------------------------
# Adjust these freely — they define the whole Stage 1 backtest grid.
forecast_horizons <- 1:6                       # months ahead of each origin

roll_windows <- c(                             # training-window lengths, in months
  fixed_5y  = 60L,
  fixed_7y  = 84L,
  fixed_10y = 120L,
  expanding = NA_integer_                      # NA => all history to the origin
)

min_train_months <- 36L                        # a country needs this many observed
                                               # months in-window to enter a fit
origin_start <- as.Date("2016-01-01")          # earliest rolling-origin month

# ---- Model output quantile schema ------------------------------------------
# Single source of truth for the five predictive quantiles every model targets
# (matches forecast_output_cols in models/utils/forecast_helpers.R: q25/q75 are
# the 50% interval, q05/q95 the 90% interval, q50 the median/.pred). Every
# model file sources this rather than keeping its own copy, so the schema can't
# drift between models.
interval_probs <- c(q05 = 0.05, q25 = 0.25, q50 = 0.50, q75 = 0.75, q95 = 0.95)

# ---- Model object saving -------------------------------------------------
save_fit_objects <- TRUE

# ---- Canonical regions --------------------------------------------------
region_levels <- c(
  "North & Central America",
  "Caribbean",
  "South America",
  "Europe, Middle East & North Africa",
  "Sub-Saharan Africa",
  "South Asia",
  "East & Southeast Asia",
  "Pacific Islands"
)

# ---- Provisional scenario strata --------------------------------------
# PROVISIONAL cut-points — flagged in every table these produce, and to be
# confirmed with the team once the country summary (02) has been reviewed.

endemic_min_mean_cases    <- 50     # mean monthly cases at/above => "endemic" candidate
endemic_min_seasons       <- 8      # complete seasons at/above   => "endemic" candidate
endemic_max_zero_fraction <- 0.10   # fraction of zero-case months below => "endemic" candidate

# seasonality_signal (strong / moderate / weak) is assigned by tertiles of the
# seasonal-concentration index in 02_data_summary.R — no fixed constant.
# note: could instead use Kishen's seasonality clusters — ask him for the file.

# ---- Country-size scenario stratum ----------------------------------
# Stratify forecast skill by country land area (OB, 2026-09-08 meeting): a
# continental country averages over far more environmental heterogeneity than a
# city-state.
#
# size_class is assigned in 02_data_summary.R as
#   cut(country_area_km2, breaks = size_breaks, labels = size_labels, right = FALSE)
# so the split is changed entirely from here — adjust the break-points AND/OR the
# number of classes by editing these two vectors (length(size_breaks) must stay
# length(size_labels) + 1). PROVISIONAL — Katie's call on cuts and class count.
size_breaks <- c(0, 25e3, 100e3, 1e6, 5e6, Inf)   # km^2 boundaries
size_labels <- c("very_small", "small", "medium", "large", "continental")
#   very_small  <   25,000 km^2   (island / city states)
#   small        25,000 - 100,000
#   medium      100,000 - 1,000,000
#   large     1,000,000 - 5,000,000
#   continental >  5,000,000 km^2 (BRA, CHN, USA, AUS, ...)