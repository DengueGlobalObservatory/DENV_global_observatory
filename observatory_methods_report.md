# DENV Global Observatory Methods (Implementation-Aligned)

This document describes the implemented methods in the DENV Global Observatory codebase and is written to support manuscript methods drafting (for example, a *Scientific Data* submission). It emphasizes reproducible algorithmic details, operational assumptions, and current outputs.

---

## 1) System Scope and Analytical Objective

The DENV Global Observatory is a Quarto-based, static web reporting system that ingests multi-source dengue surveillance data, estimates incomplete recent months, and reports country-, region-, and global-scale relative burden against historical seasonal baselines.

Core outputs:

- Pipeline outputs in `Output/YYYY_MM_DD/`: `DENV_cases_nowcast_output.csv`, `DENV_cases_backfill_output.csv`, `DENV_average_season.csv`, and `country_tracking.csv`.
- Validation outputs in `Output/validation/` and calibrated interval lookup in `Assets/Stable/calibrated_prediction_intervals.csv`.
- Dashboard site rendered to `docs/` from Quarto sources.

Primary orchestration scripts:

- `Scripts/V1_Pipeline.R` (operational data pipeline).
- `Scripts/V1_Dashboard_setup.R` (dashboard data assembly and text generation).
- `Scripts/validation/04_nowcast_validation.R` (retrospective validation and calibration).

---

## 2) Data Inputs and Acquisition

### 2.1 Data Sources

The pipeline loads four source streams in `Scripts/data_sourcing/01_dengue_data.R`:

1. PAHO crawler output (GitHub API).
2. WHO global dengue crawler output (GitHub API, latest `.xlsx` by filename date).
3. SEARO crawler output (GitHub API, latest timestamped `.csv`).
4. OpenDengue historical baseline file (`Assets/Stable/OD_maps/pred_downscale_with_ci_V3.csv`).

### 2.2 Retrieval Behavior

- API listing calls use GitHub endpoints, then select the most recent file by parsed date/timestamp.
- WHO and SEARO files are downloaded to temporary files and parsed into memory.
- OpenDengue data are read from the repository-local stable asset.
- Logging is active throughout execution (`Scripts/utils/logging.R`).

---

## 3) Historic Data Consolidation for Baseline Estimation

Implemented in `Scripts/data_sourcing/01_select_historic_data.R`.

### 3.1 Transformations

- OpenDengue transformed to canonical fields: `iso3`, `country`, `cases`, `Year`, `Month`, and `date`.
- WHO transformed and harmonized; country names normalized from ISO3 via `countrycode`, with explicit overrides (e.g., `MDR`, `MAF`).
- WHO missingness is interpolated via helper functions in `Scripts/data_sourcing/FUNCTIONS/00_WHO_data_processing_functions.R`.

### 3.2 Source Precedence and De-duplication

- OpenDengue and WHO are row-bound with a `source` tag.
- De-duplication key is `iso3 + date`.
- If duplicate monthly rows exist, OpenDengue is preferred over WHO.

### 3.3 Filtering

- Records constrained to `Year > 2009`.
- Country-years with all-zero incidence are removed.
- Resulting table (`full_data`) is passed to seasonal baseline modeling.

---

## 4) Seasonal Baseline Construction

Implemented in `Scripts/seasonal_baseline/02_identify_seasonal_baseline.R`.

### 4.1 Season Alignment

- For each country-year, a low-transmission month is identified (minimum monthly cases).
- A circular mean of low-month values is used to define each country’s mean seasonal anchor month.
- Calendar months are transformed to season-relative month index (`season_nMonth` in 1-12).
- A season string is created as `"YYYY/YYYY+1"` based on alignment.

### 4.2 Inclusion Filters

Three sequential filters are applied:

1. Complete seasons only (`12` observations per season).
2. Minimum seasonal burden threshold (`monthly_ave_case_threshold <- 3` cases/month on average).
3. Minimum historical support (`>= 3` qualifying seasons per country).

### 4.3 Baseline Quantities

For each country and season-month:

- Mean monthly cases (`Ave_season_monthly_cases`).
- Mean cumulative seasonal cases (`Ave_season_monthly_cum_cases`).
- Mean monthly proportion and cumulative proportion (`Ave_monthly_proportion`, `Ave_cum_monthly_proportion`).

Negative binomial (NB) models are fit separately for:

- Monthly counts (`nb_size`, `nb_mean`).
- Cumulative seasonal counts at each season-month (`nb_size_cum`, `nb_mean_cum`), used in severity classification.

---

## 5) Current-Season Source Selection and Backfill Integration

Current-season data assembly is handled upstream of nowcasting and saved as `current_data` (`Scripts/backfilling/02_PAHO_monthly_cases_and_source_selection.R` plus PAHO helpers in `Scripts/backfilling/FUNCTIONS/00_FUN_paho_data_process.R`).

Operational behavior:

- PAHO corrections are applied using empirical reporting factors where available.
- Multi-source monthly consolidation prioritizes non-missing records and de-prioritizes WHO when alternatives are present.
- Output is the harmonized monthly current-season table used for nowcasting.

---

## 6) Proportion-Based Nowcasting

Implemented in `Scripts/nowcasting/03_proportion_nowcast.R`.

### 6.1 Merge Strategy

- Seasonal baseline (`full_data_average_season`) and current data are merged by `iso3`, `season_nMonth`, and `Month`.
- `season_nMonth` is mapped onto current data from baseline month-to-season relations.
- Country names are reconciled post-merge to avoid name-based join loss.

### 6.2 Predicted Seasonal Total

For each `iso3 + season`:

- Identify the latest observed month in that season.
- Compute:
  - `last_cum_cases = cum_todate_cases_season[last_observed]`
  - `last_cum_prop = Ave_cum_monthly_proportion[last_observed]`
  - `Predicted_total_seasonal_cases = round(last_cum_cases / last_cum_prop)`

### 6.3 Filling Missing Recent Months

For rows with:

- `Data_status == "Unobserved"`,
- missing `cases`,
- valid `Ave_monthly_proportion`,
- and `date <= last_month_date` (where `last_month_date` corresponds to previous calendar month),

the nowcast is:

`estimated_cases = round(Predicted_total_seasonal_cases * Ave_monthly_proportion)`

Estimated rows are labeled `source = "Estimates"`.

### 6.4 Post-fill Diagnostics

- Cumulative values are recomputed after filling (`cum_todate_cases_season`, `cum_todate_cases_year`).
- A diagnostic post-fill total (`pred_total_post_fill`) is calculated but does not overwrite the frozen pre-fill predicted total.

---

## 7) Severity Classification

Implemented in `Scripts/V1_Pipeline.R` (Step 7), using cumulative seasonal progress.

For each country-season-month:

- Percentile is computed from the cumulative NB distribution:
  `percentile_cumulative = pnbinom(cum_todate_cases_season, size = nb_size_cum, mu = nb_mean_cum) * 100`
- Severity classes:
  - `<=5`: Extremely Low
  - `<=25`: Low
  - `<75`: Normal
  - `<95`: High
  - `>=95`: Extremely High

An approximate standardized deviation is also computed:

`z = (observed - mean) / sqrt(mean + mean^2 / size)`

where `observed = cum_todate_cases_season`, `mean = nb_mean_cum`, `size = nb_size_cum`.

---

## 8) Region Assignment and Final Pipeline Outputs

In `Scripts/V1_Pipeline.R` Step 6:

- Regions are joined from OpenDengue metadata using `iso3`.
- Missing region assignments are backfilled via `get_od_regions()` (`Assets/Stable/OD_maps/fn_OD_region.R`) when possible.
- Region factor levels are standardized to 8 observatory regions.

Outputs are written into date-stamped run directories:

- `DENV_cases_backfill_output.csv` (`current_data`)
- `DENV_cases_nowcast_output.csv` (`data_sev`)
- `DENV_average_season.csv` (`full_data_average_season`)
- `country_tracking.csv` (attrition/retention by processing step)

---

## 9) Validation and Calibration Framework

Implemented in:

- `Scripts/validation/04_nowcast_validation.R`
- `Scripts/validation/FUNCTIONS/00_FUN_validation_metrics.R`

### 9.1 Validation Dataset

`prepare_validation_dataset()` reruns data sourcing and seasonal baseline steps, then extracts:

- `country`, `iso3`, `Year`, `season`, `season_nMonth`, `Month`, `cases`
- `Actual_monthly_proportion`, `Actual_cum_monthly_proportion`

Region metadata are joined from OpenDengue country attributes.

### 9.2 Moving-Window Cross-Validation

`run_moving_window_validation()` loops by country and held-out season (requires at least 3 seasons):

- Training set: all seasons except test season.
- Test set: held-out season.
- For each cutoff month `1..11`:
  - predict seasonal total from cumulative observed proportion.
  - predict post-cutoff monthly counts as `predicted_total * Ave_monthly_proportion`.

Point and distributional diagnostics include:

- absolute error, squared error, APE, relative error, and log score.
- NB parametric interval summaries (95% and 50%) at month level.

### 9.3 Aggregated Validation Metrics

`compute_error_metrics()` returns:

- country-level metrics (`MAE`, `RMSE`, `RMSPE`, coverage, log score),
- cutoff-level metrics,
- country-cutoff metrics.

`classify_performance()` labels countries into `Good/Moderate/Poor` by RMSPE tertiles.

`season_month_accuracy_analysis()` identifies reliable horizon using `median_RMSPE < 0.25`.

### 9.4 Empirical Interval Calibration

`build_prediction_interval_lookup()` estimates quantiles of relative error (`q025_rel`, `q25_rel`, `q75_rel`, `q975_rel`) at:

1. country level (`iso3 + cutoff_month + horizon`) when `n_obs >= min_obs` (default `5`),
2. region fallback,
3. global fallback.

`apply_prediction_intervals()` maps these to absolute bounds:

- `lower_95 = max(0, predicted_cases * (1 + q025_rel))`
- `upper_95 = max(0, predicted_cases * (1 + q975_rel))`
- analogous for 50% intervals.

### 9.5 Snapshot Convergence and Backfill Stability

- `compute_snapshot_convergence()` compares nowcast estimates (`source == "Estimates"` and `Data_status == "Unobserved"`) across dated output folders and quantifies convergence toward final snapshots.
- `compute_backfill_assessment()` benchmarks historical backfill values against latest snapshot truth and reports source-level MAE/RMSE.

### 9.6 Validation Artifacts

Primary outputs:

- `validation_results_detail.csv`
- `table1_country_validation_summary.csv`
- `table2_cutoff_accuracy_summary.csv`
- `table3_country_cutoff_detail.csv`
- `table4_snapshot_convergence_summary.csv`
- `table5_calibrated_prediction_intervals.csv`
- `calibration_summary.csv`
- `snapshot_convergence_detail.csv`
- `backfill_assessment_paho.csv`
- `reliable_horizon.txt`
- `fig1` to `fig7` PNG files

Operational interval lookup is written to:

- `Assets/Stable/calibrated_prediction_intervals.csv`

---

## 10) Dashboard Rendering and Narrative Generation

Implemented in `Scripts/V1_Dashboard_setup.R`, sourced by rendered Quarto pages.

### 10.1 Data Loading and Temporal Masking

- Latest available dated output folder is discovered dynamically.
- `DENV_cases_nowcast_output.csv` is loaded.
- Any unnamed index column is removed if present.
- Future months are masked (`cases`, `cum_todate_cases_year`, `cum_todate_cases_season` set to `NA`) for dates beyond the latest reporting month (`recent_month = current_month - 1`, with January wrap logic).

### 10.2 Aggregated Objects for Pages

The setup script computes:

- `region_summary`
- `world_summary`
- `country_summary_df`
- `region_callouts`
- `world_summary_text`
- `all_country_plots`, `region_plot_list`, `world_plot`
- `top_severity_countries`
- `country_data_status`

### 10.3 Rule-Based Language Components

Representative thresholds:

- `ratio_phrase()`:
  - `>=1.3` well above
  - `>=1.1` slightly above
  - `<=0.7` well below
  - `<=0.9` slightly below
  - else near
- Season badge labels:
  - `>=1.2` high
  - `<=0.85` low
  - else near baseline
- Country `SeasonStatus` classification in summaries:
  - `>1.2` above
  - `<0.8` below
  - else near

---

## 11) Map Generation Methods

### 11.1 Regional and Global Ratio Maps

Implemented in `Scripts/figures/FUN_map.R` and runner scripts:

- `Scripts/figures/run_global_map.R`
- `Scripts/figures/run_region_maps.R`

Method:

- Build world geometries from Natural Earth (`scale = 10`) and add selected map units (notably overseas territories) for region completeness.
- Join observatory regions via `get_od_regions()`.
- Compute cumulative ratio by ISO3:
  `cum_ratio = sum(cases) / sum(Ave_season_monthly_cases)` up to target month, capped to `[0.5, 2]`.
- Render fill scale (`#7CC8AE` → `#F2D06B` → `#E07A6E`, midpoint `1`), with no-data in `grey80`.

Outputs:

- `Assets/Dynamic/global_ratio_map.png`
- `Assets/Dynamic/region_maps/*.png`

### 11.2 Country Context Maps

Implemented in `Scripts/figures/FUN_country_map.R`:

- `make_country_context_map(iso3, region)` highlights one country inside its regional extent.
- `save_country_context_map()` writes PNG files to `Assets/Stable/country_maps/{iso3}.png`.

These are consumed by country pages as static locator assets.

---

## 12) Quarto Site and Country-Page Architecture

Site configuration is defined in `_quarto.yml`:

- `project.output-dir: docs`
- dynamic render list includes `pages/country/*.qmd` (not only pilot pages).

Country pages are generated from config:

- `Scripts/country/generate_country_pages.R` reads `pages/country/country-config.csv`.
- For each enabled row, it writes `pages/country/{slug}.qmd` that includes `pages/country/_country-template.qmd`.

Country template methods (`_country-template.qmd`):

- Sources `Scripts/V1_Dashboard_setup.R`.
- Builds country-year monthly table with statuses (`Observed`, `Estimated`, `Unobserved`).
- Computes country-page uncertainty bounds for estimated points using NB quantiles with mean set to nowcasted monthly cases:
  - `lower95 = qnbinom(0.025, size = nb_size, mu = cases)`
  - `upper95 = qnbinom(0.975, size = nb_size, mu = cases)`
- Renders a client-side Chart.js panel:
  - observed vs estimated line styling,
  - 95% uncertainty ribbon/whiskers for estimated months,
  - selectable comparator series (5-year average + prior years).

---

## 13) Reproducibility Notes for Manuscript Methods

When adapting this for peer-reviewed methods text, include:

- execution date of pipeline run and the corresponding `Output/YYYY_MM_DD/` snapshot used,
- software environment (R version and package versions),
- source repositories and retrieval windows,
- explicit definition of observed vs estimated rows (`Data_status`, `source`),
- ratio capping convention (`[0.5, 2]`) and threshold rules used for communication layers.

Recommended minimum citation set in methods:

- `Scripts/V1_Pipeline.R`
- `Scripts/seasonal_baseline/02_identify_seasonal_baseline.R`
- `Scripts/nowcasting/03_proportion_nowcast.R`
- `Scripts/validation/FUNCTIONS/00_FUN_validation_metrics.R`
- `Scripts/V1_Dashboard_setup.R`
- `_quarto.yml` and `pages/country/_country-template.qmd`

---

## 14) File Index (Current)

### Pipeline

- `Scripts/V1_Pipeline.R`
- `Scripts/data_sourcing/01_dengue_data.R`
- `Scripts/data_sourcing/01_select_historic_data.R`
- `Scripts/seasonal_baseline/02_identify_seasonal_baseline.R`
- `Scripts/backfilling/02_PAHO_monthly_cases_and_source_selection.R`
- `Scripts/nowcasting/03_proportion_nowcast.R`

### Validation

- `Scripts/validation/04_nowcast_validation.R`
- `Scripts/validation/FUNCTIONS/00_FUN_validation_metrics.R`

### Dashboard and Mapping

- `Scripts/V1_Dashboard_setup.R`
- `Scripts/figures/FUN_utility.R`
- `Scripts/figures/Radial.R`
- `Scripts/figures/FUN_map.R`
- `Scripts/figures/FUN_country_map.R`
- `Scripts/figures/run_global_map.R`
- `Scripts/figures/run_region_maps.R`

### Country Page Generation

- `Scripts/country/generate_country_pages.R`
- `pages/country/country-config.csv`
- `pages/country/_country-template.qmd`
- `_quarto.yml`

---

*Last updated: 21 April 2026 (code-aligned refresh).*
