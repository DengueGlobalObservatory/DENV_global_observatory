## `00_FUN_validation_metrics.R` — Function Reference

Documentation for functions in `Scripts/validation/FUNCTIONS/00_FUN_validation_metrics.R`.

---

### `safe_divide(num, den)`

**Title:** Safe division with NA/zero guard

**Description:** Computes `num / den` while returning `NA_real_` when the denominator is missing or zero.

**Usage:** `safe_divide(num, den)`

**Arguments:**

- `num`: Numeric vector. Numerator.
- `den`: Numeric vector. Denominator.

**Value:** Numeric vector with `NA_real_` where `den` is `NA` or `0`.

**Examples:**

```r
safe_divide(10, 2)
safe_divide(c(1, 2, 3), c(1, 0, NA))
```

**Reviewed:** ✅

---

### `prepare_validation_dataset()`

**Title:** Prepare retrospective validation dataset (pipeline-based)

**Description:** Runs the data sourcing and seasonal baseline scripts to construct a season-aligned dataset suitable for moving-window retrospective validation of the proportion-based nowcasting method.

**Usage:** `prepare_validation_dataset()`

**Details:**

- Sources:
  - `Scripts/data_sourcing/01_dengue_data.R`
  - `Scripts/data_sourcing/01_select_historic_data.R`
  - `Scripts/seasonal_baseline/02_identify_seasonal_baseline.R`
- Expects `full_data_season_monthly_proportions` to exist afterward.
- Builds `validation_data` with columns:
  `country`, `iso3`, `season`, `season_nMonth`, `Month`, `cases`,
  `Actual_monthly_proportion`, `Actual_cum_monthly_proportion`.
- Joins `Region` metadata from `OD_national` (OpenDengue).

**Value:** Named list:

- `validation_data`: tibble of season-aligned country-season-month rows.
- `full_data`: the `full_data_season_monthly_proportions` object.

**Examples:**

```r
# not run
prep <- prepare_validation_dataset()
validation_data <- prep$validation_data
```

**Reviewed:** ✅
    annotations added to function 
    code reviewed. I added "Year" to the validation_data 
    function run -- expected output generated w/o errors or warnings
---

### `fit_baseline_profile(train_df)`

**Title:** Fit average seasonal proportion profile (training set)

**Description:** Computes the baseline seasonal profile by averaging actual monthly and cumulative monthly proportions over seasons, indexed by `season_nMonth`.

**Usage:** `fit_baseline_profile(train_df)`

**Arguments:**

- `train_df`: Data frame with `season_nMonth`, `Actual_monthly_proportion`, `Actual_cum_monthly_proportion`.

**Value:** Tibble with `season_nMonth`, `Ave_monthly_proportion`, `Ave_cum_monthly_proportion`.

**Examples:**

```r
# not run
baseline <- fit_baseline_profile(train_df)
```
**Reviewed:** ✅
    function run - works - does not include the country grouping

---

### `fit_nb_month_distribution(train_df)`

**Title:** Fit negative binomial distributions for monthly cases by season month

**Description:** Fits NB (`glm.nb`) to monthly `cases` at each `season_nMonth` in the training set.

**Usage:** `fit_nb_month_distribution(train_df)`

**Arguments:**

- `train_df`: Data frame with `season_nMonth` and `cases`.

**Value:** Tibble with `season_nMonth`, `nb_size` (theta), `nb_mu` (mu).

**Details:** Fits are skipped (NA) when fewer than 3 non-missing non-negative observations exist or when sum of cases is zero.

**Examples:**

```r
# not run
nb_dist <- fit_nb_month_distribution(train_df)
```
**Reviewed:** ✅
    function run - works - does not include the country grouping

---

### `compute_nowcast_at_cutoff(country_df, test_season, cutoff_month)`

**Title:** Compute proportion-based nowcast for one country-season at one cutoff

**Description:** Simulates nowcast at `cutoff_month` using a leave-one-season-out baseline computed from other seasons for the same country.

**Usage:** `compute_nowcast_at_cutoff(country_df, test_season, cutoff_month)`

**Arguments:**

- `country_df`: Single-ISO3 dataset with `season`, `season_nMonth`, `Month`, `cases`, `Actual_monthly_proportion`, `Actual_cum_monthly_proportion`.
- `test_season`: Held-out season identifier (e.g. `"2023/2024"`).
- `cutoff_month`: Integer 1–11.

**Value:** Tibble for months `season_nMonth > cutoff_month` with (key columns):

- `predicted_total`, `predicted_cases`, `actual_cases`
- `cutoff_month`, `horizon`
- NB params `nb_size`, `nb_mu` and quantiles `lower_95_nb`, `upper_95_nb`, `lower_50_nb`, `upper_50_nb`
- `logscore` (NB log predictive density at truth)

**Details:**

Predicted seasonal total:
\[
\hat{T} = C_{\le k} / \bar{P}_{\le k}
\]

Returns empty tibble if the baseline cumulative proportion at cutoff is missing/non-positive.

**Examples:**

```r
# not run
pred <- compute_nowcast_at_cutoff(country_df, test_season = "2022/2023", cutoff_month = 6)
```

**Reviewed:** ✅
  code reviewed
  function runs - have not done for a single country yet
  
---

### `run_moving_window_validation(validation_data)`

**Title:** Run leave-one-season-out moving-window retrospective nowcast validation

**Description:** For each country and eligible season, runs cutoffs 1–11, nowcasts unobserved months, and returns predictions with errors.

**Usage:** `run_moving_window_validation(validation_data)`

**Arguments:**

- `validation_data`: Tibble with `iso3`, `country`, `Region`, `season`, `season_nMonth`, `Month`, `cases`, `Actual_*` proportions.

**Value:** Tibble with predictions and derived errors:

- `absolute_error`, `abs_error`, `sq_error`
- `ape` and `relative_error` (NA when `actual_cases == 0`)

**Details:**

- Skips countries with <3 seasons.
- Requires at least 2 distinct training seasons when holding out a season.

**Examples:**

```r
# not run
results <- run_moving_window_validation(validation_data)
```

**Reviewed:** ✅

---

### `compute_error_metrics(validation_results)`

**Title:** Compute validation error metrics and summary tables

**Description:** Aggregates results into summary tables by country, cutoff month, and country x cutoff.

**Usage:** `compute_error_metrics(validation_results)`

**Arguments:**

- `validation_results`: Output of `run_moving_window_validation()`.

**Value:** Named list of tibbles:

- `by_country`
- `by_cutoff`
- `by_country_cutoff`

**Details:** Includes MAE, RMSE, RMSPE, NB interval coverage (50/95), and mean logscore.

**Examples:**

```r
# not run
metrics <- compute_error_metrics(results)
metrics$by_country
```

**Reviewed:** ✅

---

### `classify_performance(country_metrics, validation_data)`

**Title:** Classify country nowcast performance and compute correlates

**Description:** Creates performance tiers based on RMSPE tertiles and computes correlates (burden and seasonal profile variability).

**Usage:** `classify_performance(country_metrics, validation_data)`

**Arguments:**

- `country_metrics`: Typically `metrics$by_country`, includes `iso3` and `RMSPE`.
- `validation_data`: Season-aligned dataset with `cases` and `Actual_monthly_proportion`.

**Value:** `country_metrics` with:

- `seasonal_profile_cv`
- `mean_monthly_cases`
- `burden_group` (High/Low by median)
- `performance_tier` (Good/Moderate/Poor)

**Examples:**

```r
# not run
country_metrics2 <- classify_performance(metrics$by_country, validation_data)
```

**Reviewed:** ✅

---

### `season_month_accuracy_analysis(by_cutoff, threshold = 0.25)`

**Title:** Analyze accuracy by cutoff month and identify a reliable horizon

**Description:** Identifies earliest cutoff month where `median_RMSPE < threshold`.

**Usage:** `season_month_accuracy_analysis(by_cutoff, threshold = 0.25)`

**Arguments:**

- `by_cutoff`: Typically `metrics$by_cutoff`, includes `cutoff_month` and `median_RMSPE`.
- `threshold`: Numeric scalar.

**Value:** List with `by_cutoff` and `reliable_horizon` (or NA).

**Examples:**

```r
# not run
cutoff_analysis <- season_month_accuracy_analysis(metrics$by_cutoff, threshold = 0.25)
```

---

### `build_prediction_interval_lookup(validation_results, min_obs = 5)`

**Title:** Build calibrated prediction interval lookup from validation residuals

**Description:** Computes empirical quantiles of `relative_error` by (`iso3`, `cutoff_month`, `horizon`) with region/global fallback when sample size is small.

**Usage:** `build_prediction_interval_lookup(validation_results, min_obs = 5)`

**Arguments:**

- `validation_results`: Validation predictions containing `relative_error`, `iso3`, `Region`, `cutoff_month`, `horizon`.
- `min_obs`: Minimum country-specific residual count.

**Value:** Tibble with:
`iso3`, `cutoff_month`, `horizon`, `n_obs`, `q025_rel`, `q25_rel`, `q75_rel`, `q975_rel`.

**Details:** Fallback is country → region → global.

**Examples:**

```r
# not run
lookup <- build_prediction_interval_lookup(results, min_obs = 5)
```

---

### `apply_prediction_intervals(df, lookup)`

**Title:** Attach calibrated prediction interval bounds to nowcast predictions

**Description:** Joins lookup quantiles and converts them into case-space interval bounds around `predicted_cases`.

**Usage:** `apply_prediction_intervals(df, lookup)`

**Arguments:**

- `df`: Tibble with `iso3`, `cutoff_month`, `horizon`, `predicted_cases`.
- `lookup`: Output of `build_prediction_interval_lookup()`.

**Value:** `df` with `lower_95`, `upper_95`, `lower_50`, `upper_50`.

**Examples:**

```r
# not run
df2 <- apply_prediction_intervals(results, lookup)
```

---

### `coverage_calibration(validation_results, lookup)`

**Title:** Compute empirical coverage of calibrated prediction intervals

**Description:** Applies calibrated intervals and computes empirical coverage for 50% and 95% bounds.

**Usage:** `coverage_calibration(validation_results, lookup)`

**Arguments:**

- `validation_results`: Tibble with `actual_cases` and interval join keys.
- `lookup`: Calibrated lookup.

**Value:** Tibble with `interval`, `nominal`, `empirical`.

**Examples:**

```r
# not run
calibration <- coverage_calibration(results, lookup)
```

---

### `load_snapshot_nowcast(snapshot_dir)`

**Title:** Load nowcast estimates from one pipeline output snapshot

**Description:** Reads `DENV_cases_nowcast_output.csv` and returns `Unobserved` rows where `source == "Estimates"`.

**Usage:** `load_snapshot_nowcast(snapshot_dir)`

**Arguments:**

- `snapshot_dir`: Path to an `Output/YYYY_MM_DD` snapshot directory.

**Value:** Tibble with `snapshot_date`, `iso3`, `country`, `Year`, `Month`, `cases_nowcast` (or empty tibble if missing file).

**Examples:**

```r
# not run
snap <- load_snapshot_nowcast("Output/2026_03_24")
```

---

### `compute_snapshot_convergence(output_root = "Output")`

**Title:** Compare nowcast convergence across pipeline output snapshots

**Description:** Tracks how nowcast estimates change across dated snapshots and compares each snapshot against the latest snapshot for the same country-month.

**Usage:** `compute_snapshot_convergence(output_root = "Output")`

**Arguments:**

- `output_root`: Directory containing `YYYY_MM_DD` snapshot folders.

**Value:** List with:

- `detail`: per-snapshot estimates + diffs vs latest
- `summary`: per country-month convergence summary

**Examples:**

```r
# not run
conv <- compute_snapshot_convergence("Output")
```

---

### `compute_backfill_assessment(output_root = "Output", paho_iso3 = NULL)`

**Title:** Assess backfill series against final snapshot values (PAHO-focused)

**Description:** Compares snapshot backfill values to the latest snapshot values, summarised by `source`.

**Usage:** `compute_backfill_assessment(output_root = "Output", paho_iso3 = NULL)`

**Arguments:**

- `output_root`: Directory containing `YYYY_MM_DD` snapshot folders.
- `paho_iso3`: Optional ISO3 restriction; if `NULL`, inferred from rows with `source == "PAHO"`.

**Value:** Tibble summarised by `source` with `n`, `MAE_vs_final`, `RMSE_vs_final`.

**Examples:**

```r
# not run
bf <- compute_backfill_assessment("Output")
```

---

### `build_world_sf()`

**Title:** Build a world `sf` layer keyed by ISO3

**Description:** Returns Natural Earth country polygons with ISO3 codes for mapping joins.

**Usage:** `build_world_sf()`

**Value:** `sf` object with `iso_a3` and `geometry`.

**Examples:**

```r
# not run
world <- build_world_sf()
```
