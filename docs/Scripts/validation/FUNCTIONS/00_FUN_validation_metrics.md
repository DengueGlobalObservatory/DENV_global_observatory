## `00_FUN_validation_metrics.R` — function reference

Documentation for [`00_FUN_validation_metrics.R`](00_FUN_validation_metrics.R). These helpers are used only inside the leave-one-season-out loop in `03_nowcast_validation_ind.R`. Summaries, quantiles, coverage, snapshots, and figures are implemented in the action scripts.

---

### `fit_baseline_profile(train_df)`

**Description:** Computes the mean monthly proportion and mean cumulative monthly proportion by `season_nMonth` over the rows in `train_df` (typically all seasons for a country except the held-out season).

**Arguments**

- `train_df`: Data frame with `season_nMonth`, `Actual_monthly_proportion`, `Actual_cum_monthly_proportion`.

**Value:** Tibble with columns `season_nMonth`, `Ave_monthly_proportion`, `Ave_cum_monthly_proportion`, sorted by `season_nMonth`.

---

### `nowcast_one_cutoff(test_df, baseline, cutoff_k)`

**Description:** For one complete test season and one cutoff `k` (1..11), estimates total seasonal cases from cumulative observed cases through month `k` divided by the training mean cumulative proportion at `k`, then allocates predicted counts to each future month using training mean monthly proportions.

**Arguments**

- `test_df`: Rows for a single `season` (12 `season_nMonth` rows expected).
- `baseline`: Output of `fit_baseline_profile()` on training seasons only.
- `cutoff_k`: Integer in 1..11; months with `season_nMonth <= k` are treated as observed.

**Value:** Tibble with columns `cutoff_month`, `prediction_month`, `Month`, `predicted_total`, `predicted_cases` (rounded to integers), `actual_cases`. Returns zero rows if the cumulative proportion at `cutoff_k` is missing or non-positive.

---

### Related scripts

- [`../ORC_nowcast_validation.R`](../ORC_nowcast_validation.R) — orchestrator: runs the steps below in order.
- [`../03_nowcast_validation_ind.R`](../03_nowcast_validation_ind.R) — builds `Output/validation/validation_detail.csv`.
- [`../03_nowcast_validation_summary.R`](../03_nowcast_validation_summary.R) — summaries, quantiles, calibrated lookup, coverage.
- [`../03_nowcast_validation_snapshots.R`](../03_nowcast_validation_snapshots.R) — snapshot convergence.
- [`../04_nowcast_validation_FIG.R`](../04_nowcast_validation_FIG.R) — publication figures.

Ad hoc Brazil-only test (legacy helpers): [`../04_nowcast_validation_BRA_test.R`](../04_nowcast_validation_BRA_test.R) sources [`00_FUN_validation_metrics_legacy_BRA.R`](00_FUN_validation_metrics_legacy_BRA.R), not this slim file.
