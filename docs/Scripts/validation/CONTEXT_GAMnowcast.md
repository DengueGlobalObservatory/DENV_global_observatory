# GDO nowcast validation — GAM prototype (context / labbook)

**Date:** 2026-06-02  
**Project:** DENV Global Observatory (`DENV_global_observatory`)  
**Tags:** #gdo #nowcast #validation #gam #mgcv #proportion-method #loso  
**Status:** Prototype complete in validation only; **not** integrated into `V1_Pipeline.R` or production nowcasting.

---

## Motivation

- Retrospective validation of the **empirical proportion-based nowcast** showed very poor **mean APE (MAPE)** on pooled rows (order of hundreds of percent in `summary_country.csv`), driven by small actual counts and unstable seasonal-proportion denominators.
- Goal: test whether a **model-based** alternative that **still uses seasonal proportions** (`P_k`, `p_m`) but learns nonlinear relationships can improve metrics, especially **MAPE**, without changing the live GDO pipeline yet.
- Peer-review deadline pressure → start with lightweight **negative-binomial GAM** (`mgcv::bam`), defer Bayesian / ML until a GAM shows signal.

---

## Empirical baseline (production logic, validation mirror)

### Two-step formula (per country, held-out season, cutoff month `k`, prediction month `m > k`)

```text
T_hat = C_{c,s,k} / P_{c,-s,k}
predicted_cases = round(T_hat * p_{c,-s,m})
```

| Symbol | Meaning |
|--------|---------|
| `C_{c,s,k}` | Cumulative observed cases in held-out season `s` through season month `k` |
| `P_{c,-s,k}` | Mean **cumulative** seasonal proportion at month `k`, from training seasons (LOSO: all seasons of country `c` except `s`) |
| `p_{c,-s,m}` | Mean **monthly** seasonal proportion at month `m`, same training pool |
| `Y_{c,s,k,m}` | Actual cases in month `m` (validation only; unknown at real nowcast time) |

### Validation design (existing)

- **Script:** `Scripts/validation/03_nowcast_validation_ind.R`
- **Input:** latest `Output/YYYY_MM_DD/full_data_season_monthly_proportions.csv` (from pipeline Step 8)
- **Design:** per country with ≥3 seasons → **leave-one-season-out (LOSO)** → cutoffs `k = 1..11` → predict all `m > k`
- **Output:** `Output/validation/validation_detail.csv`
- **Orchestrator:** `Scripts/validation/ORC_nowcast_validation.R` (empirical path only)

### MAPE / APE units in empirical files

- Column `absolute_percent_error` in `validation_detail.csv` is stored as **percent** (×100), e.g. relative error 0.29 → **29.41** in file.
- `summary_country.csv` uses `MAPE = mean(absolute_percent_error)` → also **percent units**.

---

## Proposed direct GAM (Option A — final intent)

**Do not** use empirical prediction as an offset. **Directly** model monthly cases with the same information as predictors.

### Publication-style model (first version)

```text
Y_{c,s,k,m} ~ NegBin(μ_{c,s,k,m}, θ)

log(μ_{c,s,k,m}) =
  β₀
  + f₁{log(1 + C_{c,s,k})}
  + f₂{log(1 + L_{c,s,k})}
  + f₃(m − k)                    # lead_time
  + f₄(P_{c,-s,k})
  + f₅(p_{c,-s,m})
  + f₆(m)                        # season_month (prediction month in season coordinates)
  + f₇(k)                        # cutoff month
  + γ_{r[c]}                     # Region fixed effect
```

| Predictor (code name) | Role |
|----------------------|------|
| `observed_cumulative_cases_at_cutoff` | `C_{c,s,k}` |
| `last_observed_cases` | `L_{c,s,k}` (cases in month `k`) |
| `Ave_cum_monthly_proportion_at_cutoff` | `P_{c,-s,k}` |
| `Ave_monthly_proportion_missing` | `p_{c,-s,m}` |
| `lead_time` | `m - k` (season-month index; wraps across year boundary in data) |
| `season_month` | `prediction_month` (= `season_nMonth` at target month) |
| `prediction_calendar_month` | calendar `Month` (cyclic 1–12) |
| `cutoff_month_num` | `k` (cyclic 1–12) |
| `Region_factor` | OpenDengue region |

**Still “proportion-based”** in the sense that `P_k` and `p_m` are explicit smooth terms; the model does **not** impose `Y = (C/P)·p`.

---

## Implementation (validation-only sandbox)

### New / modified files

| File | Role |
|------|------|
| `Scripts/validation/FUNCTIONS/00_FUN_validation_metrics.R` | Added GAM helpers: `build_gam_rows_one_holdout`, `build_gam_rows_all`, `add_gam_features`, `fit_gam_nowcast`, `predict_gam_nowcast` |
| `Scripts/validation/03_GAMnowcast_validation_ind.R` | Independent GAM LOSO validation |
| `Scripts/validation/03_GAMnowcast_validation_comparison.R` | Multi-method comparison (recomputes APE in **percent**) |
| `Output/validation/gam_prototype/` | All GAM outputs (does not overwrite `Assets/Stable/calibrated_prediction_intervals.csv`) |

**Left unchanged:** `03_nowcast_validation_ind.R`, `03_proportion_nowcast.R`, `V1_Pipeline.R`.

### Row construction

- Same LOSO eligibility as empirical (`MIN_SEASONS = 3`, `MIN_TRAIN_SEASONS = 2`).
- One row per `(iso3, season, cutoff_month, prediction_month)` with fold-safe `P_k`, `p_m` from `fit_baseline_profile(train_df)` on non-held-out seasons only.
- `iso3_factor` levels fixed across all rows before fitting (needed for prediction on held-out country in RE / factor-smooth models).

### Fitting strategy

| Setting | Value | Notes |
|---------|-------|-------|
| Default `STRATEGY` | **`loso`** | One `bam()` per **(country, season)** fold (~1081 fits); required for country-specific terms |
| Deprecated quick run | `loco` | One fit per held-out **country** (~97 fits); **invalid** for `iso3_re` / `iso3_smooth` (held-out country has no RE / no country smooth) |
| Parallelism | `mclapply`, `GAM_CORES=8` | ~1 `bam` thread per worker |
| Estimator | `mgcv::bam`, `family = nb()`, `method = "fREML"`, `discrete = TRUE` | Fallback to factor months if cyclic smooths fail |

### Model variants (`GAM_VARIANT` env var)

| Variant | `iso3_term` | Formula change |
|---------|-------------|----------------|
| `base` | `none` | Global `s(season_month, bs="cc")` + shared smooths |
| `iso3_re` | `re` | `+ s(iso3_factor, bs="re")` random intercept |
| `iso3_smooth` | `smooth` | Replace global season smooth with **`s(season_month, iso3_factor, bs="fs", xt=list(bs="cc"))`** — per-country cyclic seasonal shape, shrunk toward common shape |

**Not yet implemented:** hybrid `offset(log(empirical_pred))` variant.

### Run commands (from repo root)

```bash
# Strict LOSO — base
GAM_STRATEGY=loso GAM_VARIANT=base GAM_CORES=8 \
  Rscript Scripts/validation/03_GAMnowcast_validation_ind.R

# Strict LOSO — country random intercept
GAM_STRATEGY=loso GAM_VARIANT=iso3_re GAM_CORES=8 \
  Rscript Scripts/validation/03_GAMnowcast_validation_ind.R

# Strict LOSO — country-specific seasonal shape (factor smooth)
GAM_STRATEGY=loso GAM_VARIANT=iso3_smooth GAM_CORES=8 \
  Rscript Scripts/validation/03_GAMnowcast_validation_ind.R

# Comparison (auto-discovers validation_detail_gam_*.csv)
Rscript Scripts/validation/03_GAMnowcast_validation_comparison.R
```

### Runtime (2026-06-02, M-series Mac, 8 cores)

| Variant | Wall time | ~sec/fold |
|---------|-----------|-----------|
| `base` | ~8.8 min | ~2 s |
| `iso3_re` | ~8.1 min | ~2 s |
| `iso3_smooth` | ~35.9 min | ~12 s |

Data: `Output/2026_06_02/full_data_season_monthly_proportions.csv` → **71,346** prediction rows, **97** countries, **1081** country–season folds.

---

## Results (strict LOSO, matched rows n = 71,346)

**Source:** `Output/validation/gam_prototype/gam_vs_empirical_overall.csv`  
**APE/MAPE:** recomputed in comparison script as **percent** (×100), consistent with empirical `summary_country.csv`.

| method | MAPE (%) | median APE (%) | p95 APE (%) | p99 APE (%) | MAE | RMSE/μ | MRE signed (%) |
|--------|----------|----------------|-------------|-------------|-----|--------|----------------|
| **empirical** | **430.85** | **50.04** | **721.83** | **4700** | **2434** | **5.08** | 382.6 |
| gam_base | 983.61 | 85.42 | 3165.95 | 14100 | 2888 | 6.43 | 960.9 |
| gam_iso3_re | 680.24 | 71.43 | 2130 | 10100 | 2798 | 6.63 | 652.9 |
| gam_iso3_smooth | 554.28 | 67.51 | 1609.24 | 7716 | 2828 | 6.44 | 525.0 |

### Interpretation

1. **Empirical baseline still wins globally** on MAPE, median APE, MAE, and RMSE-scaled under strict LOSO.
2. **GAM variants improve monotonically:** base → iso3_re → iso3_smooth on MAPE and tail APE (p95, p99).
3. **Country-specific seasonal shape (`iso3_smooth`)** matters more than random intercept alone for tail behaviour.
4. **Earlier “MAPE 9.88 vs 430” was wrong** — caused by comparing GAM APE in **decimal** to empirical column in **percent** during an early LOCO run. Always use `03_GAMnowcast_validation_comparison.R` or recompute `100 * abs(pred - actual) / actual`.

### Where `gam_iso3_smooth` beats empirical (MAPE, country-level)

**25 / 97** countries (examples with large MAPE reduction):

| country | empirical MAPE (%) | iso3_smooth MAPE (%) |
|---------|-------------------|----------------------|
| Seychelles | 6536 | 4103 |
| Senegal | 3645 | 1778 |
| Guadeloupe | 2434 | 989 |
| Kiribati | 3723 | 2508 |
| Aruba | 1947 | 1080 |
| Bhutan | 1578 | 829 |
| Martinique | 978 | 419 |
| Ethiopia | 696 | 384 |
| Afghanistan | 164 | 94 |
| Indonesia | 37.2 | 37.1 |

Wins concentrated in **small/moderate mean monthly burden** where empirical proportion formula is unstable (`P_k` small, noisy seasons).

### Where empirical still dominates

- **High-burden countries** (Brazil, Argentina, etc.): pooled GAM under-predicts relative scale even with country smooths; MAE rises.
- **Low actual-count buckets** (comparison `gam_vs_empirical_lowcount_overpred.csv`):

| actual_bucket | n | empirical median APE (%) | iso3_smooth median APE (%) |
|---------------|---|--------------------------|----------------------------|
| 0 | 3859 | NA | NA |
| 1–4 | 4852 | 200 | 950 |
| 5–19 | 8643 | 57.1 | 200 |
| 20–99 | 12033 | 60.9 | 83.9 |
| **100+** | **41959** | **44.5** | **51.0** |

Bulk of rows (`100+`) → GAM median APE **close** to empirical; weakness remains at **very low counts**.

### MAE up while MAPE down (LOCO run — illustrative only)

Under **leave-one-country-out** (invalid for country RE, but ran once), GAM looked artificially strong on MAPE due to units bug. Mechanism still relevant:

- **MAE** = mean absolute error in **cases** → dominated by Brazil-scale rows.
- **MAPE** = mean **percent** error → dominated by small-`actual` rows.
- Empirical: huge relative errors on low counts; GAM pooled model shrinks toward global structure → better % on small rows, worse absolute error on epidemic-scale rows.

---

## Design decisions log

- [x] Direct NB-GAM on `actual_cases`, not offset correction of empirical `B_i`.
- [x] Separate scripts `03_GAMnowcast_validation_ind.R` + `03_GAMnowcast_validation_comparison.R`.
- [x] Default validation = **LOSO** (align with empirical + enable country terms).
- [x] `iso3_re` = random intercept only.
- [x] `iso3_smooth` = `bs="fs"` factor smooth on `(season_month, iso3)` — **best GAM so far**.
- [ ] Hybrid: `offset(log(empirical_pred))` + reduced smooth set — **recommended next**.
- [ ] Report MAPE on `actual_cases >= 5` (or by bucket) in manuscript — standard for count data.
- [ ] Optional: `te(log_cum_cases, p_target)` after additive model stable.

---

## Output inventory (`Output/validation/gam_prototype/`)

| File | Contents |
|------|----------|
| `validation_detail_gam_base.csv` | Row-level LOSO predictions, base GAM |
| `validation_detail_gam_iso3_re.csv` | Row-level, iso3 random intercept |
| `validation_detail_gam_iso3_smooth.csv` | Row-level, factor-smooth season × country |
| `gam_vs_empirical_overall.csv` | Pooled metrics, all methods |
| `gam_vs_empirical_by_country.csv` | Per-country, long format |
| `gam_vs_empirical_by_pair.csv` | By `(cutoff_month, prediction_month)` |
| `gam_vs_empirical_by_lead_time.csv` | By lead time |
| `gam_vs_empirical_lowcount_overpred.csv` | Bucketed low-count behaviour |
| `gam_vs_empirical_long_paired.csv` | Long paired rows for plotting |

Deleted obsolete: `validation_detail_gam.csv` (early LOCO, ambiguous units).

---

## Open questions for future sessions

1. Can **hybrid offset + iso3_smooth** beat empirical on **both** MAPE (≥5 cases) and MAE?
2. Should production move to **per-country empirical profile only** (already implicit in validation) vs pooled GAM?
3. Is **LOSO per country** the right operational analogue of “nowcast this season with history from other seasons” — yes for methods; GAM still pools **across countries** within each fold (by design).
4. **Lead_time** defined as `prediction_month - cutoff_month` on season-month index 1–12 — check wrap behaviour for months crossing season boundary in edge countries.
5. **Zero actual months:** both methods struggle; consider zero-inflated NB or separate model for reporting vs imputation.

---

## Manuscript / methods snippet (draft)

> We evaluated a negative-binomial generalized additive model as an alternative to the deterministic proportion nowcast. For each country and held-out epidemic season, training seasons supplied leave-one-season-out estimates of mean cumulative and monthly seasonal proportions at the reporting cutoff. The model predicted monthly case counts as a function of cumulative cases to date, last observed monthly cases, lead time, cutoff and target season months, regional fixed effects, and (in extended specifications) country-specific random intercepts or country-specific cyclic seasonal shapes via factor-smooth interactions. Retrospective validation used identical folds and cutoffs as the empirical baseline (n = 71,346 prediction rows). Under strict leave-one-season-out evaluation, the empirical method achieved lower global MAPE (431% vs 554% for the best GAM variant) and MAE (2434 vs 2828 cases), but the factor-smooth GAM reduced MAPE in 25 of 97 countries, predominantly those with low mean monthly burden where proportional scaling is unstable.

---

## Related paths

| Path | Purpose |
|------|---------|
| `Scripts/validation/03_nowcast_validation_ind.R` | Empirical baseline validation |
| `Scripts/validation/03_GAMnowcast_validation_ind.R` | GAM LOSO validation |
| `Scripts/validation/03_GAMnowcast_validation_comparison.R` | Multi-method comparison |
| `Scripts/validation/FUNCTIONS/00_FUN_validation_metrics.R` | Shared + GAM helpers |
| `Output/validation/validation_detail.csv` | Empirical row-level output |
| `Output/validation/gam_prototype/` | GAM outputs and comparison tables |
