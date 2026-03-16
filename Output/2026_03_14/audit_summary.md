# Global Dengue Observatory – Country & Region Audit

**Data used:** `Output/2026_03_14/DENV_cases_nowcast_output.csv`  
**Run:** 2026-03-14 (local)

---

## 1. Countries with missing or partial data (accompanying text)

These are the **7 countries** that have a plot on the All Countries page but **lack full current-year data**. The “accompanying text” is what is shown with the plot (from `severity_country_blurb`).

| Country | Region | Type of missingness | Accompanying text |
|--------|--------|----------------------|-------------------|
| Bahamas | Caribbean | No current year data reported | Current year data for Bahamas has not been reported yet. We do not have enough data to estimate current cases. |
| Bonaire, Saint Eustatius And Saba | Caribbean | No current year data reported | Current year data for Bonaire, Saint Eustatius And Saba has not been reported yet. We do not have enough data to estimate current cases. |
| Curacao | Caribbean | No current year data reported | Current year data for Curacao has not been reported yet. We do not have enough data to estimate current cases. |
| Haiti | Caribbean | No current year data reported | Current year data for Haiti has not been reported yet. We do not have enough data to estimate current cases. |
| Malaysia | East & Southeast Asia | No current year data reported | Current year data for Malaysia has not been reported yet. We do not have enough data to estimate current cases. |
| Nicaragua | North & Central America | No current year data reported | Current year data for Nicaragua has not been reported yet. We do not have enough data to estimate current cases. |
| Venezuela | South America | No current year data reported | Current year data for Venezuela has not been reported yet. We do not have enough data to estimate current cases. |

**Note:** There are no countries in a “Data still loading” or “Cases being compiled” state in this run; all non–full-data cases are “No current year data reported.”

---

## 2. Full table: all 88 countries (region, type of missingness, accompanying text)

The full table with all countries on the All Countries page, their region, type of missingness, and accompanying text is in:

**`Output/2026_03_14/audit_country_missingness.csv`**

Columns: `country`, `region`, `type_of_missingness`, `accompanying_text`, `data_status_message`.

- **81 countries** have **Full data** (current-year cases and ratio; full blurb with cases and severity).
- **7 countries** have **No current year data reported** (blurb: “Current year data for … has not been reported yet. We do not have enough data to estimate current cases.”).

---

## 3. All Countries page vs region pages – mismatches

- **All Countries page:** 88 countries (only countries that **have a plot**).
- **Region pages (from data):** 95 unique countries in the source data (by `Region` + `Country`).

So **7 countries** appear in the data (and thus in the region-level list derived from `data`) but **do not appear on the All Countries page** because they have **no plot** (plot is `NULL`). On region pages they are also dropped when building the grid (`region_plots <- region_plots[!sapply(region_plots, is.null)]`), so they do **not** get a country card on the region page either.

| Country | Region(s) in data | Why not on All Countries |
|---------|--------------------|---------------------------|
| Brunei Darussalam | East & Southeast Asia | No plot (excluded from All Countries) |
| Dominica | Caribbean | No plot (excluded from All Countries) |
| Oman | Europe, Middle East & North Africa | No plot (excluded from All Countries) |
| Philippines | East & Southeast Asia | No plot (excluded from All Countries) |
| Saudi Arabia | Europe, Middle East & North Africa | No plot (excluded from All Countries) |
| Taiwan | East & Southeast Asia | No plot (excluded from All Countries) |
| Yemen | Europe, Middle East & North Africa | No plot (excluded from All Countries) |

**Per-region summary:**

| Region | On region page (from data) | On All Countries (this region) | Only on region list (no plot) |
|--------|----------------------------|----------------------------------|--------------------------------|
| North & Central America | 9 | 9 | — |
| Caribbean | 25 | 24 | Dominica |
| South America | 13 | 13 | — |
| Europe, Middle East & North Africa | 4 | 1 | Oman, Saudi Arabia, Yemen (Sudan is the 1 on index) |
| Sub-Saharan Africa | 8 | 8 | — |
| South Asia | 8 | 8 | — |
| East & Southeast Asia | 13 | 10 | Brunei Darussalam, Philippines, Taiwan |
| Pacific Islands | 15 | 15 | — |

So:

- **On All Countries but not in any region:** none.
- **In at least one region’s data but not on All Countries:** 7 (all due to missing plot).

---

## 4. Current errors / issues observed

1. **Duplicate months in radial plot:** When building the radial plot, one country (with an empty or problematic `country` name in the data) triggered “WARNING: Duplicate Month values in df_region!” and “Plotting:” with no country name. This suggests at least one row in the nowcast or upstream data has a blank/missing `country` (or duplicated months after the baseline merge). Worth checking the pipeline for rows with empty `country` and for duplicate Month per country in the plot input.
2. **Seven countries with no plot:** Brunei Darussalam, Dominica, Oman, Philippines, Saudi Arabia, Taiwan, Yemen have no plot and therefore no card on All Countries and no card on their region page. Investigating why `make_radial_plot()` returns `NULL` for these (e.g. data shape, baseline, or option 1/2/3 logic in `Radial.R`) will fix the mismatch.

---

## 5. How to reproduce

From project root:

```bash
Rscript Scripts/audit_countries.R
```

Outputs:

- Console: Table 1 (country, region, type of missingness, accompanying text), Table 2 (All Countries vs region pages), and per-region counts.
- File: `Output/2026_03_14/audit_country_missingness.csv` (full Table 1).
