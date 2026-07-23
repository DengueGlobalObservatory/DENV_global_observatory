---

editor: 
  markdown: 
    wrap: 72
---

# Appendix N: Summary Text Generation Scheme

## Overview

The Global Dengue Observatory (GDO) generates descriptive summary text at global, regional, and national levels. All narrative text is produced programmatically based on **severity ratios** — the ratio of observed (or nowcast-estimated) dengue cases to a historical seasonal baseline. This appendix documents the complete text classification scheme, threshold definitions, and sentence templates used across the observatory.

------------------------------------------------------------------------

## 1. Severity Ratio Definition

The severity ratio quantifies how current dengue activity compares to the historical average. Two ratio types are calculated:

### 1.1 Monthly Ratio

$$R_{\text{monthly}} = \frac{C_m}{E_m}$$

Where: - $C_m$ = reported (or nowcast-estimated) cases in the most recent month with available data - $E_m$ = average seasonal monthly cases for that calendar month (`Ave_season_monthly_cases`)

### 1.2 Year-to-Date (YTD) Cumulative Ratio

$$R_{\text{YTD}} = \frac{\sum_{j=1}^{m} C_j}{\sum_{j=1}^{m} E_j}$$

Where: - $C_j$ = cases in month $j$ of the current year - $E_j$ = expected average cases for month $j$ - $m$ = most recent month with available data

### 1.3 Baseline Derivation

The historical expected value ($E_m$) is the arithmetic mean of monthly cases across all qualifying historical seasons for a given country and calendar month. A season qualifies for inclusion in the baseline if:

1.  It contains 12 complete months of data
2.  It averages ≥3 cases per month
3.  The country has at least 3 qualifying seasons

Seasons are aligned to each country's dengue season (starting from the mean low-activity month, identified via circular statistics) rather than the calendar year.

------------------------------------------------------------------------

## 2. Classification Systems

The GDO employs context-specific classification systems that map severity ratios to descriptive phrases. These are applied at different spatial scales and page contexts as detailed below.

### 2.1 Five-Level Classification (Regional Summaries)

Used for monthly and year-to-date regional summary sentences.

| Severity Ratio | Descriptor | Example Output |
|:--------------------------:|:---------------------|:---------------------|
| ≥ 1.30 | running well above | "…running well above the seasonal baseline." |
| ≥ 1.10 and \< 1.30 | running slightly above | "…running slightly above the seasonal baseline." |
| \> 0.90 and \< 1.10 | tracking near | "…tracking near the seasonal baseline." |
| ≥ 0.70 and ≤ 0.90 | running slightly below | "…running slightly below the seasonal baseline." |
| \< 0.70 | running well below | "…running well below the seasonal baseline." |
| NA | tracking close to | "…tracking close to the seasonal baseline." |

**Implementation:** `ratio_phrase()` function in `Scripts/V1_Dashboard_setup.R`.

### 2.2 Five-Level Classification (Country Monthly Text)

Used for country-level analysis of the most recent month's cases relative to expectations.

| Severity Ratio | Descriptor | Example Output |
|:--------------------------:|:---------------------|:---------------------|
| ≥ 1.30 | well above | "…well above what we would typically expect…" |
| ≥ 1.10 and \< 1.30 | slightly above | "…slightly above what we would typically expect…" |
| ≥ 0.90 and \< 1.10 | close to | "…close to what we would typically expect…" |
| ≥ 0.70 and \< 0.90 | slightly below | "…slightly below what we would typically expect…" |
| \< 0.70 | well below | "…well below what we would typically expect…" |

**Implementation:** `monthly_phrase()` function in `pages/country/_country-template.qmd`.

### 2.3 Five-Level Classification (Country Hero Summary)

Used for the prominent status sentence on each country page, comparing year-to-date cumulative cases.

| Severity Ratio | Descriptor | Example Output |
|:--------------------------:|:---------------------|:---------------------|
| ≥ 1.40 | well above | "…reported cases are well above the expected number…" |
| ≥ 1.10 and \< 1.40 | above | "…reported cases are above the expected number…" |
| ≥ 0.90 and \< 1.10 | near | "…reported cases are near the expected number…" |
| ≥ 0.70 and \< 0.90 | below | "…reported cases are below the expected number…" |
| \< 0.70 | well below | "…reported cases are well below the expected number…" |

**Implementation:** `relative_level_phrase()` function in `pages/country/_country-template.qmd`.

### 2.4 Three-Level Classification (Season Status)

Used for season badges and country-level year-to-date season assessment.

|   Severity Ratio   | Descriptor | Badge Label            |
|:------------------:|:-----------|:-----------------------|
|       ≥ 1.20       | above      | "Season running high"  |
| ≥ 0.85 and \< 1.20 | near       | "Season near baseline" |
|      \< 0.85       | below      | "Season running low"   |

**Implementation:** `season_phrase()` and `season_badge_label_text()` functions.

### 2.5 Three-Level Classification (Global and Country Cards)

Used for the global scrolly narrative and country summary cards on the homepage and country index.

|  Severity Ratio   | Descriptor | Example Output                            |
|:-----------------:|:-----------|:------------------------------------------|
|      \> 1.10      | above      | "…the dengue situation is above average." |
| ≥ 0.90 and ≤ 1.10 | near       | "…the dengue situation is near average."  |
|      \< 0.90      | below      | "…the dengue situation is below average." |

**Implementation:** `classify_modifier()` in `build_global_story_points()` and `severity_country_blurb()` in `Scripts/figures/FUN_utility.R`.

------------------------------------------------------------------------

## 3. Text Generation Flowchart

The following diagram illustrates the complete text generation process from data inputs to rendered narrative text.

``` mermaid
flowchart TD
    %% Data inputs
    subgraph INPUTS ["Data Inputs"]
        NC["Nowcast output CSV<br/>(monthly cases by country)"]
        BL["Seasonal baseline CSV<br/>(Ave_season_monthly_cases)"]
    end

    %% Ratio calculation
    subgraph RATIOS ["Ratio Calculation"]
        MR["Monthly Ratio<br/>R = cases / expected"]
        YR["YTD Ratio<br/>R = Σcases / Σexpected"]
    end

    NC --> MR
    NC --> YR
    BL --> MR
    BL --> YR

    %% Aggregation
    subgraph AGGREGATE ["Spatial Aggregation"]
        CTR["Country-level ratios"]
        REG["Regional-level ratios<br/>(sum across countries)"]
        GLB["Global-level ratios<br/>(sum across all countries)"]
    end

    MR --> CTR
    YR --> CTR
    CTR --> REG
    CTR --> GLB

    %% Classification
    subgraph CLASSIFY ["Classification Functions"]
        RP["ratio_phrase()<br/>5-level: well above → well below"]
        MP["monthly_phrase()<br/>5-level: well above → well below"]
        RLP["relative_level_phrase()<br/>5-level: well above → well below"]
        SP["season_phrase()<br/>3-level: above / near / below"]
        CM["classify_modifier()<br/>3-level: above / near / below"]
        SB["season_badge_label_text()<br/>3-level: high / near / low"]
    end

    REG --> RP
    CTR --> MP
    CTR --> RLP
    CTR --> SP
    GLB --> CM
    REG --> SB
    CTR --> SB

    %% Output text
    subgraph OUTPUT ["Generated Text"]
        RS["Regional summary sentences"]
        CS["Country analysis sentences"]
        CH["Country hero status"]
        GS["Global scrolly narrative"]
        CB["Country cards / blurbs"]
        BD["Season badges"]
    end

    RP --> RS
    MP --> CS
    RLP --> CH
    CM --> GS
    CM --> CB
    SP --> CS
    SB --> BD
```

------------------------------------------------------------------------

## 4. Classification Threshold Flowchart

The following flowchart shows the decision logic for the primary 5-level classification (`ratio_phrase`) used in regional summaries:

``` mermaid
flowchart TD
    START["Input: Severity Ratio (R)"] --> NA_CHECK{"Is R = NA?"}
    NA_CHECK -->|Yes| NA_OUT["'tracking close to<br/>the seasonal baseline'"]
    NA_CHECK -->|No| HIGH{"R ≥ 1.30?"}
    HIGH -->|Yes| WELL_ABOVE["'running well above<br/>the seasonal baseline'"]
    HIGH -->|No| SLIGHT_HIGH{"R ≥ 1.10?"}
    SLIGHT_HIGH -->|Yes| SLIGHTLY_ABOVE["'running slightly above<br/>the seasonal baseline'"]
    SLIGHT_HIGH -->|No| LOW{"R ≤ 0.70?"}
    LOW -->|Yes| WELL_BELOW["'running well below<br/>the seasonal baseline'"]
    LOW -->|No| SLIGHT_LOW{"R ≤ 0.90?"}
    SLIGHT_LOW -->|Yes| SLIGHTLY_BELOW["'running slightly below<br/>the seasonal baseline'"]
    SLIGHT_LOW -->|No| NEAR["'tracking near<br/>the seasonal baseline'"]

    style WELL_ABOVE fill:#D32F2F,color:#fff
    style SLIGHTLY_ABOVE fill:#FF8F00,color:#fff
    style NEAR fill:#388E3C,color:#fff
    style SLIGHTLY_BELOW fill:#1976D2,color:#fff
    style WELL_BELOW fill:#0D47A1,color:#fff
    style NA_OUT fill:#9E9E9E,color:#fff
```

------------------------------------------------------------------------

## 5. Sentence Templates

### 5.1 Global Summary (Homepage Scrolly Narrative)

The global summary is assembled from five sequential components:

| \# | Component | Template |
|:--------------------------:|:---------------------|:---------------------|
| 1 | Monthly cases | "At this time globally we estimate **{N}** cases in {Month}." |
| 2 | Monthly ratio + trajectory | "This represents **{R}×** the expected number of cases for this time of the year. This is an **{increase/decrease/similar level}** over the number of cases reported in {Previous Month}." |
| 3 | YTD cases | "Globally we estimate **{N}** cases have been reported in {Year} as of {Month}." |
| 4 | YTD ratio | "This represents **{R}×** the expected number of cases by this time in the year." |
| 5 | Season status | "The {Year} dengue season is **{below/near/above}** average globally so far." |

**Trajectory classification** (month-over-month percent change): - Change \> +5% → "increase" - Change \< −5% → "decrease" - Otherwise → "similar level"

### 5.2 Regional Summary

Each regional page displays two sentences and a season badge:

| Component | Template |
|:-----------------------------------|:-----------------------------------|
| Latest month sentence | "{Region} logged **{N}** cases in {Month Year}, {5-level phrase}." |
| YTD sentence | "Year-to-date totals sit at **{N}**, {5-level phrase referring to 'the expected burden'}." |
| Season badge | "{Season running high / Season near baseline / Season running low}" |

### 5.3 Country Summary (National Pages)

Each country page includes a hero status sentence and further analysis text:

| Component | Ratio Used | Template |
|:-----------------------|:-----------------------|:-----------------------|
| Hero status | YTD (`relative_level_phrase`) | "In {Year} so far, the reported cases are **{well above/above/near/below/well below}** the expected number of cases. Cases are currently {increasing/decreasing/stable}, {trajectory context}." |
| Analysis sentence 1 | Monthly (`monthly_phrase`) | "In {Month Year}, {Country} reported **{N}** dengue cases, **{well above/slightly above/close to/slightly below/well below}** what we would typically expect for this time of year." |
| Analysis sentence 2 | N/A | "Compared with {Previous Month}, in which {N} cases were reported, this is a similar level of reported cases." |
| Analysis sentence 3 | YTD (`season_phrase`) | "So far in {Year}, the country has recorded **{N}** cases — about **{R}×** the expected level — so the season is **{above/near/below}** average to date." |

### 5.4 Country Cards (Index Pages)

Used on the homepage high-severity panel and the all-countries index:

| Template |
|:-----------------------------------------------------------------------|
| "{Country} is estimated to have experienced **{N}** cases this year to date. This is **{R}×** the number of cases reported in an average year. This year the dengue situation in {Country} is **{above/near/below}** average." |

------------------------------------------------------------------------

## 6. Consolidated Threshold Lookup Table

The table below provides a unified view of all classification thresholds across the observatory, organized by the ratio value:

| Ratio Range | `ratio_phrase()` | `monthly_phrase()` | `relative_level_phrase()` | `season_phrase()` | `classify_modifier()` / cards |
|:--------:|:----------:|:------------:|:-----------:|:-----------:|:----------------:|
| ≥ 1.40 | running well above | well above | **well above** | above | above |
| 1.30–1.39 | running well above | well above | above | above | above |
| 1.20–1.29 | running slightly above | slightly above | above | **above** | above |
| 1.10–1.19 | running slightly above | slightly above | above | near | above |
| 1.01–1.09 | tracking near | close to | near | near | near |
| 0.91–1.00 | tracking near | close to | near | near | near |
| 0.90 | tracking near | close to | near | near | near |
| 0.86–0.89 | running slightly below | slightly below | below | near | below |
| 0.85 | running slightly below | slightly below | below | **below** | below |
| 0.71–0.84 | running slightly below | slightly below | below | below | below |
| 0.70 | running slightly below | slightly below | below | below | below |
| \< 0.70 | running well below | well below | **well below** | below | below |

*Bold entries mark the exact boundary where the classification changes for that function.*

------------------------------------------------------------------------

## 7. Month-Over-Month Trajectory Classification

In addition to severity ratios, the GDO classifies the trajectory of case counts between consecutive months:

$$\Delta = \frac{C_m - C_{m-1}}{C_{m-1}}$$

| Percent Change | Classification |
|:--------------:|:---------------|
|     \> +5%     | Increasing     |
|   −5% to +5%   | Stable         |
|     \< −5%     | Decreasing     |

This trajectory classification is used in the global scrolly narrative and country hero sentences to provide context on the direction of change.

------------------------------------------------------------------------

## 8. Data Flow Summary

``` mermaid
flowchart LR
    subgraph Sources ["External Data Sources"]
        WHO["WHO"]
        PAHO["PAHO"]
        SEARO["SEARO"]
        OD["OpenDengue"]
    end

    subgraph Pipeline ["V1 Pipeline"]
        direction TB
        INGEST["Data sourcing<br/>& ingestion"]
        BACKFILL["Backfilling &<br/>source selection"]
        BASELINE["Seasonal baseline<br/>identification"]
        NOWCAST["Proportion-based<br/>nowcasting"]
    end

    subgraph Dashboard ["Dashboard Rendering"]
        direction TB
        SETUP["V1_Dashboard_setup.R<br/>• Compute ratios<br/>• Apply classification functions<br/>• Build summary dataframes"]
        RENDER["Quarto render<br/>• Global/regional/country pages<br/>• Insert classified text"]
    end

    Sources --> INGEST
    INGEST --> BACKFILL --> BASELINE --> NOWCAST
    NOWCAST --> SETUP
    BASELINE --> SETUP
    SETUP --> RENDER
```

------------------------------------------------------------------------

## 9. Implementation Reference

All text generation logic is contained in the following source files:

| File | Functions | Scope |
|:-----------------------|:-----------------------|:-----------------------|
| `Scripts/V1_Dashboard_setup.R` | `ratio_phrase()`, `season_badge_label_text()`, `season_badge_state_class()`, region_callouts block | Regional summaries, badges |
| `Scripts/figures/FUN_utility.R` | `build_global_story_points()`, `severity_country_blurb()`, `render_region_overview()` | Global narrative, country cards |
| `pages/country/_country-template.qmd` | `monthly_phrase()`, `season_phrase()`, `relative_level_phrase()` | Country-level analysis text |

------------------------------------------------------------------------

## 10. Notes

1.  **No external lookup tables are used.** All ratio-to-text classification is performed inline via conditional logic (`case_when` / `if` statements) within R functions.

2.  **NA handling.** When a severity ratio cannot be computed (e.g., due to missing baseline data), the system defaults to neutral phrasing ("tracking close to" or "near") rather than omitting text.

3.  **Ratios are uncapped for text classification.** Although radial plots cap ratios to the range \[0.5, 2.0\] for visual display, text classifications use the full uncapped ratio value.

4.  **Baseline phrase is configurable.** The `ratio_phrase()` function accepts a `baseline_phrase` parameter, allowing contextual variation (e.g., "the seasonal baseline" for monthly comparisons vs. "the expected burden" for YTD comparisons).
