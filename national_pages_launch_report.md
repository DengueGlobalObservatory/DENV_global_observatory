# National Pages Launch Report

Date: 2026-04-17  
Project: Global Dengue Observatory (`DENV_global_observatory`)

## Objective

Launch scalable national pages using a reusable template and metadata-driven generation flow, while preserving current dashboard behavior and keeping updates maintainable for biweekly refreshes.

## What Was Implemented

### 1) Reusable scaffold architecture

- Added shared template: `pages/country/_country-template.qmd`
- Added metadata registry: `pages/country/country-config.csv`
- Added generator script: `Scripts/country/generate_country_pages.R`
- Added usage/context docs:
  - `pages/country/README_scaffold.md`
  - `pages/country/CONTEXT_national_pages.md`

Each generated country page is a small wrapper (`pages/country/<slug>.qmd`) that passes params and includes `_country-template.qmd`.

### 2) Render pipeline updates

- `_quarto.yml` now uses wildcard render for national pages:
  - `pages/country/*.qmd`
- This avoids manual edits for every new country page wrapper.

### 3) Country and region navigation behavior

- All Countries page (`pages/country-index.qmd`):
  - Radial plots now link to national pages.
  - Added guidance text telling users plots are clickable.
  - Restored original country title header styling.
  - Added hover/focus magnify effect via `.country-card-plot-link` in `style.css`.

- Region pages (shared renderer in `Scripts/figures/FUN_utility.R`):
  - Radial plots now link to national pages.
  - Added similar guidance text above region country cards.

### 4) Large-screen country layout

- Centered national pages on large screens by wrapping template output in `.country-page-shell`.
- Updated `style.css` to keep content centered while preserving mobile behavior.

### 5) Uncertainty visualization improvement

- Updated country time-series uncertainty in `_country-template.qmd`:
  - Replaced thick bar-style uncertainty marks with whisker-style error lines.
  - Whiskers only appear for estimated months where CI exists.
  - Verified on Afghanistan page render.

## Inclusion Logic Used for Launch

Initial scale-up used countries marked `Included` in:

- `Output/2026_02_17/country_tracking.csv`

Then `country-config.csv` was filtered to countries shown on All Countries page **with enough current-year data**, excluding countries with:

- “Current year data for X has not been reported yet...”

Excluded (7): Bahamas, Bonaire/Saint Eustatius/Saba, Curacao, Haiti, Indonesia, Nicaragua, Venezuela.

Current `country-config.csv` rows after filtering: **81** countries.

## Operational Workflow (Current)

1. Run data pipeline:
   - `Rscript Scripts/V1_Pipeline.R`
2. Regenerate wrappers from metadata:
   - `Rscript Scripts/country/generate_country_pages.R`
3. Render pages:
   - Country pages (exclude template partial in manual shell loops)
   - Region pages / country-index / full site as needed

## Validation Performed

- Render checks run for:
  - sample country pages (e.g., Brazil, Cuba, Guyana, Afghanistan)
  - All Countries page
  - sample region page (South America)
- No blocking linter issues introduced by the core scaffold edits.

## Known Pitfalls / Notes

- `quarto render pages/country/*.qmd` may try to render `_country-template.qmd` (no params) and fail.
  - Use explicit file loops excluding `_country-template.qmd` for bulk manual renders.
- Wrapper pages are generated artifacts; avoid manual content edits in those files.
- `country-config.csv` is the source of truth for published national pages.
- `renv` warning appears in this environment but did not block successful renders.

## Summary

National pages are now launch-ready with a reusable scaffold, link-integrated navigation from country/region cards, improved uncertainty display, and a metadata-driven generation workflow suitable for ongoing observatory updates.
