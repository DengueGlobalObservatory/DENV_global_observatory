# National Pages Context

This file is the working context for future edits to national pages and the shared country template.

## Core Architecture

- `pages/country/_country-template.qmd`
  - Shared page structure and logic for all country pages.
  - Contains hero block, narrative text, and the yearly time-series chart logic.
  - Uses Quarto `params` for country-specific values.

- `pages/country/country-config.csv`
  - Source of truth for which countries get national pages.
  - Columns: `iso3`, `country_name`, `region`, `region_href`, `map_src`, `slug`, `enabled`.
  - Keep this file in sync with current observatory inclusion rules.

- `Scripts/country/generate_country_pages.R`
  - Generates wrapper pages (`pages/country/<slug>.qmd`) from `country-config.csv`.
  - Each wrapper sets params and includes `_country-template.qmd`.

- `pages/country/*.qmd` (generated wrappers)
  - Generated files, not hand-authored content.
  - Safe to regenerate at any time from config + script.

- `_quarto.yml`
  - Uses `pages/country/*.qmd` in `project.render`.
  - Important: wildcard also matches `_country-template.qmd` if rendered directly by glob in shell.

## Current Scope Rule

- National pages are currently configured to include countries from the All Countries list that have enough current-year data.
- Countries with "Current year data ... not been reported yet" are excluded from `country-config.csv`.

## Recently Added National Pages

- Ethiopia (`ethiopia`, `ETH`)
- Tanzania (`tanzania`, `TZA`)

## Editing Rules

- Change shared behavior/layout in `_country-template.qmd`.
- Change which countries are published in `country-config.csv`.
- After updating config, regenerate wrappers via:
  - `Rscript Scripts/country/generate_country_pages.R`
- Avoid manual edits to generated wrapper files unless debugging.

## Render Workflow

- Full country refresh:
  1. Update config/template.
  2. Run `Rscript Scripts/country/generate_country_pages.R`.
  3. Render country pages.

- Safe render command pattern:
  - Render per file, or run a loop excluding `_country-template.qmd`.
  - Direct wildcard render (`quarto render pages/country/*.qmd`) can attempt to render `_country-template.qmd` and fail because it has no params.

## Link Behavior

- `pages/country-index.qmd`:
  - Radial plot is clickable and links to country pages via `country-config.csv` slugs.
  - Intro text explicitly tells users plots are clickable.

- Region pages:
  - `Scripts/figures/FUN_utility.R` (`render_country_plot_text_grid`) now:
    - Adds guidance text about clickable radial plots.
    - Wraps radial plots with links to national pages using `country-config.csv`.
  - Working-directory pitfall: region pages call the function from a later chunk. knitr restores cwd back to `pages/` between chunks even after `setwd("..")` in the setup chunk, so the function probes a few candidate paths to locate `country-config.csv` (project-root, `pages/`-relative, and `../pages/`-relative). If those fail, the lookup table is empty and plots render without links while the note still shows.

## Styling Notes

- Link hover/magnify effect for plot links is defined in `style.css` under:
  - `.country-card-plot-link`
- Country page large-screen centering uses:
  - `.country-page-shell` in `style.css`
  - Wrapper `<div class="country-page-shell">` in `_country-template.qmd`

## Common Pitfalls

- Rendering `_country-template.qmd` directly causes `params` errors.
- If country links stop resolving, verify `slug` values in `country-config.csv`.
- If region or country cards lose layout, check whether link wrappers are using `country-card-plot-link` class.
- If map does not exist for a country, template shows a non-blocking "Map coming soon" placeholder.

## Quick Checklist Before Commit

- [ ] `country-config.csv` sorted and valid.
- [ ] Wrappers regenerated after config changes.
- [ ] At least one sample country page rendered successfully.
- [ ] Country index rendered successfully.
- [ ] Region page sample rendered successfully (if region card logic changed).
