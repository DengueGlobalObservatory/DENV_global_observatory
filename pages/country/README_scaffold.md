# Country Page Scaffold

Country pages are generated from `country-config.csv` and the shared template `_country-template.qmd`.

## Workflow

1. Update `pages/country/country-config.csv`.
2. Run `Rscript Scripts/country/generate_country_pages.R`.
3. Render only national pages with `quarto render pages/country/*.qmd` or full site with `quarto render`.

## Config Schema

- `iso3`: ISO3 country code used to filter country data.
- `country_name`: Display name and lookup key for `all_country_plots`.
- `region`: Region label shown on page and used in narrative.
- `region_href`: Relative link to regional page.
- `map_src`: Relative map asset path.
- `slug`: Output filename stem (`pages/country/<slug>.qmd`).
- `enabled`: Whether a country page should be generated.

## Validation Rules

- `iso3`, `country_name`, `region`, and `slug` are required.
- `slug` values must be unique.
- `map_src` is expected for launch; if missing at render time, the template displays a non-blocking "Map coming soon" placeholder.
