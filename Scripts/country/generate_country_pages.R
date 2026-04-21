#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(glue)
})

config_path <- file.path("pages", "country", "country-config.csv")
output_dir <- file.path("pages", "country")

if (!file.exists(config_path)) {
  stop("Missing country config: ", config_path)
}

country_cfg <- readr::read_csv(config_path, show_col_types = FALSE) %>%
  dplyr::mutate(
    iso3 = toupper(trimws(iso3)),
    country_name = trimws(country_name),
    region = trimws(region),
    region_href = trimws(region_href),
    map_src = trimws(map_src),
    slug = stringr::str_to_lower(trimws(slug))
  ) %>%
  dplyr::filter(isTRUE(enabled) | tolower(as.character(enabled)) == "true")

required_cols <- c("iso3", "country_name", "region", "region_href", "map_src", "slug")
missing_cols <- setdiff(required_cols, names(country_cfg))
if (length(missing_cols) > 0) {
  stop("country-config.csv missing columns: ", paste(missing_cols, collapse = ", "))
}

if (any(country_cfg$iso3 == "" | is.na(country_cfg$iso3))) stop("Config has blank iso3 values.")
if (any(country_cfg$country_name == "" | is.na(country_cfg$country_name))) stop("Config has blank country_name values.")
if (any(country_cfg$region == "" | is.na(country_cfg$region))) stop("Config has blank region values.")
if (any(country_cfg$slug == "" | is.na(country_cfg$slug))) stop("Config has blank slug values.")
if (anyDuplicated(country_cfg$slug) > 0) stop("Config contains duplicated slugs.")

write_country_page <- function(row) {
  output_path <- file.path(output_dir, paste0(row$slug, ".qmd"))
  page_text <- glue::glue(
'---
title: ""
pagetitle: "{row$country_name}"
format: html
page-layout: full
title-block-style: none
self-contained: true
params:
  iso3: "{row$iso3}"
  country_name: "{row$country_name}"
  region: "{row$region}"
  region_href: "{row$region_href}"
  map_src: "{row$map_src}"
  slug: "{row$slug}"
---

{{{{< include _country-template.qmd >}}}}
'
  )
  writeLines(page_text, output_path, useBytes = TRUE)
  output_path
}

written <- apply(country_cfg, 1, function(x) {
  row <- as.list(x)
  write_country_page(row)
})

message("Country pages generated: ", length(written))
for (path in written) message(" - ", path)
