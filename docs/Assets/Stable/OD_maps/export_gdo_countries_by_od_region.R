#!/usr/bin/env Rscript

# Export the canonical list of GDO countries with their OD region and three
# inclusion-tier flags (pipeline-included = 97, has radial plot = 88,
# has complete-data national page = 81).
#
# Usage (from project root):
#   Rscript Assets/Stable/OD_maps/export_gdo_countries_by_od_region.R
#   Rscript Assets/Stable/OD_maps/export_gdo_countries_by_od_region.R Output/2026_05_31/country_tracking.csv

suppressPackageStartupMessages({
  library(dplyr)
})

# ---- Locate project root (file is at Assets/Stable/OD_maps/...) ------------
if (!file.exists("Assets/Stable/OD_maps/fn_OD_region.R")) {
  if (file.exists("fn_OD_region.R")) {
    setwd("../../..")
  }
}
if (!file.exists("Assets/Stable/OD_maps/fn_OD_region.R")) {
  stop("Run this script from the project root (cannot find Assets/Stable/OD_maps/fn_OD_region.R)")
}

source("Assets/Stable/OD_maps/fn_OD_region.R")

# ---- Resolve country_tracking.csv ------------------------------------------
resolve_tracking_file <- function(cli_arg = NULL) {
  if (!is.null(cli_arg) && nzchar(cli_arg)) {
    if (!file.exists(cli_arg)) {
      stop("Tracking file not found: ", cli_arg)
    }
    return(cli_arg)
  }
  candidates <- list.files(
    "Output", pattern = "^country_tracking\\.csv$",
    recursive = TRUE, full.names = TRUE
  )
  if (length(candidates) == 0) {
    stop("No Output/*/country_tracking.csv files found.")
  }
  folder_dates <- suppressWarnings(as.Date(gsub("_", "-", basename(dirname(candidates)))))
  candidates <- candidates[!is.na(folder_dates)]
  folder_dates <- folder_dates[!is.na(folder_dates)]
  if (length(candidates) == 0) {
    stop("No date-named Output/<YYYY_MM_DD>/country_tracking.csv folders found.")
  }
  candidates[which.max(folder_dates)]
}

cli_args <- commandArgs(trailingOnly = TRUE)
tracking_csv <- resolve_tracking_file(if (length(cli_args) >= 1) cli_args[1] else NULL)
message("Tracking file: ", tracking_csv)

# ---- Tier 1: pipeline-included (97) ----------------------------------------
tracking <- read.csv(tracking_csv, check.names = FALSE, stringsAsFactors = FALSE)
required_cols <- c("iso3", "country", "final_status")
missing_cols <- setdiff(required_cols, names(tracking))
if (length(missing_cols) > 0) {
  stop("Tracking file missing required columns: ", paste(missing_cols, collapse = ", "))
}

included <- tracking %>%
  dplyr::filter(.data$final_status == "Included") %>%
  dplyr::transmute(
    iso3 = toupper(trimws(.data$iso3)),
    country = trimws(.data$country)
  ) %>%
  dplyr::filter(.data$iso3 != "") %>%
  dplyr::distinct(.data$iso3, .keep_all = TRUE)

if (nrow(included) == 0) {
  stop("No countries with final_status == 'Included' were found.")
}

# ---- Tier 2: has radial plot (88) ------------------------------------------
# Sourcing the dashboard setup loads the latest nowcast output and builds
# `all_country_plots`, the exact object the All Countries page uses.
message("Loading dashboard setup to determine which countries have radial plots...")
suppressMessages(suppressWarnings(source("Scripts/V1_Dashboard_setup.R")))

if (!exists("all_country_plots") || !exists("data")) {
  stop("Dashboard setup did not produce expected objects (`data`, `all_country_plots`).")
}

country_col <- if ("country" %in% names(data)) "country" else "Country"
radial_iso3 <- data %>%
  dplyr::filter(.data[[country_col]] %in% names(all_country_plots)) %>%
  dplyr::distinct(.data$iso3) %>%
  dplyr::pull(.data$iso3) %>%
  toupper()

# ---- Tier 3: complete-data national page (81) ------------------------------
config_path <- "pages/country/country-config.csv"
if (!file.exists(config_path)) {
  stop("Country config not found: ", config_path)
}
country_cfg <- read.csv(config_path, check.names = FALSE, stringsAsFactors = FALSE)
complete_iso3 <- country_cfg %>%
  dplyr::filter(isTRUE(.data$enabled) | tolower(as.character(.data$enabled)) == "true") %>%
  dplyr::pull(.data$iso3) %>%
  toupper()

# ---- Region levels (match pipeline ordering) -------------------------------
region_levels <- c(
  "South America",
  "Caribbean",
  "Pacific Islands",
  "South Asia",
  "North & Central America",
  "Sub-Saharan Africa",
  "East & Southeast Asia",
  "Europe, Middle East & North Africa"
)

# ---- Build output table ----------------------------------------------------
regions <- get_od_regions(iso3_vector = included$iso3) %>%
  dplyr::transmute(
    iso3 = .data$ISO_A0,
    country_name = .data$country_name,
    od_region = .data$od_region
  )

out <- included %>%
  dplyr::left_join(regions, by = "iso3") %>%
  dplyr::mutate(
    pipeline_included = TRUE,
    has_radial_plot = iso3 %in% radial_iso3,
    has_complete_data = iso3 %in% complete_iso3,
    display_tier = dplyr::case_when(
      has_complete_data ~ "complete_data",
      has_radial_plot ~ "radial_plot_only",
      TRUE ~ "pipeline_only"
    ),
    od_region_f = factor(od_region, levels = region_levels)
  ) %>%
  dplyr::arrange(od_region_f, country_name) %>%
  dplyr::select(
    iso3, country_name, country, od_region,
    pipeline_included, has_radial_plot, has_complete_data, display_tier
  )

# ---- Checks ----------------------------------------------------------------
other_or_na <- out %>% dplyr::filter(is.na(od_region) | od_region == "Other")
if (nrow(other_or_na) > 0) {
  warning(
    "Countries with missing or 'Other' OD region: ",
    paste(other_or_na$iso3, collapse = ", ")
  )
}

# Sanity: included rows in tracking should match output rows
n_included_in_tracking <- sum(tracking$final_status == "Included", na.rm = TRUE)
if (nrow(out) != n_included_in_tracking) {
  warning(sprintf(
    "Output rows (%d) != Included rows in tracking (%d)",
    nrow(out), n_included_in_tracking
  ))
}

# ---- Write CSV -------------------------------------------------------------
out_path <- "Assets/Stable/OD_maps/gdo_included_countries_by_od_region.csv"
write.csv(out, out_path, row.names = FALSE)

message("Wrote: ", out_path)
message(sprintf(
  "Rows: %d  |  pipeline=%d  radial=%d  complete=%d",
  nrow(out),
  sum(out$pipeline_included),
  sum(out$has_radial_plot),
  sum(out$has_complete_data)
))
message("display_tier counts:")
print(table(out$display_tier, useNA = "ifany"))
message("od_region counts:")
print(table(out$od_region, useNA = "ifany"))
