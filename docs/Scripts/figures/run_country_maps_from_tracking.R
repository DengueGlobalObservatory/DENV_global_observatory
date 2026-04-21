#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
})

# Build and save country context maps for all countries marked as Included
# in the specified country tracking file.

tracking_csv <- file.path("Output", "2026_02_17", "country_tracking.csv")
out_dir <- file.path("Assets", "Stable", "country_maps")

source("Scripts/figures/FUN_country_map.R")
source("Assets/Stable/OD_maps/fn_OD_region.R")

if (!file.exists(tracking_csv)) {
  stop("Tracking file not found: ", tracking_csv)
}

tracking <- read.csv(tracking_csv, check.names = FALSE, stringsAsFactors = FALSE)
required_cols <- c("iso3", "country", "final_status")
missing_cols <- setdiff(required_cols, names(tracking))
if (length(missing_cols) > 0) {
  stop("Tracking file missing required columns: ", paste(missing_cols, collapse = ", "))
}

# Object requested: ISO3 + OD region for all included countries
included_countries <- tracking %>%
  dplyr::filter(.data$final_status == "Included") %>%
  dplyr::transmute(
    iso3 = toupper(trimws(.data$iso3)),
    country = trimws(.data$country)
  ) %>%
  dplyr::filter(.data$iso3 != "") %>%
  dplyr::distinct(.data$iso3, .keep_all = TRUE) %>%
  dplyr::left_join(
    get_od_regions(iso3_vector = unique(.$iso3)) %>%
      dplyr::transmute(
        iso3 = .data$ISO_A0,
        region = .data$od_region
      ),
    by = "iso3"
  ) %>%
  dplyr::arrange(.data$country)

if (nrow(included_countries) == 0) {
  stop("No countries with final_status == 'Included' were found.")
}

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

results <- lapply(seq_len(nrow(included_countries)), function(i) {
  iso3_i <- included_countries$iso3[i]
  country_i <- included_countries$country[i]
  region_i <- included_countries$region[i]
  out_file_i <- file.path(out_dir, paste0(tolower(iso3_i), ".png"))

  if (is.na(region_i) || region_i == "Other" || region_i == "") {
    return(data.frame(
      iso3 = iso3_i,
      country = country_i,
      region = region_i,
      out_file = out_file_i,
      status = "error",
      error = "Missing or unsupported OD region",
      stringsAsFactors = FALSE
    ))
  }

  tryCatch({
    # save_country_context_map() internally calls make_country_context_map().
    save_country_context_map(
      iso3 = iso3_i,
      region = region_i,
      out_file = out_file_i,
      width = 7,
      height = 7,
      dpi = 220
    )

    data.frame(
      iso3 = iso3_i,
      country = country_i,
      region = region_i,
      out_file = out_file_i,
      status = "ok",
      error = NA_character_,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      iso3 = iso3_i,
      country = country_i,
      region = region_i,
      out_file = out_file_i,
      status = "error",
      error = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
})

results_df <- dplyr::bind_rows(results)

results_path <- file.path("Output", "2026_02_17", "country_map_build_results.csv")
write.csv(results_df, results_path, row.names = FALSE)

message("Included countries: ", nrow(included_countries))
message("Maps built successfully: ", sum(results_df$status == "ok"))
message("Maps failed: ", sum(results_df$status == "error"))
message("Included country object available as `included_countries`.")
message("Build results saved to: ", results_path)
