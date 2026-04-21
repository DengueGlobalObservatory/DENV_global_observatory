# Audit: countries, missing data, and All Countries vs region page consistency
# Run from project root: Rscript Scripts/audit_countries.R

setwd("/Users/lshks26/Dropbox/DMMG/DENV_dashboard/DENV_global_observatory")
if (basename(getwd()) == "pages") setwd("..")

# Load setup (same as country-index and region pages)
suppressMessages(suppressWarnings({
  source("Scripts/V1_Dashboard_setup.R")
}))

# ---- 1) Countries that appear on All Countries page ----
# all_countries_data is built in country-index.qmd from country_summary_df, filtered to has_plot
country_name_col <- if ("country" %in% names(country_summary_df)) "country" else "Country"
recent_month_val <- recent_month
current_year_val <- current_year

all_countries_from_index <- country_summary_df %>%
  filter(
    !is.na(.data[[country_name_col]]) & .data[[country_name_col]] != "",
    !is.na(Month),
    Month <= recent_month_val
  ) %>%
  group_by(.data[[country_name_col]]) %>%
  arrange(Month, .by_group = TRUE) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(has_plot = .data[[country_name_col]] %in% names(all_country_plots)) %>%
  filter(has_plot)

# So: "All Countries page" = unique(all_countries_from_index[[country_name_col]])
all_country_names <- sort(unique(all_countries_from_index[[country_name_col]]))

# ---- 2) Per-country: region, type of missingness, accompanying text ----
# Use severity_country_blurb logic / actual blurb from summary
out <- all_countries_from_index %>%
  mutate(
    country_label = .data[[country_name_col]],
    region_label = coalesce(Region, ""),
    # Type of missingness
    has_data = has_current_year_cases,
    ratio_valid = !is.na(cum_ratio) & is.finite(cum_ratio),
    cases_valid = !is.na(cum_high) & is.finite(cum_high),
    type_of_missingness = case_when(
      !has_data ~ "No current year data reported",
      !ratio_valid ~ "Data still loading (totals being compiled)",
      !cases_valid ~ "Cases being compiled (ratio only)",
      TRUE ~ "Full data"
    ),
    accompanying_text = severity_country_blurb(
      country_label, cum_ratio, cum_high,
      region = region_label, region_href = NA_character_, has_data = has_data
    )
  ) %>%
  select(country = country_label, region = region_label, type_of_missingness, accompanying_text)

# Add data status footnote (same as on page)
if (exists("country_data_status") && nrow(country_data_status) > 0) {
  out <- out %>%
    left_join(
      country_data_status %>% select(country_name, data_status_message),
      by = c("country" = "country_name")
    ) %>%
    mutate(data_status_message = coalesce(data_status_message, "Recent months contain observed data only"))
} else {
  out$data_status_message <- "Recent months contain observed data only"
}

# Print table 1: country, region, type of missingness, accompanying text
cat("\n===== TABLE 1: Country, Region, Type of missingness, Accompanying text =====\n\n")
print(out, n = Inf)
cat("\n")

# Countries with missing / partial data only
missing_only <- out %>% filter(type_of_missingness != "Full data")
if (nrow(missing_only) > 0) {
  cat("Countries with non-full data (missing or partial):\n")
  print(missing_only %>% select(country, region, type_of_missingness), n = Inf)
  cat("\n")
}

# ---- 3) Countries in data but WITHOUT a plot (would not appear on All Countries) ----
in_data <- data %>% filter(Year == current_year_val, Month <= recent_month_val) %>%
  distinct(.data[[country_name_col]], Region) %>%
  filter(!is.na(.data[[country_name_col]]) & .data[[country_name_col]] != "")
colnames(in_data)[1] <- "country"
in_data$has_plot <- in_data$country %in% names(all_country_plots)
no_plot <- in_data %>% filter(!has_plot)
if (nrow(no_plot) > 0) {
  cat("Countries in data but with NO plot (excluded from All Countries page):\n")
  print(no_plot, n = Inf)
  cat("\n")
}

# ---- 4) Region pages: countries listed per region ----
# Region pages use: data %>% filter(Region == region_for_page) %>% distinct(Country) %>% pull(Country)
# So they use column "Country" (capital C). data has both country and Country (same values).
regions_in_site <- c(
  "North & Central America",
  "Caribbean",
  "South America",
  "Europe, Middle East & North Africa",
  "Sub-Saharan Africa",
  "South Asia",
  "East & Southeast Asia",
  "Pacific Islands"
)

region_to_countries <- function(region_name) {
  data %>%
    filter(Region == region_name) %>%
    distinct(Country) %>%
    pull(Country) %>%
    sort()
}

countries_by_region <- lapply(setNames(regions_in_site, regions_in_site), region_to_countries)

# All countries that appear on at least one region page
all_countries_on_region_pages <- unique(unlist(countries_by_region))

# ---- 5) Compare All Countries page vs Region pages ----
cat("===== TABLE 2: All Countries page vs Region pages =====\n\n")
cat("Countries on All Countries page:", length(all_country_names), "\n")
cat("Countries on region pages (unique):", length(all_countries_on_region_pages), "\n\n")

on_index_not_in_any_region <- setdiff(all_country_names, all_countries_on_region_pages)
on_region_not_on_index <- setdiff(all_countries_on_region_pages, all_country_names)

cat("On All Countries page but NOT in any region page:\n")
if (length(on_index_not_in_any_region) == 0) {
  cat("  (none)\n")
} else {
  for (c in sort(on_index_not_in_any_region)) cat(" -", c, "\n")
}

cat("\nOn a region page but NOT on All Countries page:\n")
if (length(on_region_not_on_index) == 0) {
  cat("  (none)\n")
} else {
  for (c in sort(on_region_not_on_index)) {
    regs <- names(regions_in_site)[sapply(countries_by_region, function(x) c %in% x)]
    cat(" -", c, "  [regions:", paste(regs, collapse = ", "), "]\n")
  }
}

# Per-region: list countries on region page vs on index (with that region)
cat("\nPer-region: countries on region page vs on All Countries (same region):\n")
for (reg in regions_in_site) {
  on_region <- countries_by_region[[reg]]
  on_index_this_region <- intersect(all_country_names, out %>% filter(region == reg) %>% pull(country))
  only_region <- setdiff(on_region, on_index_this_region)
  only_index <- setdiff(on_index_this_region, on_region)
  cat("\n", reg, ":\n", sep = "")
  cat("  On region page:", length(on_region), "| On index (this region):", length(on_index_this_region), "\n")
  if (length(only_region) > 0) cat("  Only on region page:", paste(only_region, collapse = ", "), "\n")
  if (length(only_index) > 0) cat("  Only on index:", paste(only_index, collapse = ", "), "\n")
}

# Write CSV of Table 1 for reference
out_file <- "Output/2026_03_14/audit_country_missingness.csv"
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
write.csv(out, out_file, row.names = FALSE)
cat("\nTable 1 written to:", out_file, "\n")
