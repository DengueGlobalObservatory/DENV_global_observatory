library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)

# Build a consistent world sf layer with ISO3 and OD region labels.
# Reuses the Natural Earth + map_units approach from Assets/Stable/OD_maps/OD_region_vis.R
build_world_sf <- function(exclude_names = c("Greenland", "St. Pierre and Miquelon", "Falkland Is.", "Antarctica")) {
  if (!exists("get_od_regions", mode = "function")) {
    stop("Missing get_od_regions(). Source Assets/Stable/OD_maps/fn_OD_region.R first.")
  }
  
  map_countries <- rnaturalearth::ne_countries(
    scale = 10,
    type = "countries",
    returnclass = "sf"
  ) %>%
    dplyr::select(iso_a3, brk_name) %>%
    dplyr::filter(!.data$brk_name %in% exclude_names)
  
  # Natural Earth sets France's ISO to -99 in some layers; enforce FRA for join consistency.
  map_countries$iso_a3[map_countries$brk_name == "France"] <- "FRA"
  
  geounit <- rnaturalearth::ne_countries(
    scale = 10,
    type = "map_units",
    returnclass = "sf"
  ) %>%
    dplyr::filter(.data$sovereignt %in% c("France", "Netherlands", "New Zealand")) %>%
    dplyr::filter(
      !.data$geounit %in% c(
        "Clipperton Island",
        "French Southern and Antarctic Lands",
        "Saint Pierre and Miquelon",
        "France",
        "Netherlands"
      )
    ) %>%
    dplyr::select(iso_a3, brk_name)
  
  geounit$brk_name[geounit$brk_name == "Caribbean Netherlands"] <- "Bonaire, Sint Eustatius, and Saba"
  geounit <- geounit[!geounit$iso_a3 %in% map_countries$iso_a3, ]
  
  world_sf <- rbind(map_countries, geounit[!geounit$iso_a3 %in% map_countries$iso_a3, ])
  
  region_table <- get_od_regions(world_sf$iso_a3)
  
  world_sf <- merge(
    world_sf %>% dplyr::select(-brk_name),
    region_table,
    by.x = "iso_a3",
    by.y = "ISO_A0",
    all.x = TRUE
  ) %>%
    dplyr::mutate(
      od_region = factor(.data$od_region, levels = c(
        "North & Central America",
        "South America",
        "Caribbean",
        "East & Southeast Asia",
        "South Asia",
        "Pacific Islands",
        "Sub-Saharan Africa",
        "Europe, Middle East & North Africa"
      ))
    )
  
  world_sf
}

compute_cum_ratio_by_iso3 <- function(data, year, month) {
  required_cols <- c("iso3", "Year", "Month", "cases", "Ave_season_monthly_cases")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in `data`: ", paste(missing_cols, collapse = ", "))
  }
  
  month <- as.integer(month)
  year <- as.integer(year)
  
  data %>%
    dplyr::filter(.data$Year == year, .data$Month <= month) %>%
    dplyr::group_by(.data$iso3) %>%
    dplyr::summarise(
      cum_low = sum(.data$Ave_season_monthly_cases, na.rm = TRUE),
      cum_high = sum(.data$cases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      cum_ratio = dplyr::if_else(.data$cum_low > 0, .data$cum_high / .data$cum_low, NA_real_),
      cum_ratio_capped = pmin(pmax(.data$cum_ratio, 0.5), 2)
    )
}

map_ratio_fill_scale <- function() {
  scale_fill_gradient2(
    low = "#7CC8AE",
    mid = "#F2D06B",
    high = "#E07A6E",
    midpoint = 1,
    limits = c(0.5, 2),
    name = "Relative cases",
    breaks = c(0.5, 1, 2),
    labels = c("<0.5x expected", "expected", ">2x expected"),
    na.value = "grey80"
  )
}

make_region_ratio_map <- function(
    data,
    region,
    year = as.integer(format(Sys.Date(), "%Y")),
    month = as.integer(format(Sys.Date(), "%m")) - 1,
    world_sf = NULL,
    bbox_lookup = NULL
) {
  if (is.null(bbox_lookup)) {
    bbox_lookup <- list(
      "South America" = list(xlim = c(-82, -34), ylim = c(-56, 13)),
      # extend northward so USA/Canada are visible
      "North & Central America" = list(xlim = c(-140, -50), ylim = c(5, 72)),
      "Caribbean" = list(xlim = c(-90, -58), ylim = c(9, 28)),
      # small extension upward
      "East & Southeast Asia" = list(xlim = c(90, 155), ylim = c(-12, 50)),
      "South Asia" = list(xlim = c(60, 100), ylim = c(0, 40)),
      # extend downward so Australia is not clipped
      "Pacific Islands" = list(xlim = c(100, 182), ylim = c(-48, 22)),
      # extend slightly upward
      "Sub-Saharan Africa" = list(xlim = c(-20, 55), ylim = c(-36, 28)),
      # small extension down
      "Europe, Middle East & North Africa" = list(xlim = c(-15, 65), ylim = c(0, 72))
    )
  }
  
  if (!region %in% names(bbox_lookup)) {
    stop("Unknown region '", region, "'. Expected one of: ", paste(names(bbox_lookup), collapse = ", "))
  }
  
  if (is.null(world_sf)) {
    world_sf <- build_world_sf()
  }
  
  ratio_df <- compute_cum_ratio_by_iso3(data = data, year = year, month = month)
  
  world_joined <- world_sf %>%
    dplyr::left_join(ratio_df, by = c("iso_a3" = "iso3"))
  
  region_sf <- world_joined %>%
    dplyr::filter(.data$od_region == region)
  
  # Within the cropped view, show all other land masses in light grey without borders
  background_sf <- world_joined %>%
    dplyr::filter(is.na(.data$od_region) | .data$od_region != region)
  
  xlim <- bbox_lookup[[region]]$xlim
  ylim <- bbox_lookup[[region]]$ylim
  
  ggplot() +
    geom_sf(data = background_sf, fill = "grey93", color = NA) +
    geom_sf(
      data = region_sf,
      aes(fill = .data$cum_ratio_capped),
      color = "grey40",
      linewidth = 0.2,
      alpha = 0.9
    ) +
    map_ratio_fill_scale() +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = "none"
    )
}

make_global_ratio_map <- function(
    data,
    year = as.integer(format(Sys.Date(), "%Y")),
    month = as.integer(format(Sys.Date(), "%m")) - 1,
    world_sf = NULL
) {
  if (is.null(world_sf)) {
    world_sf <- build_world_sf()
  }
  
  ratio_df <- compute_cum_ratio_by_iso3(data = data, year = year, month = month)
  
  world_joined <- world_sf %>%
    dplyr::left_join(ratio_df, by = c("iso_a3" = "iso3"))
  
  ggplot(world_joined) +
    geom_sf(aes(fill = .data$cum_ratio_capped), color = "grey70", linewidth = 0.1, alpha = 0.95) +
    map_ratio_fill_scale() +
    coord_sf(expand = FALSE) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = "none"
    )
}

