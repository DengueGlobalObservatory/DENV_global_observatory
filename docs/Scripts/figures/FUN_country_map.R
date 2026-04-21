library(dplyr)
library(ggplot2)

# Shared regional bounding boxes aligned with Scripts/figures/FUN_map.R
country_map_bbox_lookup <- function() {
  list(
    "South America" = list(xlim = c(-82, -34), ylim = c(-56, 13)),
    "North & Central America" = list(xlim = c(-140, -50), ylim = c(5, 72)),
    "Caribbean" = list(xlim = c(-90, -58), ylim = c(9, 28)),
    "East & Southeast Asia" = list(xlim = c(90, 155), ylim = c(-12, 50)),
    "South Asia" = list(xlim = c(60, 100), ylim = c(0, 40)),
    "Pacific Islands" = list(xlim = c(100, 182), ylim = c(-48, 22)),
    "Sub-Saharan Africa" = list(xlim = c(-20, 55), ylim = c(-36, 28)),
    "Europe, Middle East & North Africa" = list(xlim = c(-15, 65), ylim = c(0, 72))
  )
}

make_country_context_map <- function(
    iso3,
    region,
    world_sf = NULL,
    bbox_lookup = NULL,
    highlight_fill = "grey55",
    highlight_border = "grey25"
) {
  if (is.null(bbox_lookup)) {
    bbox_lookup <- country_map_bbox_lookup()
  }
  if (!region %in% names(bbox_lookup)) {
    stop("Unknown region '", region, "'.")
  }

  if (is.null(world_sf)) {
    if (!exists("build_world_sf", mode = "function")) {
      source("Scripts/figures/FUN_map.R")
    }
    if (!exists("get_od_regions", mode = "function")) {
      source("Assets/Stable/OD_maps/fn_OD_region.R")
    }
    build_world_sf_fn <- get("build_world_sf", mode = "function")
    world_sf <- build_world_sf_fn()
  }

  region_sf <- world_sf %>% dplyr::filter(.data$od_region == region)
  target_sf <- region_sf %>% dplyr::filter(.data$iso_a3 == iso3)
  if (nrow(target_sf) == 0) {
    stop("No polygon found for iso3=", iso3, " in region=", region)
  }

  region_others_sf <- region_sf %>% dplyr::filter(.data$iso_a3 != iso3)
  background_sf <- world_sf %>% dplyr::filter(is.na(.data$od_region) | .data$od_region != region)

  xlim <- bbox_lookup[[region]]$xlim
  ylim <- bbox_lookup[[region]]$ylim

  ggplot() +
    geom_sf(data = background_sf, fill = "grey95", color = NA) +
    geom_sf(data = region_others_sf, fill = "grey84", color = "white", linewidth = 0.2) +
    geom_sf(data = target_sf, fill = highlight_fill, color = highlight_border, linewidth = 0.45) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

save_country_context_map <- function(
    iso3,
    region,
    out_file = file.path("Assets", "Stable", "country_maps", paste0(tolower(iso3), ".png")),
    width = 7,
    height = 7,
    dpi = 220
) {
  plot_obj <- make_country_context_map(iso3 = iso3, region = region)
  out_dir <- dirname(out_file)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  ggplot2::ggsave(out_file, plot = plot_obj, width = width, height = height, dpi = dpi, bg = "white")
  invisible(out_file)
}
