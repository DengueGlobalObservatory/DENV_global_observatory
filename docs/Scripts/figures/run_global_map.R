library(dplyr)
library(ggplot2)

source("Assets/Stable/OD_maps/fn_OD_region.R")
source("Scripts/figures/FUN_map.R")

get_latest_nowcast_path <- function(output_root = "Output", file_name = "DENV_cases_nowcast_output.csv") {
  output_dirs <- list.dirs(output_root, recursive = FALSE, full.names = TRUE)
  candidate_dirs <- output_dirs[file.exists(file.path(output_dirs, file_name))]
  
  if (length(candidate_dirs) == 0) {
    return(NA_character_)
  }
  
  folder_dates <- as.Date(gsub("_", "-", basename(candidate_dirs)))
  valid_dates <- !is.na(folder_dates)
  if (!any(valid_dates)) {
    return(file.path(candidate_dirs[[1]], file_name))
  }
  
  candidate_dirs <- candidate_dirs[valid_dates]
  folder_dates <- folder_dates[valid_dates]
  
  latest_idx <- which.max(folder_dates)
  file.path(candidate_dirs[[latest_idx]], file_name)
}

# ---- Parameters (edit as needed)
output_path <- "Assets/Dynamic/global_ratio_map.png"
data_source <- get_latest_nowcast_path()
fallback_data_source <- "Output/2025_10_12/DENV_cases_nowcast_output.csv"

current_year <- as.integer(format(Sys.Date(), "%Y"))
current_month <- as.integer(format(Sys.Date(), "%m"))
recent_month <- current_month - 1
if (recent_month == 0) recent_month <- 12

# ---- Load data
if (is.na(data_source) || !file.exists(data_source)) {
  message("Latest nowcast output not found. Falling back to: ", fallback_data_source)
  data_source <- fallback_data_source
}

message("Using data source: ", data_source)
data <- read.csv(data_source, check.names = FALSE)

col_names <- names(data)
if (length(col_names) > 0 && (col_names[1] == "" || col_names[1] == "X" || col_names[1] == "X.")) {
  data <- data %>% dplyr::select(-1)
}

# Ensure future months are NA (match dashboard behavior)
data <- data %>%
  dplyr::mutate(
    is_future = (.data$Year > current_year) | (.data$Year == current_year & .data$Month > recent_month),
    cases = dplyr::if_else(.data$is_future, NA_real_, .data$cases)
  ) %>%
  dplyr::select(-.data$is_future)

# ---- Build map + save
world_sf <- build_world_sf()
p <- make_global_ratio_map(data = data, year = current_year, month = recent_month, world_sf = world_sf)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(output_path, plot = p, width = 12, height = 7, dpi = 200, bg = "white")

message("Saved global map: ", output_path)

