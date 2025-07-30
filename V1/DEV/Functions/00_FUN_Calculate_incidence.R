#' ---
#' title: "00_FUN_Calculate_incidence"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Calculate incidence. 
#' 
#' Timeline:
#' =========
#' 22-05-2025: Prepared script. 


calculate_incidence <- function(x, 
                                Pop_raster_1990, Pop_raster_1995, Pop_raster_2000, Pop_raster_2005, 
                                Pop_raster_2010, Pop_raster_2015, Pop_raster_2020, Pop_raster_2025){
  
  #---------------------- Process population rasters 
  
  # Aggregating from 1km squares to 5km squares
  Pop_raster_1990_agg <- terra::aggregate(Pop_raster_1990, fact = 5, fun = "sum")
  Pop_raster_1995_agg <- terra::aggregate(Pop_raster_1995, fact = 5, fun = "sum")
  Pop_raster_2000_agg <- terra::aggregate(Pop_raster_2000, fact = 5, fun = "sum")
  Pop_raster_2005_agg <- terra::aggregate(Pop_raster_2005, fact = 5, fun = "sum")
  Pop_raster_2010_agg <- terra::aggregate(Pop_raster_2010, fact = 5, fun = "sum")
  Pop_raster_2015_agg <- terra::aggregate(Pop_raster_2015, fact = 5, fun = "sum")
  Pop_raster_2020_agg <- terra::aggregate(Pop_raster_2020, fact = 5, fun = "sum")
  Pop_raster_2025_agg <- terra::aggregate(Pop_raster_2025, fact = 5, fun = "sum")
  
  
  #---------------------- Prepare shapefiles 
  # Load country shapefiles + filter for target countries
  world_mapunit_shapefiles <- ne_countries(scale = 50, type = "map_units")
  world_mapunit_shapefiles_filtered <- world_mapunit_shapefiles %>%
    dplyr::select(iso_a3_eh, name, geometry) %>%
    rename(iso3 = iso_a3_eh) %>%
    filter(iso3 %in% x$iso3) %>% 
    group_by(iso3) %>% 
    summarise(geometry = st_union(geometry)) %>% 
    ungroup()
  
  target_crs <- st_crs(Pop_raster_2015)
  world_mapunit_shapefiles_final <- st_transform(world_mapunit_shapefiles_filtered, target_crs)
  
  #---------------------- Extract population for each country by year
  # Generate grid with pop raster years and combine to shapefiles df
  adm0_years_GHS <- expand.grid(iso3 = unique(x$iso3),
                                Year = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025)) %>%
    left_join(., world_mapunit_shapefiles_final, by = "iso3")
  
  adm0_years_GHS_pop <- adm0_years_GHS %>%
    mutate(Population = case_when(Year == 1990 ~ exact_extract(Pop_raster_1990_agg, geometry, fun = "sum"),
                                  Year == 1995 ~ exact_extract(Pop_raster_1995_agg, geometry, fun = "sum"),
                                  Year == 2000 ~ exact_extract(Pop_raster_2000_agg, geometry, fun = "sum"),
                                  Year == 2005 ~ exact_extract(Pop_raster_2005_agg, geometry, fun = "sum"),
                                  Year == 2010 ~ exact_extract(Pop_raster_2010_agg, geometry, fun = "sum"),
                                  Year == 2015 ~ exact_extract(Pop_raster_2015_agg, geometry, fun = "sum"),
                                  Year == 2020 ~ exact_extract(Pop_raster_2020_agg, geometry, fun = "sum"),
                                  Year == 2025 ~ exact_extract(Pop_raster_2025_agg, geometry, fun = "sum"),
                                  TRUE ~ NA))
  
  #---------------------- Prepping population data dfs to join with cases df
  
  adm0_years_GHS_pop_clean <- adm0_years_GHS_pop %>%
    mutate(ISO_code_Year = paste0(iso3, "_", Year)) %>%
    select(ISO_code_Year, Population)
  
  adm0_years_full <- expand.grid(iso3 = unique(x$iso3),
                                 Year = 1990:2025) %>%
    mutate(ISO_code_Year = paste0(iso3, "_", Year)) %>%
    full_join(., adm0_years_GHS_pop_clean, by = "ISO_code_Year")
  
  adm0_years_full_clean <- adm0_years_full %>%
    ungroup() %>%
    group_by(iso3) %>%
    arrange(iso3, Year) %>% 
    mutate(Population_interpolated = na_interpolation(Population))
  
  #---------------------- Joining population data df with cases by country df
  
  adm0_data_population <- x %>%
    mutate(ISO_code_Year = paste0(iso3, "_", Year)) %>%
    full_join(., adm0_years_full_clean, by = "ISO_code_Year") %>%
    select(!iso3.y & !Year.y & !ISO_code_Year & !Population) %>%
    rename(iso3 = iso3.x,
           Year = Year.x)
  
  adm0_data_incidence <- adm0_data_population %>%
    ungroup() %>%
    mutate(Actual_cases_per_100000_pop = round((Cases_clean/Population_interpolated)*100000),
           Predicted_cases_per_100000_pop = round((Predicted_cases/Population_interpolated)*100000),)
  
  return(adm0_data_incidence)

  
}