#' ---
#' title: "00_FUN_population_by_country"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
# Clean combined WHO and OpenDengue dataset 
#'  
#' Timeline:
#' =========
#' 21-05-2025: Prepared function.
#'

population_by_country <- function(iso3_list, pop_raster){
  
  #----- Load country shapefiles + filter for target countries
  world_mapunit_shapefiles <- ne_countries(scale = 50, type = "map_units")
  world_mapunit_shapefiles_clean <- world_mapunit_shapefiles %>%
    dplyr::select(iso_a3_eh, name, geometry) %>%
    rename(iso3 = iso_a3_eh) %>%
    filter(iso3 %in% iso3_list)
  
  #----- Prepare population raster and shapefiles (set CRS, check extent etc...)
  
  #' Population raster at 5x higher resolution than DEN occurrence raster - aggregate 5 fold.
  pop_raster_aggregated <- terra::aggregate(pop_raster, fact = 5, fun = "sum")
  
  # Check CRS + reset if necessary
  pop_raster_aggregated_crs <- st_crs(pop_raster_aggregated)
  world_shapefiles_crs <- st_crs(world_mapunit_shapefiles_clean)
  
  if(!identical(pop_raster_aggregated_crs, world_shapefiles_crs)){
    world_mapunit_shapefiles_final <- st_transform(world_mapunit_shapefiles_clean, pop_raster_aggregated_crs)
  }
  
  #----- Calculate population 
  target_countries_pop <- world_mapunit_shapefiles_clean %>%
    na.omit() %>%
    mutate(Population = exact_extract(pop_raster_aggregated, geometry, fun = "sum")) %>% 
    select(!geometry)
  
  return(target_countries_pop)
  
}