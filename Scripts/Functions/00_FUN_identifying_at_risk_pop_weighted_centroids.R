#' ---
#' title: "00 identifying_at_risk_pop_weighted_centroids_fun"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Identify centroid of country weighted by at risk population. 
#' 
#' Timeline:
#' =========
#' 22-05-2025: Prepared script.

identifying_at_risk_pop_weighted_centroids_fun <- function(pop_raster, DENV_occurrence_raster, target_countries_iso3){
  
  #----- Load country shapefiles 
  world_mapunit_shapefiles <- ne_countries(scale = 50, type = "map_units")
  world_mapunit_shapefiles_clean <- world_mapunit_shapefiles %>%
    dplyr::select(iso_a3_eh, name, geometry) %>%
    rename(iso3 = iso_a3_eh)
  
  
  #----- Overlay occurrence raster on population raster 
  
  #' Population raster at 5x higher resolution than DEN occurrence raster - aggregate 5 fold.
  pop_raster_aggregated <- terra::aggregate(pop_raster, fact = 5, fun = "sum")
  
  # Check CRS + reset if necessary
  pop_raster_aggregated_crs <- st_crs(pop_raster_aggregated)
  DENV_occurrence_raster_crs <- st_crs(DENV_occurrence_raster)
  
  if(!identical(Pop_raster_2025_CRS, DEN_occurrence_raster_CRS)){
    crs(pop_raster_aggregated) <- DENV_occurrence_raster_crs
  }
  
  #' Check extent + crop if necessary 
  pop_raster_aggregated_ext <- ext(pop_raster_aggregated)
  DENV_occurrence_raster_ext <- ext(DENV_occurrence_raster)
  
  if(!identical(pop_raster_aggregated_ext, DENV_occurrence_raster_ext)){
    DENV_occurrence_raster_clean <- resample(DENV_occurrence_raster, pop_raster_aggregated, method = "near")
  } else if(identical(pop_raster_aggregated_ext, DENV_occurrence_raster_ext)){
    DENV_occurrence_raster_clean <- DENV_occurrence_raster
  }
  
  # Overlay rasters
  Pop_at_risk <- mask(pop_raster_aggregated, DENV_occurrence_raster_clean, maskvalue = 0)
  
  #----- Identify centroid 
  identifying_pop_centre_fun <- function(shapeunit, pop_raster){
    
    shapeunit_raster <- crop(pop_raster, shapeunit)
    cells <- which(!is.na(values(shapeunit_raster)))
    coords <- terra::xyFromCell(shapeunit_raster, cells)
    Pop <- values(shapeunit_raster)[cells]
    
    Pop_centre <- cbind(coords, Pop) %>% 
      as.data.frame() %>% 
      mutate(Total_pop = sum(Pop)) %>% 
      summarize(Pop_centre_x = sum(x * Pop) / Total_pop,
                Pop_centre_y = sum(y * Pop) / Total_pop) %>% 
      distinct()
    
    return(Pop_centre)
  }
  
  # Split into chunks
  chunk_size <- 10
  n <- nrow(world_mapunit_shapefiles_clean)
  chunks <- split(world_mapunit_shapefiles_clean, ceiling(seq_len(n)/chunk_size))
  
  # Process each chunk
  At_risk_pop_centroid <- map_dfr(chunks, ~{
    .x %>%
      mutate(
        Pop_centre = map(geometry, ~safely(identifying_pop_centre_fun)(.x, Pop_at_risk)$result)
      ) %>%
      unnest(Pop_centre)
  })
  
  At_risk_pop_centroid_clean <- At_risk_pop_centroid %>% 
    select(!geometry)
  
  return(At_risk_pop_centroid_clean)
  
}