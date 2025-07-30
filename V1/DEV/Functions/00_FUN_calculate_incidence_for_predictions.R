#' ---
#' title: "00_FUN_calculate_incidence_for_predictions"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========


calculate_incidence_for_predictions <- function(predictions, Pop_raster_2025, Pop_raster_2030){
  
  #---------------------- Process population rasters 
  
  # Aggregating from 1km squares to 5km squares
  Pop_raster_2025_agg <- terra::aggregate(Pop_raster_2025, fact = 5, fun = "sum")
  Pop_raster_2030_agg <- terra::aggregate(Pop_raster_2030, fact = 5, fun = "sum")
  
  print("Finished aggregating population rasters from 1km sq to 5km sq.")
  
  #---------------------- Prepare shapefiles 
  # Load country shapefiles + filter for target countries
  world_mapunit_shapefiles <- ne_countries(scale = 50, type = "map_units")
  world_mapunit_shapefiles_filtered <- world_mapunit_shapefiles %>%
    dplyr::select(iso_a3_eh, name, geometry) %>%
    rename(iso3 = iso_a3_eh) %>%
    filter(iso3 %in% predictions$iso3) %>% 
    group_by(iso3) %>% 
    summarise(geometry = st_union(geometry)) %>% 
    ungroup()
  
  target_crs <- st_crs(Pop_raster_2025)
  world_mapunit_shapefiles_final <- st_transform(world_mapunit_shapefiles_filtered, target_crs)
  
  #---------------------- Extract population for each country by year
  # Generate grid with pop raster years and combine to shapefiles df
  adm0_years_GHS <- expand.grid(iso3 = unique(predictions$iso3),
                                Year = c(2025, 2030)) %>%
    left_join(., world_mapunit_shapefiles_final, by = "iso3")
  
  adm0_years_GHS_pop <- adm0_years_GHS %>%
    mutate(Population = case_when(Year == 2025 ~ exact_extract(Pop_raster_2025_agg, geometry, fun = "sum"),
                                  Year == 2030 ~ exact_extract(Pop_raster_2030_agg, geometry, fun = "sum"),
                                  TRUE ~ NA))
  
  #---------------------- Prepping population data dfs to join with cases df
  
  adm0_years_GHS_pop_clean <- adm0_years_GHS_pop %>%
    mutate(ISO_code_Year = paste0(iso3, "_", Year)) %>%
    select(ISO_code_Year, Population)
  
  adm0_years_full <- expand.grid(iso3 = unique(predictions$iso3),
                                 Year = 2025:2030) %>%
    mutate(ISO_code_Year = paste0(iso3, "_", Year)) %>%
    full_join(., adm0_years_GHS_pop_clean, by = "ISO_code_Year")
  
  adm0_years_full_clean <- adm0_years_full %>%
    ungroup() %>%
    group_by(iso3) %>%
    arrange(iso3, Year) %>% 
    mutate(Population_interpolated = na_interpolation(Population))
  
  print("Finished identifying population for each country.")
  
  #---------------------- Joining population data df with cases by country df
  
  adm0_data_population <- predictions %>%
    group_by(Country, iso3) %>%
    mutate(Year = case_when(Any_data_available == "No_data_available" ~ NA,
                            Any_data_available == "Data_available" ~ first(na.omit(Year)))) %>%
    mutate(ISO_code_Year = paste0(iso3, "_", Year)) %>%
    full_join(., adm0_years_full_clean, by = "ISO_code_Year") %>%
    select(!iso3.y & !Year.y & !ISO_code_Year & !Population) %>%
    rename(iso3 = iso3.x,
           Year = Year.x) %>%
    ungroup() 
  
  adm0_data_incidence <- adm0_data_population %>%
    ungroup() %>%
    mutate(Monthly_incid = case_when(Data_status == "Observed" ~ round((Cases_clean / Population_interpolated) * 100000),
                                     Data_status == "Predicted" ~ (round(Predicted_monthly_cases / Population_interpolated) * 100000)),
           Predicted_monthly_incid_lower_CI95 = case_when(Data_status == "Observed" ~ NA,
                                                          Data_status == "Predicted" ~ round((Predicted_monthly_cases_lower_CI95 / Population_interpolated) * 100000)),
           Predicted_monthly_incid_upper_CI95 = case_when(Data_status == "Observed" ~ NA,
                                                          Data_status == "Predicted" ~ round((Predicted_monthly_cases_upper_CI95 / Population_interpolated) * 100000)))
             
  return(adm0_data_incidence)
  
}

