#' ---
#' title: "00_FUN_Visualise_monthly_pred_cases_vs_acc_cases_cor_results"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Calculate corrleation between actual and predicted cases by country, and generate plots.
#' 
#' Timeline:
#' =========
#' 22-05-2025: Prepared script.

Visualise_monthly_pred_cases_vs_acc_cases_cor_results_fun <- function(x){

  # Scatterplot of predicted vs actual cases 
  pred_cases_vs_acc_cases_plot <- ggplot(x) +
    geom_point(mapping = aes(x = Predicted_cases, y = Cases_clean)) +
    facet_wrap(~ Country, scales = "free") +
    labs(x = "Predicted cases", y = "Actual cases")

  # DF of acc vs pred cor results
  pred_prop_vs_acc_prop_cor <- x %>% 
    ungroup() %>% 
    group_by(Country, iso3) %>% 
    summarise(Correlation = cor(Actual_monthly_proportion, LOO_monthly_proportion)) %>% 
    arrange(Correlation)
  
  pred_prop_vs_acc_prop_cor$Country <-  factor(pred_prop_vs_acc_prop_cor$Country, levels = pred_prop_vs_acc_prop_cor$Country) 

  # Heatmap of acc vs pred cor results - ordered by performance
  pred_prop_vs_acc_prop_cor_plot <- ggplot(pred_prop_vs_acc_prop_cor) + 
    geom_tile(mapping = aes(x = 1, y = Country, fill = Correlation)) + 
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limits = c(-1, 1)) +
    labs(x = "", y = "Country", fill = "Correlation") +
    ggtitle("Correlation between actual and predicted monthly proportion of cases") +
    theme_minimal() +
    theme(axis.text.x = element_blank())
  
  #----- Heatmap of acc vs pred cor results - ordered by latitude 
  
  # Load country shapefiles + filter for target countries
  world_mapunit_shapefiles <- ne_countries(scale = 50, type = "map_units")
  world_mapunit_shapefiles_filtered <- world_mapunit_shapefiles %>%
    dplyr::select(iso_a3_eh, name, geometry) %>%
    rename(iso3 = iso_a3_eh) %>%
    filter(iso3 %in% x$iso3) %>% 
    group_by(iso3) %>% 
    summarise(geometry = st_union(geometry)) %>% 
    ungroup()
  
  pred_prop_vs_acc_prop_cor_shapefiles <- x %>% 
    full_join(., world_mapunit_shapefiles_filtered, by = "iso3") %>% 
    mutate(centroid = st_centroid(geometry)) %>% 
    mutate(Latitude = st_coordinates(centroid)[,2]) %>% 
    ungroup() 
  
  pred_prop_vs_acc_prop_cor_lat_ordered <- pred_prop_vs_acc_prop_cor_shapefiles %>% 
    group_by(Country, iso3, Latitude) %>% 
    summarise(Correlation = cor(Actual_monthly_proportion, LOO_monthly_proportion)) %>%
    ungroup() %>%
    mutate(dist_from_equator = abs(Latitude)) %>%
    arrange(dist_from_equator) 
  
  pred_prop_vs_acc_prop_cor_lat_ordered$Country <-  factor(pred_prop_vs_acc_prop_cor_lat_ordered$Country, levels = pred_prop_vs_acc_prop_cor_lat_ordered$Country) 
  
  
  pred_prop_vs_acc_prop_cor_plot_lat_ordered <- ggplot(pred_prop_vs_acc_prop_cor_lat_ordered) + 
    geom_tile(mapping = aes(x = 1, y = Country, fill = Correlation)) + 
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limits = c(-1, 1)) +
    labs(x = "", y = "Country", fill = "Correlation") +
    ggtitle("Correlation between actual and predicted monthly proportion of cases (Latitude ordered)") +
    theme_minimal() +
    theme(axis.text.x = element_blank())
  
  #----- Format results
  results <- list(Scatterplot = pred_cases_vs_acc_cases_plot,
                  DataFrame = pred_prop_vs_acc_prop_cor,
                  Cor_heatmap = pred_prop_vs_acc_prop_cor_plot,
                  Lat_ordered_cor_df = pred_prop_vs_acc_prop_cor_lat_ordered,
                  Lat_ordered_cor_heatmap = pred_prop_vs_acc_prop_cor_plot_lat_ordered)
  
  return(results)

}