#' ---
#' title: "00_FUN_Visualise_monthly_pred_cases_vs_acc_cases_rmse_results"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Calculate rmse of actual and predicted cases by country, and generate plots.
#' 
#' Timeline:
#' =========
#' 22-05-2025: Prepared script.

Visualise_monthly_pred_cases_vs_acc_cases_rmse_results_fun <- function(x){
  
  #--------------- DF of acc vs pred RMSE
  pred_vs_acc_cases_RMSE <- x %>% 
    ungroup() %>% 
    group_by(Country, iso3) %>% 
    summarise(RMSE = rmse(Cases_clean, Predicted_cases)) %>% 
    arrange(RMSE) 
  
  pred_vs_acc_cases_RMSE$Country = factor(pred_vs_acc_cases_RMSE$Country, levels = pred_vs_acc_cases_RMSE$Country)
  
  #--------------- Heatmap of acc vs pred cor results - ordered by performance
  pred_vs_acc_cases_RMSE_plot <- ggplot(pred_vs_acc_cases_RMSE) + 
    geom_tile(mapping = aes(x = 1, y = Country, fill = RMSE)) + 
    scale_fill_gradient(low = "blue", high = "red") +
    labs(x = "", y = "Country", fill = "RMSE") +
    ggtitle("RMSE of actual vs predicted cases") +
    theme_minimal() +
    theme(axis.text.x = element_blank())
  
  pred_vs_acc_cases_RMSE_plot_without_Brazil <- ggplot(pred_vs_acc_cases_RMSE %>% 
                                                         filter(iso3 != "BRA")) + 
    geom_tile(mapping = aes(x = 1, y = Country, fill = RMSE)) + 
    scale_fill_gradient(low = "blue", high = "red") +
    labs(x = "", y = "Country", fill = "RMSE") +
    ggtitle("RMSE of actual vs predicted cases (w/out Brazil)") +
    theme_minimal() +
    theme(axis.text.x = element_blank())
  
  #--------------- Heatmap of acc vs pred cor results - ordered by latitude 
  
  # Load country shapefiles + filter for target countries
  world_mapunit_shapefiles <- ne_countries(scale = 50, type = "map_units")
  world_mapunit_shapefiles_filtered <- world_mapunit_shapefiles %>%
    dplyr::select(iso_a3_eh, name, geometry) %>%
    rename(iso3 = iso_a3_eh) %>%
    filter(iso3 %in% x$iso3) %>% 
    group_by(iso3) %>% 
    summarise(geometry = st_union(geometry)) %>% 
    ungroup()
  
  pred_vs_acc_cases_RMSE_shapefiles <- x %>% 
    full_join(., world_mapunit_shapefiles_filtered, by = "iso3") %>% 
    mutate(centroid = st_centroid(geometry)) %>% 
    mutate(Latitude = st_coordinates(centroid)[,2]) %>% 
    ungroup() 
  
  pred_vs_acc_cases_RMSE_lat_ordered <- pred_vs_acc_cases_RMSE_shapefiles %>% 
    group_by(Country, iso3, Latitude) %>% 
    summarise(RMSE = rmse(Cases_clean, Predicted_cases)) %>% 
    ungroup() %>%
    mutate(dist_from_equator = abs(Latitude)) %>%
    arrange(dist_from_equator) %>%
    mutate(Country = factor(Country, levels = unique(Country))) 
  
  pred_vs_acc_cases_RMSE_lat_ordered_plot <- ggplot(pred_vs_acc_cases_RMSE_lat_ordered) + 
    geom_tile(mapping = aes(x = 1, y = Country, fill = RMSE)) + 
    scale_fill_gradient(low = "blue", high = "red") +
    labs(x = "", y = "Country", fill = "RMSE") +
    ggtitle("RMSE of actual vs predicted cases (Latitude ordered") +
    theme_minimal() +
    theme(axis.text.x = element_blank())
  
  pred_vs_acc_cases_RMSE_lat_ordered_plot_without_Brazil <- ggplot(pred_vs_acc_cases_RMSE_lat_ordered %>%
                                                                     filter(iso3 != "BRA")) + 
    geom_tile(mapping = aes(x = 1, y = Country, fill = RMSE)) + 
    scale_fill_gradient(low = "blue", high = "red") +
    labs(x = "", y = "Country", fill = "RMSE") +
    ggtitle("RMSE of actual vs predicted cases (Latitude ordered, w/out Brazil)") +
    theme_minimal() +
    theme(axis.text.x = element_blank())
  
  #--------------- Format results
  results <- list(Cases_RMSE_df = pred_vs_acc_cases_RMSE_lat_ordered,
                  Cases_RMSE_heatmap = pred_vs_acc_cases_RMSE_plot,
                  Cases_RMSE_heatmap_minus_brazil = pred_vs_acc_cases_RMSE_plot_without_Brazil,
                  Lat_ordered_cases_RMSE_df = pred_vs_acc_cases_RMSE_lat_ordered,
                  Lat_ordered_cases_RMSE_heatmap = pred_vs_acc_cases_RMSE_lat_ordered_plot,
                  Lat_ordered_cases_RMSE_heatmap_minus_brazil = pred_vs_acc_cases_RMSE_lat_ordered_plot_without_Brazil)
  
  return(results)
  
  
}
