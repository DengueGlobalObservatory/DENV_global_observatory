#' ---
#' title: "00_FUN_Visualise_monthly_pred_vs_acc_incidence_rmse_results"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Calculate rmse of actual and predicted incidence by country, and generate plots.
#' 
#' Timeline:
#' =========
#' 22-05-2025: Prepared script.

Visualise_monthly_pred_vs_acc_incidence_rmse_results_fun <- function(x){
  
  
  #--------------- DF of acc vs pred RMSE
  pred_vs_acc_incid_RMSE <- x %>% 
    ungroup() %>% 
    group_by(Country, iso3) %>% 
    summarise(RMSE_incidence = rmse(Actual_cases_per_100000_pop, Predicted_cases_per_100000_pop)) %>% 
    arrange(RMSE_incidence) %>%
    na.omit()
  pred_vs_acc_incid_RMSE$Country = factor(pred_vs_acc_incid_RMSE$Country, levels = pred_vs_acc_incid_RMSE$Country)
  
  #--------------- Add in latitude ordering
  
  #----- Load country shapefiles + filter for target countries
  world_mapunit_shapefiles <- ne_countries(scale = 50, type = "map_units")
  world_mapunit_shapefiles_filtered <- world_mapunit_shapefiles %>%
    dplyr::select(iso_a3_eh, name, geometry) %>%
    rename(iso3 = iso_a3_eh) %>%
    filter(iso3 %in% x$iso3) %>% 
    group_by(iso3) %>% 
    summarise(geometry = st_union(geometry)) %>% 
    ungroup()
  
  #----- Combine shapefiles with input data
  pred_vs_acc_incid_RMSE_shapefiles <- x %>% 
    full_join(., world_mapunit_shapefiles_filtered, by = "iso3") %>% 
    mutate(centroid = st_centroid(geometry)) %>% 
    mutate(Latitude = st_coordinates(centroid)[,2]) %>% 
    ungroup() 
  
  #----- Calculate RMSE
  pred_vs_acc_incid_RMSE_lat_ordered <- pred_vs_acc_incid_RMSE_shapefiles %>% 
    group_by(Country, iso3, Latitude) %>% 
    summarise(RMSE_incidence = rmse(Actual_cases_per_100000_pop, Predicted_cases_per_100000_pop)) %>% 
    ungroup() %>%
    mutate(dist_from_equator = abs(Latitude)) %>%
    arrange(dist_from_equator) %>%
    na.omit()

    pred_vs_acc_incid_RMSE_lat_ordered$Country = factor(pred_vs_acc_incid_RMSE_lat_ordered$Country, levels = pred_vs_acc_incid_RMSE_lat_ordered$Country)
  
  #--------------- Plot
  pred_vs_acc_incid_RMSE_plot <- ggplot(pred_vs_acc_incid_RMSE) + 
    geom_tile(mapping = aes(x = 1, y = Country, fill = RMSE_incidence)) + 
    scale_fill_gradient(low = "blue", high = "red") +
    labs(x = "", y = "Country", fill = "RMSE incidence") +
    ggtitle("RMSE of actual vs pred incidence")
    theme_minimal() +
    theme(axis.text.x = element_blank())
  
    pred_vs_acc_incid_RMSE_plot_minus_st_barthelemy <- ggplot(pred_vs_acc_incid_RMSE %>% 
                                                                filter(iso3 != "BLM")) + 
      geom_tile(mapping = aes(x = 1, y = Country, fill = RMSE_incidence)) + 
      scale_fill_gradient(low = "blue", high = "red") +
      labs(x = "", y = "Country", fill = "RMSE incidence") +
      ggtitle("RMSE of actual vs pred incidence (w/out St Barthelemy)")
    theme_minimal() +
      theme(axis.text.x = element_blank())  
  
  pred_vs_acc_incid_RMSE_plot_lat_ordered <- ggplot(pred_vs_acc_incid_RMSE_lat_ordered) + 
    geom_tile(mapping = aes(x = 1, y = Country, fill = RMSE_incidence)) + 
    scale_fill_gradient(low = "blue", high = "red") +
    labs(x = "", y = "Country", fill = "RMSE incidence") +
    ggtitle("RMSE of actual vs pred incidence (Lat ordered)") +
    theme_minimal() +
    theme(axis.text.x = element_blank())
  
  pred_vs_acc_incid_RMSE_plot_lat_ordered_minus_st_barthelemy <- ggplot(pred_vs_acc_incid_RMSE_lat_ordered %>% 
                                                      filter(iso3 != "BLM")) + 
    geom_tile(mapping = aes(x = 1, y = Country, fill = RMSE_incidence)) + 
    scale_fill_gradient(low = "blue", high = "red") +
    labs(x = "", y = "Country", fill = "RMSE incidence") +
    ggtitle("RMSE of actual vs pred incidence (Lat ordered, w/out St Barthelemy)") +
    theme_minimal() +
    theme(axis.text.x = element_blank())
  
  results <- list(Incidence_RMSE_df = pred_vs_acc_incid_RMSE,
                  Incidence_RMSE_heatmap = pred_vs_acc_incid_RMSE_plot,
                  Incidence_RMSE_heatmap_minus_St_Barthelemy = pred_vs_acc_incid_RMSE_plot_minus_st_barthelemy,
                  Lat_ordered_incidence_RMSE_df = pred_vs_acc_incid_RMSE_lat_ordered,
                  Lat_ordered_ncidence_RMSE_heatmap = pred_vs_acc_incid_RMSE_plot_lat_ordered, 
                  Lat_ordered_ncidence_RMSE_heatmap_minus_St_Barthelemy = pred_vs_acc_incid_RMSE_plot_lat_ordered_minus_st_barthelemy)
  
  return(results)
  
  }