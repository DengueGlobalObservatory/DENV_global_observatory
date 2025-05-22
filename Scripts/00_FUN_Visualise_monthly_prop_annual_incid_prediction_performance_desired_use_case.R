#' ---
#' title: "00_FUN_Visualise_monthly_prop_annual_incid_prediction_performance_desired_use_case"
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

Visualise_monthly_prop_annual_incid_prediction_performance_desired_use_case <- function(x){
  
  # Define theme 
  plot_theme <- theme(plot.title = element_text(size = 12),
                      legend.title = element_text(size = 10),
                      legend.text = element_text(size = 9),
                      axis.text = element_text(size = 9),
                      axis.title = element_text(size = 10))
  
  #--------------- Correlation of actual vs predicted total seasonal incidence 
  
  #----- Scatterplot
  
  x_cor_data <- x %>% 
    na.omit() %>% 
    ungroup() %>% 
    group_by(Country, iso3, season_nMonth) %>% 
    summarise(Acc_vs_pred_total_cor = cor(Actual_seasonal_total_cases_per_100000_pop, Predicted_seasonal_total_cases_per_100000_pop)) %>% 
    arrange(Acc_vs_pred_total_cor) 
  x_cor_data$Country = factor(x_cor_data$Country, levels = unique(x_cor_data$Country))
  
  acc_vs_pred_annual_incid_cor_scatterplot <- ggplot(x_cor_data) + 
    geom_point(mapping = aes(x = season_nMonth, y = Acc_vs_pred_total_cor)) + 
    scale_x_continuous(breaks = 1:12) +
    scale_y_continuous(limits = c(0,1)) +
    theme_minimal() + 
    plot_theme +
    facet_wrap(~ Country, scales = "free") +
    labs(x = "Season Month", y = "Correlation") + 
    ggtitle("Comparing actual and predicted seasonal total incidence")
  
  #----- Heatmap
  
  pred_vs_acc_season_total_incid_cor_plot <- ggplot(x_cor_data) + 
    geom_tile(mapping = aes(x = season_nMonth, y = Country, fill = Acc_vs_pred_total_cor)) + 
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limits = c(-1, 1)) +
    scale_x_continuous(breaks = 1:12) +
    labs(x = "Season Month", y = "Country", fill = "Correlation") +
    ggtitle("Correlation between actual and predicted seasonal total incidence") +
    theme_minimal() +
    plot_theme
  
  #--------------- RMSE between actual and predicted total seasonal incidence 
  
  #----- Scatterplot
  
  x_rmse_data <- x %>% 
    na.omit() %>% 
    ungroup() %>% 
    group_by(Country, iso3, season_nMonth) %>% 
    summarise(Acc_vs_pred_total_rmse = rmse(Actual_seasonal_total_cases_per_100000_pop, Predicted_seasonal_total_cases_per_100000_pop)) %>% 
    arrange(Acc_vs_pred_total_rmse) 
  x_rmse_data$Country = factor(x_rmse_data$Country, levels = unique(x_rmse_data$Country))
  
  
  acc_vs_pred_annual_incid_rmse_scatterplot <- ggplot(x_rmse_data) + 
    geom_point(mapping = aes(x = season_nMonth, y = Acc_vs_pred_total_rmse)) + 
    scale_x_continuous(breaks = 1:12) +
    theme_minimal() +
    plot_theme +
    facet_wrap(~ Country, scales = "free") +
    labs(x = "Season Month", y = "RMSE") +
    ggtitle("RMSE of actual vs predicted total seasonal incidence") 
  
  #----- heatmap
  
  pred_vs_acc_season_total_incid_rmse_plot <- ggplot(x_rmse_data) + 
    geom_tile(mapping = aes(x = season_nMonth, y = Country, fill = Acc_vs_pred_total_rmse)) + 
    scale_fill_gradient(low = "blue", high = "red") +
    scale_x_continuous(breaks = 1:12) +
    labs(x = "", y = "Country", fill = "RMSE") +
    ggtitle("RMSE of actual vs predicted total seasonal incidence") +
    theme_minimal() +
    plot_theme
  
  #--------------- Preparing results 
  
  results <- list(cor_scatterplot = acc_vs_pred_annual_incid_cor_scatterplot,
                  cor_heatmap = pred_vs_acc_season_total_incid_cor_plot,
                  rmse_scatterplot = acc_vs_pred_annual_incid_rmse_scatterplot,
                  rmse_heatmap = pred_vs_acc_season_total_incid_rmse_plot)
  
}

