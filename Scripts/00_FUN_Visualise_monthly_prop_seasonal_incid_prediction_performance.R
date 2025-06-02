#' ---
#' title: "00_FUN_Visualise_monthly_prop_seasonal_incid_prediction_performance"
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
#' 02-06-2025: Change names from annual to seasonal. Removed correlation code.

Visualise_monthly_prop_seasonal_incid_prediction_performance <- function(x){
  
  # Define theme 
  plot_theme <- theme(plot.title = element_text(size = 12),
                      legend.title = element_text(size = 10),
                      legend.text = element_text(size = 9),
                      axis.text = element_text(size = 9),
                      axis.title = element_text(size = 10))
  
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
  
  results <- list(rmse_data = x_rmse_data,
                  rmse_scatterplot = acc_vs_pred_annual_incid_rmse_scatterplot,
                  rmse_heatmap = pred_vs_acc_season_total_incid_rmse_plot)
  
}

