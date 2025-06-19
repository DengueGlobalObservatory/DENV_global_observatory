#' ---
#' title: "00_FUN_Visualise_iterative_improvement_in_monthly_prop_seasonal_incid_prediction_performance"
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
#' 28-05-2025: Saved dfs.
#' 02-06-2025: Removed correlation code - not on response scale. Changed ordering method to fct_reorder().  
#' 05-06-2025: Added code to combine heatmaps and scatterplots. 
#' 09-06-2025: Changed colour scale from blue-red to cyan-magenta.

Visualise_iterative_improvement_in_monthly_prop_seasonal_incid_prediction_performance <- function(x){
  
  # Define theme 
  plot_theme <- theme(plot.title = element_text(size = 12),
                      legend.title = element_text(size = 10),
                      legend.text = element_text(size = 9),
                      axis.text = element_text(size = 9),
                      axis.title = element_text(size = 10))
  
  #--------------- Improvement in prediction of total seasonal incid with each month of new data - data prep
  
  #' Calculate the rmse of months where 75% --> 90% of incid have occurred. Use this RMSE to calculate a RMSE ratio. 
  
  x_iterative_improvement_total_seasonal_incid_pred <- x %>% 
    group_by(Country, iso3, season) %>%
    mutate(Month_cutoff_above_0.75_of_total_cases = first(season_nMonth[Actual_cumulative_monthly_proportion >= 0.75]),
           Month_cutoff_above_0.8_of_total_cases = first(season_nMonth[Actual_cumulative_monthly_proportion >= 0.8]),
           Month_cutoff_above_0.85_of_total_cases = first(season_nMonth[Actual_cumulative_monthly_proportion >= 0.85]),
           Month_cutoff_above_0.9_of_total_cases = first(season_nMonth[Actual_cumulative_monthly_proportion >= 0.9])) %>% # Identify cut off month based on RMSE value
    ungroup() %>% 
    group_by(Country, iso3) %>%
    mutate(Ave_Month_cutoff_above_0.75_of_total_cases = round(mean(Month_cutoff_above_0.75_of_total_cases)),
           Ave_Month_cutoff_above_0.8_of_total_cases = round(mean(Month_cutoff_above_0.8_of_total_cases)),
           Ave_Month_cutoff_above_0.85_of_total_cases = round(mean(Month_cutoff_above_0.85_of_total_cases)), 
           Ave_Month_cutoff_above_0.9_of_total_cases = round(mean(Month_cutoff_above_0.9_of_total_cases))) %>% # Identify average cut off month (across all seasons)
    ungroup() %>% 
    group_by(Country, iso3, season_nMonth) %>% 
    mutate(Acc_vs_pred_total_incid_rmse = rmse(Actual_seasonal_total_cases_per_100000_pop, Predicted_seasonal_total_cases_per_100000_pop)) %>% 
    select(Country, iso3, season_nMonth, Acc_vs_pred_total_incid_rmse, 
           Ave_Month_cutoff_above_0.75_of_total_cases, Ave_Month_cutoff_above_0.8_of_total_cases, 
           Ave_Month_cutoff_above_0.85_of_total_cases, Ave_Month_cutoff_above_0.9_of_total_cases) %>%
    distinct() %>% 
    ungroup() %>% 
    group_by(Country) %>% 
    mutate(
      RMSE_at_75 = Acc_vs_pred_total_incid_rmse[which(season_nMonth == Ave_Month_cutoff_above_0.75_of_total_cases)[1]],
      RMSE_at_80 = Acc_vs_pred_total_incid_rmse[which(season_nMonth == Ave_Month_cutoff_above_0.8_of_total_cases)[1]],
      RMSE_at_85 = Acc_vs_pred_total_incid_rmse[which(season_nMonth == Ave_Month_cutoff_above_0.85_of_total_cases)[1]],
      RMSE_at_90 = Acc_vs_pred_total_incid_rmse[which(season_nMonth == Ave_Month_cutoff_above_0.9_of_total_cases)[1]]) %>% # Identify RMSE value at cutoff month 
    ungroup() %>% 
    mutate(Total_RMSE_to_0.75_prop_rmse = Acc_vs_pred_total_incid_rmse / RMSE_at_75,
           Total_RMSE_to_0.8_prop_rmse = Acc_vs_pred_total_incid_rmse / RMSE_at_80,
           Total_RMSE_to_0.85_prop_rmse = Acc_vs_pred_total_incid_rmse / RMSE_at_85,
           Total_RMSE_to_0.90_prop_rmse = Acc_vs_pred_total_incid_rmse / RMSE_at_90)  %>% # Normalise using RMSE value at cutoff month
    ungroup() %>%
    filter(!is.na(Country))
  
  #--------------- Ordering based on RMSE ratio to benchmark  
  x_iterative_improvement_total_seasonal_incid_pred_0.75_ordered <- x_iterative_improvement_total_seasonal_incid_pred %>% 
    mutate(Country = fct_reorder(Country, Total_RMSE_to_0.75_prop_rmse, .fun = mean, .desc = FALSE, .na_rm = FALSE)) 
  
  x_iterative_improvement_total_seasonal_incid_pred_0.8_ordered <- x_iterative_improvement_total_seasonal_incid_pred %>% 
    mutate(Country = fct_reorder(Country, Total_RMSE_to_0.8_prop_rmse, .fun = mean, .desc = FALSE, .na_rm = FALSE)) 
  
  x_iterative_improvement_total_seasonal_incid_pred_0.85_ordered <- x_iterative_improvement_total_seasonal_incid_pred %>% 
    mutate(Country = fct_reorder(Country, Total_RMSE_to_0.85_prop_rmse, .fun = mean, .desc = FALSE, .na_rm = FALSE)) 
  
  x_iterative_improvement_total_seasonal_incid_pred_0.9_ordered <- x_iterative_improvement_total_seasonal_incid_pred %>% 
    mutate(Country = fct_reorder(Country, Total_RMSE_to_0.90_prop_rmse, .fun = mean, .desc = FALSE, .na_rm = FALSE)) 
  
  #--------------- Generating heatmaps
  
  #----- 0.75
  iterative_improvement_total_seasonal_incid_pred_0.75_benchmark_heatmap <- ggplot(x_iterative_improvement_total_seasonal_incid_pred_0.75_ordered) + 
    geom_tile(mapping = aes(x = season_nMonth, y = Country, fill = Total_RMSE_to_0.75_prop_rmse)) + 
    scale_fill_gradient(low = "cyan", high = "magenta") +
    scale_x_continuous(breaks = 1:12) +
    labs(x = "", y = "Country", fill = "RMSE ratio") +
    ggtitle("Total seasonal incidence prediction RMSE normalised to month with >= 75% total cases") +
    theme_minimal() +
    plot_theme
  
  #----- 0.8
  iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_heatmap <- ggplot(x_iterative_improvement_total_seasonal_incid_pred_0.8_ordered) + 
    geom_tile(mapping = aes(x = season_nMonth, y = Country, fill = Total_RMSE_to_0.8_prop_rmse)) + 
    scale_fill_gradient(low = "cyan", high = "magenta") +
    scale_x_continuous(breaks = 1:12) +
    labs(x = "", y = "Country", fill = "RMSE ratio") +
    ggtitle("Total seasonal incidence prediction RMSE normalised to month with >= 80% total cases") +
    theme_minimal() +
    plot_theme
  
  iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_heatmap_minus_VCT <- ggplot(x_iterative_improvement_total_seasonal_incid_pred_0.8_ordered %>% 
                                                                                              filter(iso3 != "VCT")) + 
    geom_tile(mapping = aes(x = season_nMonth, y = Country, fill = Total_RMSE_to_0.8_prop_rmse)) + 
    scale_fill_gradient(low = "cyan", high = "magenta") +
    scale_x_continuous(breaks = 1:12) +
    labs(x = "", y = "Country", fill = "RMSE ratio") +
    ggtitle("Total seasonal incidence prediction RMSE normalised to month with >= 80% total cases") +
    theme_minimal() +
    plot_theme
  
  #----- 0.85
  iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_heatmap <- ggplot(x_iterative_improvement_total_seasonal_incid_pred_0.85_ordered) + 
    geom_tile(mapping = aes(x = season_nMonth, y = Country, fill = Total_RMSE_to_0.85_prop_rmse)) + 
    scale_fill_gradient(low = "cyan", high = "magenta") +
    scale_x_continuous(breaks = 1:12) +
    labs(x = "", y = "Country", fill = "RMSE ratio") +
    ggtitle("Total seasonal incidence prediction RMSE normalised to month with >= 85% total cases") +
    theme_minimal() +
    plot_theme
  
  iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_heatmap_minus_VCT <- ggplot(x_iterative_improvement_total_seasonal_incid_pred_0.85_ordered %>% 
                                                                                               filter(iso3 != "VCT")) + 
    geom_tile(mapping = aes(x = season_nMonth, y = Country, fill = Total_RMSE_to_0.85_prop_rmse)) + 
    scale_fill_gradient(low = "cyan", high = "magenta") +
    scale_x_continuous(breaks = 1:12) +
    labs(x = "", y = "Country", fill = "RMSE ratio") +
    ggtitle("Total seasonal incidence prediction RMSE normalised to month with >= 85% total cases") +
    theme_minimal() +
    plot_theme
  
  #----- 0.9
  iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_heatmap <- ggplot(x_iterative_improvement_total_seasonal_incid_pred_0.9_ordered) + 
    geom_tile(mapping = aes(x = season_nMonth, y = Country, fill = Total_RMSE_to_0.90_prop_rmse)) + 
    scale_fill_gradient(low = "cyan", high = "magenta") +
    scale_x_continuous(breaks = 1:12) +
    labs(x = "", y = "Country", fill = "RMSE ratio") +
    ggtitle("Total seasonal cases incidence RMSE normalised to month with >= 90% total cases") +
    theme_minimal() +
    plot_theme
  
  iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_heatmap_minus_VCT <- ggplot(x_iterative_improvement_total_seasonal_incid_pred_0.9_ordered %>%
                                                                                              filter(iso3 != "VCT")) + 
    geom_tile(mapping = aes(x = season_nMonth, y = Country, fill = Total_RMSE_to_0.90_prop_rmse)) + 
    scale_fill_gradient(low = "cyan", high = "magenta") +
    scale_x_continuous(breaks = 1:12) +
    labs(x = "", y = "Country", fill = "RMSE ratio") +
    ggtitle("Total seasonal incidence prediction RMSE normalised to month with >= 90% total cases") +
    theme_minimal() +
    plot_theme
  
  #--------------- Generating scatterplots
  
  iterative_improvement_total_seasonal_incid_pred_0.75_benchmark_scatterplot <- ggplot(x_iterative_improvement_total_seasonal_incid_pred_0.75_ordered) + 
    geom_point(mapping = aes(x = season_nMonth, y = Total_RMSE_to_0.75_prop_rmse)) + 
    scale_x_continuous(breaks = 1:12) +
    theme_minimal() +
    plot_theme +
    geom_hline(yintercept = 1, linetype = "dashed") +
    facet_wrap(~ Country, scales = "free") +
    labs(x = "Season Month", y = "RMSE ratio") +
    ggtitle("Total seasonal incidence prediction RMSE normalised to month with >= 75% total cases") 
  
  iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_scatterplot <- ggplot(x_iterative_improvement_total_seasonal_incid_pred_0.8_ordered) + 
    geom_point(mapping = aes(x = season_nMonth, y = Total_RMSE_to_0.8_prop_rmse)) + 
    scale_x_continuous(breaks = 1:12) +
    theme_minimal() +
    plot_theme +
    geom_hline(yintercept = 1, linetype = "dashed") +
    facet_wrap(~ Country, scales = "free") +
    labs(x = "Season Month", y = "RMSE ratio") +
    ggtitle("Total seasonal incidence prediction RMSE normalised to month with >= 80% total cases") 
  
  iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_scatterplot <- ggplot(x_iterative_improvement_total_seasonal_incid_pred_0.85_ordered) + 
    geom_point(mapping = aes(x = season_nMonth, y = Total_RMSE_to_0.85_prop_rmse)) + 
    scale_x_continuous(breaks = 1:12) +
    theme_minimal() +
    plot_theme +
    geom_hline(yintercept = 1, linetype = "dashed") +
    facet_wrap(~ Country, scales = "free") +
    labs(x = "Season Month", y = "RMSE ratio") +
    ggtitle("Total seasonal incidence prediction RMSE normalised to month with >= 85% total cases") 
  
  iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_scatterplot <- ggplot(x_iterative_improvement_total_seasonal_incid_pred_0.9_ordered) + 
    geom_point(mapping = aes(x = season_nMonth, y = Total_RMSE_to_0.90_prop_rmse)) + 
    scale_x_continuous(breaks = 1:12) +
    theme_minimal() +
    plot_theme +
    geom_hline(yintercept = 1, linetype = "dashed") +
    facet_wrap(~ Country, scales = "free") +
    labs(x = "Season Month", y = "RMSE ratio") +
    ggtitle("Total seasonal incidence prediction RMSE normalised to month with >= 90% total cases") 
  
  #--------------- Generating combined plots  
  
  #----- 0.75
  iterative_incid_improvement_0.75_benchmark_combined_plots <- plot_grid(iterative_improvement_total_seasonal_incid_pred_0.75_benchmark_heatmap,
                                                                   iterative_improvement_total_seasonal_incid_pred_0.75_benchmark_scatterplot, 
                                                                   ncol = 2)
  
  #----- 0.80
  iterative_incid_improvement_0.80_benchmark_combined_plots <- plot_grid(iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_heatmap,
                                                                   iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_scatterplot, 
                                                                   ncol = 2)
  iterative_incid_improvement_0.80_benchmark_minus_VCT_combined_plots <- plot_grid(iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_heatmap_minus_VCT,
                                                                             iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_scatterplot, 
                                                                             ncol = 2)
  
  #----- 0.85
  iterative_incid_improvement_0.85_benchmark_combined_plots <- plot_grid(iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_heatmap,
                                                                   iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_scatterplot, 
                                                                   ncol = 2)
  iterative_incid_improvement_0.85_benchmark_minus_VCT_combined_plots <- plot_grid(iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_heatmap_minus_VCT,
                                                                             iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_scatterplot, 
                                                                             ncol = 2)
  
  #----- 0.90
  iterative_incid_improvement_0.90_benchmark_combined_plots <- plot_grid(iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_heatmap,
                                                                   iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_scatterplot, 
                                                                   ncol = 2)
  iterative_incid_improvement_0.90_benchmark_minus_VCT_combined_plots <- plot_grid(iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_heatmap_minus_VCT,
                                                                             iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_scatterplot, 
                                                                             ncol = 2)
  
  #--------------- Preparing results 
  
  results <- list(Iterative_improvement_total_seasonal_incid_pred_data = x_iterative_improvement_total_seasonal_incid_pred,
                  # Iterative_improvement_total_seasonal_incid_pred_0.75_benchmark_heatmap = iterative_improvement_total_seasonal_incid_pred_0.75_benchmark_heatmap,
                  # Iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_heatmap = iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_heatmap,
                  # Iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_heatmap_minus_VCT = iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_heatmap_minus_VCT,
                  # Iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_heatmap = iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_heatmap,
                  # Iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_heatmap_minus_VCT = iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_heatmap_minus_VCT,
                  # Iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_heatmap = iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_heatmap,
                  # Iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_heatmap_minus_VCT = iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_heatmap_minus_VCT,
                  # Iterative_improvement_total_seasonal_incid_pred_0.75_benchmark_scatterplot = iterative_improvement_total_seasonal_incid_pred_0.75_benchmark_scatterplot,
                  # Iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_scatterplot = iterative_improvement_total_seasonal_incid_pred_0.8_benchmark_scatterplot,
                  # Iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_scatterplot = iterative_improvement_total_seasonal_incid_pred_0.85_benchmark_scatterplot,
                  # Iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_scatterplot = iterative_improvement_total_seasonal_incid_pred_0.9_benchmark_scatterplot,
                  iterative_incid_improvement_0.75_benchmark_combined_plots = iterative_incid_improvement_0.75_benchmark_combined_plots, 
                  iterative_incid_improvement_0.80_benchmark_combined_plots = iterative_incid_improvement_0.80_benchmark_combined_plots,
                  iterative_incid_improvement_0.80_benchmark_minus_VCT_combined_plots = iterative_incid_improvement_0.80_benchmark_minus_VCT_combined_plots,
                  iterative_incid_improvement_0.85_benchmark_combined_plots = iterative_incid_improvement_0.85_benchmark_combined_plots, 
                  iterative_incid_improvement_0.85_benchmark_minus_VCT_combined_plots = iterative_incid_improvement_0.85_benchmark_minus_VCT_combined_plots,
                  iterative_incid_improvement_0.90_benchmark_combined_plots = iterative_incid_improvement_0.90_benchmark_combined_plots,
                  iterative_incid_improvement_0.90_benchmark_minus_VCT_combined_plots = iterative_incid_improvement_0.90_benchmark_minus_VCT_combined_plots)
  
  return(results)
  
}