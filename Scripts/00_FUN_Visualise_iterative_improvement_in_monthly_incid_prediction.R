#' ---
#' title: "00_FUN_Visualise_iterative_improvement_in_monthly_incid_prediction"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Calculate rmse of actual and predicted monthly cases by country. Then calculate the iterative improvement as lagged RMSE ratio (divide by prev month) and generate plots.
#' 
#' Timeline:
#' =========
#' 22-05-2025: Prepared script.
#' 04-06-2025: Removed correlation code. Added in code to split plots along similar groups to that of raw error for pairing.

Visualise_iterative_improvement_in_monthly_incid_prediction <- function(x){
  
  # Define theme 
  plot_theme <- theme(plot.title = element_text(size = 12),
                      legend.title = element_text(size = 10),
                      legend.text = element_text(size = 9),
                      axis.text = element_text(size = 9),
                      axis.title = element_text(size = 10))

  #--------------- Generate data for plotting 
  
  #----- Lag RMSE and MAE values 
  x_data_lagged <- x %>% 
    ungroup() %>% 
    group_by(Country, iso3, Month_to_predict) %>% 
    arrange(season_nMonth) %>% 
    mutate(Lag_acc_vs_pred_monthly_cases_rmse = lag(Acc_vs_pred_monthly_cases_rmse, n = 1),
           Lag_acc_vs_pred_monthly_incid_rmse = lag(Acc_vs_pred_monthly_incid_rmse, n = 1),
           Lag_acc_vs_pred_monthly_cases_mae = lag(Acc_vs_pred_monthly_cases_mae, n = 1),
           Lag_acc_vs_pred_monthly_incid_mae = lag(Acc_vs_pred_monthly_incid_mae, n = 1)) %>% 
    mutate(Iterative_improvement_in_cases_prediction_rmse = Lag_acc_vs_pred_monthly_cases_rmse / Acc_vs_pred_monthly_cases_rmse,
           Iterative_improvement_in_incid_prediction_rmse = Lag_acc_vs_pred_monthly_incid_rmse / Acc_vs_pred_monthly_incid_rmse,
           Iterative_improvement_in_cases_prediction_mae = Lag_acc_vs_pred_monthly_cases_mae / Acc_vs_pred_monthly_cases_mae,
           Iterative_improvement_in_incid_prediction_mae = Lag_acc_vs_pred_monthly_incid_mae / Acc_vs_pred_monthly_incid_mae) %>%
    ungroup
  
  #----- Ordering by RMSE
  x_data_lagged_rmse_cases_ordered <- x_data_lagged %>% 
    group_by(Cases_rmse_groups) %>%
    mutate(Country = fct_reorder(Country, Acc_vs_pred_monthly_cases_rmse, .fun = mean, .desc = FALSE, .na_rm = FALSE))
  
  x_data_lagged_rmse_incid_ordered <- x_data_lagged %>% 
    group_by(Incid_rmse_groups) %>%
    mutate(Country = fct_reorder(Country, Acc_vs_pred_monthly_incid_rmse, .fun = mean, .desc = FALSE, .na_rm = FALSE))
  
  #----- Ordering by MAE
  x_data_lagged_mae_cases_ordered <- x_data_lagged %>% 
    group_by(Cases_mae_groups) %>%
    mutate(Country = fct_reorder(Country, Acc_vs_pred_monthly_cases_mae, .fun = mean, .desc = FALSE, .na_rm = FALSE))
  
  x_data_lagged_mae_incid_ordered <- x_data_lagged %>% 
    group_by(Incid_mae_groups) %>%
    mutate(Country = fct_reorder(Country, Acc_vs_pred_monthly_incid_mae, .fun = mean, .desc = FALSE, .na_rm = FALSE))
  
  #--------------- Iterative improvement in case prediction RMSE with each new month of data
  
  #----- Cases RMSE ordered
  iterative_improvement_in_monthly_case_prediction_rmse_heatmap <- ggplot(x_data_lagged_rmse_cases_ordered) + 
    geom_tile(mapping = aes(x = Month_to_predict, y = season_nMonth, fill = Iterative_improvement_in_cases_prediction_rmse)) + 
    scale_fill_gradient(low = "cyan", high = "magenta") +
    geom_vline(mapping = aes(xintercept = Average_season_peak_month, linetype = "Peak month")) +
    scale_x_continuous(breaks = 1:12) +
    scale_y_continuous(breaks = 1:12) +
    facet_wrap(~ Country, scales = "free") +
    theme_minimal() +
    plot_theme +
    scale_linetype_manual(values = c("Peak month" = "dashed")) +
    guides(linetype = guide_legend(title = NULL)) + 
    labs(x = "Month to predict", y = "Season Month", fill = "Stepwise RMSE\nratio") + 
    ggtitle("Iterative improvement in monthly case prediction")
  
  #----- Incidence RMSE ordered
  iterative_improvement_in_monthly_incid_prediction_rmse_heatmap <- ggplot(x_data_lagged_rmse_incid_ordered) + 
    geom_tile(mapping = aes(x = Month_to_predict, y = season_nMonth, fill = Iterative_improvement_in_incid_prediction_rmse)) + 
    scale_fill_gradient(low = "cyan", high = "magenta") +
    geom_vline(mapping = aes(xintercept = Average_season_peak_month, linetype = "Peak month")) +
    scale_x_continuous(breaks = 1:12) +
    scale_y_continuous(breaks = 1:12) +
    facet_wrap(~ Country, scales = "free") +
    theme_minimal() +
    plot_theme +
    scale_linetype_manual(values = c("Peak month" = "dashed")) +
    guides(linetype = guide_legend(title = NULL)) + 
    labs(x = "Month to predict", y = "Season Month", fill = "Stepwise RMSE\nratio") + 
    ggtitle("Iterative improvement in monthly incidence prediction")
  
  #----- Cases MAE ordered
  iterative_improvement_in_monthly_case_prediction_mae_heatmap <- ggplot(x_data_lagged_mae_cases_ordered) + 
    geom_tile(mapping = aes(x = Month_to_predict, y = season_nMonth, fill = Iterative_improvement_in_cases_prediction_mae)) + 
    scale_fill_gradient(low = "cyan", high = "magenta") +
    geom_vline(mapping = aes(xintercept = Average_season_peak_month, linetype = "Peak month")) +
    scale_x_continuous(breaks = 1:12) +
    scale_y_continuous(breaks = 1:12) +
    facet_wrap(~ Country, scales = "free") +
    theme_minimal() +
    plot_theme +
    scale_linetype_manual(values = c("Peak month" = "dashed")) +
    guides(linetype = guide_legend(title = NULL)) + 
    labs(x = "Month to predict", y = "Season Month", fill = "Stepwise MAE\nratio") + 
    ggtitle("Iterative improvement in monthly case prediction")
  
  #----- Incidence RMSE ordered
  iterative_improvement_in_monthly_incid_prediction_mae_heatmap <- ggplot(x_data_lagged_mae_incid_ordered) + 
    geom_tile(mapping = aes(x = Month_to_predict, y = season_nMonth, fill = Iterative_improvement_in_incid_prediction_mae)) + 
    scale_fill_gradient(low = "cyan", high = "magenta") +
    geom_vline(mapping = aes(xintercept = Average_season_peak_month, linetype = "Peak month")) +
    scale_x_continuous(breaks = 1:12) +
    scale_y_continuous(breaks = 1:12) +
    facet_wrap(~ Country, scales = "free") +
    theme_minimal() +
    plot_theme +
    scale_linetype_manual(values = c("Peak month" = "dashed")) +
    guides(linetype = guide_legend(title = NULL)) + 
    labs(x = "Month to predict", y = "Season Month", fill = "Stepwise MAE\nratio") + 
    ggtitle("Iterative improvement in monthly incidence prediction")
  
  #--------------- Iterative improvement in case prediction RMSE with each new month of data - split into bands
  
  #----- Cases RMSE ordered
  Case_lagged_rmse_ratio_plots <- list()
  for (group in unique(x_data_lagged_rmse_cases_ordered$Cases_rmse_groups)){
    df_subset <- x_data_lagged_rmse_cases_ordered %>% filter(Cases_rmse_groups == group)
    p <- ggplot(df_subset) +
      geom_tile(mapping = aes(x = Month_to_predict, y = season_nMonth, fill = Iterative_improvement_in_cases_prediction_rmse)) + 
      scale_fill_gradient(low = "cyan", high = "magenta") +
      geom_vline(mapping = aes(xintercept = Average_season_peak_month, linetype = "Peak month")) +
      scale_x_continuous(breaks = 1:12) +
      scale_y_continuous(breaks = 1:12) +
      facet_wrap(~ Country, scales = "free") +
      theme_minimal() +
      plot_theme +
      scale_linetype_manual(values = c("Peak month" = "dashed")) +
      guides(linetype = guide_legend(title = NULL)) + 
      labs(x = "Month to predict", y = "Season Month", fill = "Stepwise RMSE\nratio") + 
      ggtitle("Iterative improvement in monthly case prediction")  
    Case_lagged_rmse_ratio_plots[[paste0("Group", group)]] <- p
  }
  
  #----- Incidence RMSE ordered
  Incid_lagged_rmse_ratio_plots <- list()
  for (group in unique(x_data_lagged_rmse_incid_ordered$Incid_rmse_groups)){
    df_subset <- x_data_lagged_rmse_incid_ordered %>% filter(Incid_rmse_groups == group)
    p <- ggplot(df_subset) +
      geom_tile(mapping = aes(x = Month_to_predict, y = season_nMonth, fill = Iterative_improvement_in_incid_prediction_rmse)) +
      scale_fill_gradient(low = "cyan", high = "magenta") +
      geom_vline(mapping = aes(xintercept = Average_season_peak_month, linetype = "Peak month")) +
      scale_x_continuous(breaks = 1:12) +
      scale_y_continuous(breaks = 1:12) +
      facet_wrap(~ Country, scales = "free") +
      theme_minimal() +
      plot_theme +
      scale_linetype_manual(values = c("Peak month" = "dashed")) +
      guides(linetype = guide_legend(title = NULL)) + 
      labs(x = "Month to predict", y = "Season Month", fill = "Stepwise RMSE\nratio") + 
      ggtitle("Iterative improvement in monthly incidence prediction")  
    Incid_lagged_rmse_ratio_plots[[paste0("Group", group)]] <- p
  }
  
  #----- Cases MAE ordered
  Case_lagged_mae_ratio_plots <- list()
  for (group in unique(x_data_lagged_mae_cases_ordered$Cases_mae_groups)){
    df_subset <- x_data_lagged_mae_cases_ordered %>% filter(Cases_mae_groups == group)
    p <- ggplot(df_subset) +
      geom_tile(mapping = aes(x = Month_to_predict, y = season_nMonth, fill = Iterative_improvement_in_cases_prediction_mae)) + 
      geom_vline(mapping = aes(xintercept = Average_season_peak_month, linetype = "Peak month")) +
      scale_fill_gradient(low = "cyan", high = "magenta") +
      scale_x_continuous(breaks = 1:12) +
      scale_y_continuous(breaks = 1:12) +
      facet_wrap(~ Country, scales = "free") +
      theme_minimal() +
      plot_theme +
      scale_linetype_manual(values = c("Peak month" = "dashed")) +
      guides(linetype = guide_legend(title = NULL)) + 
      labs(x = "Month to predict", y = "Season Month", fill = "Stepwise MAE\nratio") + 
      ggtitle("Iterative improvement in monthly case prediction")  
    Case_lagged_mae_ratio_plots[[paste0("Group", group)]] <- p
  }
  
  #----- Incidence MAE ordered
  Incid_lagged_mae_ratio_plots <- list()
  for (group in unique(x_data_lagged_mae_incid_ordered$Incid_mae_groups)){
    df_subset <- x_data_lagged_mae_incid_ordered %>% filter(Incid_mae_groups == group)
    p <- ggplot(df_subset) +
      geom_tile(mapping = aes(x = Month_to_predict, y = season_nMonth, fill = Iterative_improvement_in_incid_prediction_mae)) + 
      scale_fill_gradient(low = "cyan", high = "magenta") +
      geom_vline(mapping = aes(xintercept = Average_season_peak_month, linetype = "Peak month")) +
      scale_x_continuous(breaks = 1:12) +
      scale_y_continuous(breaks = 1:12) +
      facet_wrap(~ Country, scales = "free") +
      theme_minimal() +
      plot_theme +
      scale_linetype_manual(values = c("Peak month" = "dashed")) +
      guides(linetype = guide_legend(title = NULL)) + 
      labs(x = "Month to predict", y = "Season Month", fill = "Stepwise MAE\nratio") + 
      ggtitle("Iterative improvement in monthly incidence prediction")  
    Incid_lagged_mae_ratio_plots[[paste0("Group", group)]] <- p
  }
  
  
  #--------------- Preparing results 
  
  results <- list(Plot_data = x_data_lagged, 
                  Iterative_improvement_in_monthly_case_prediction_heatmap_rmse = iterative_improvement_in_monthly_case_prediction_rmse_heatmap,
                  Iterative_improvement_in_monthly_incid_prediction_heatmap_rmse = iterative_improvement_in_monthly_incid_prediction_rmse_heatmap,
                  Iterative_improvement_in_monthly_case_prediction_heatmap_mae = iterative_improvement_in_monthly_case_prediction_mae_heatmap,
                  Iterative_improvement_in_monthly_incid_prediction_heatmap_mae = iterative_improvement_in_monthly_incid_prediction_mae_heatmap,
                  Split_iterative_improvement_in_monthly_case_prediction_heatmap_rmse = Case_lagged_rmse_ratio_plots,
                  Split_iterative_improvement_in_monthly_incid_prediction_heatmap_rmse = Incid_lagged_rmse_ratio_plots,
                  Split_iterative_improvement_in_monthly_case_prediction_heatmap_mae = Case_lagged_mae_ratio_plots,
                  Split_iterative_improvement_in_monthly_incid_prediction_heatmap_mae = Incid_lagged_mae_ratio_plots)
  
  return(results)
  
}