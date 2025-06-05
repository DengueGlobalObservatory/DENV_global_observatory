#' ---
#' title: "00_FUN_Visualise_monthly_prop_iterative_monthly_incid_prediction_performance_desired_use_case"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Calculate rmse of actual and predicted monthly cases by country, and generate plots.
#' 
#' Timeline:
#' =========
#' 22-05-2025: Prepared script.
#' 05-06-2025: Removed correlation code. Added code to calculate MAE as an alternative metric. 
#'             Split heatmaps by max MAE/ RMSE value to increase interpretanility + spread out colour scale. 

Visualise_monthly_prop_iterative_monthly_incid_prediction_performance_desired_use_case_fun <- function(x){
  
  # Define theme 
  plot_theme <- theme(plot.title = element_text(size = 12),
                      legend.title = element_text(size = 10),
                      legend.text = element_text(size = 9),
                      axis.text = element_text(size = 9),
                      axis.title = element_text(size = 10))
  
  #--------------- Calculate RMSE and MAE between actual and predicted monthly cases 
  x_error_data <- x %>% 
    na.omit() %>% 
    ungroup() %>% 
    group_by(Country, iso3, season_nMonth, Month_to_predict) %>% 
    mutate(Acc_vs_pred_monthly_incid_rmse = rmse(Actual_monthly_cases_per_100000_pop, Predicted_monthly_cases_per_100000_pop),
           Acc_vs_pred_monthly_cases_rmse = rmse(Actual_monthly_cases, Predicted_monthly_cases),
           Acc_vs_pred_monthly_incid_mae = mae(Actual_monthly_cases_per_100000_pop, Predicted_monthly_cases_per_100000_pop),
           Acc_vs_pred_monthly_cases_mae = mae(Actual_monthly_cases, Predicted_monthly_cases)) %>% 
    select(Country, iso3, season_nMonth, Month_to_predict, 
           Acc_vs_pred_monthly_cases_rmse, Acc_vs_pred_monthly_incid_rmse, Acc_vs_pred_monthly_cases_mae, Acc_vs_pred_monthly_incid_mae, 
           mean_low_month, Average_season_peak_month) %>% 
    distinct() %>%
    arrange(Acc_vs_pred_monthly_incid_rmse) %>%
    group_by(Country) %>% 
    mutate(Max_acc_vs_pred_monthly_cases_rmse = max(Acc_vs_pred_monthly_cases_rmse),
           Max_acc_vs_pred_monthly_incid_rmse = max(Acc_vs_pred_monthly_incid_rmse),
           Max_acc_vs_pred_monthly_cases_mae = max(Acc_vs_pred_monthly_cases_mae),
           Max_acc_vs_pred_monthly_incid_mae = max(Acc_vs_pred_monthly_incid_mae)) %>%
    mutate(Cases_rmse_groups = cut(Max_acc_vs_pred_monthly_cases_rmse, breaks = c(0, 2000, 5000, 20000, 90000, 1000000), labels = c(1, 2, 3, 4, 5), include.lowest = TRUE),
           Incid_rmse_groups = cut(Max_acc_vs_pred_monthly_incid_rmse, breaks = c(0, 100, 500, 2000, Inf), labels = c(1, 2, 3, 4), include.lowest = TRUE),
           Cases_mae_groups = cut(Max_acc_vs_pred_monthly_cases_mae, breaks = c(0, 5000, 10000, 50000, Inf), labels = c(1, 2, 3, 4), include.lowest = TRUE),
           Incid_mae_groups = cut(Max_acc_vs_pred_monthly_incid_mae, breaks = c(0, 100, 500, 1000, Inf), labels = c(1, 2, 3, 4), include.lowest = TRUE)) %>%
    ungroup() 
  
  #----- Ordering by RMSE
  x_error_data_rmse_cases_ordered <- x_error_data %>% 
    group_by(Cases_rmse_groups) %>%
    mutate(Country = fct_reorder(Country, Acc_vs_pred_monthly_cases_rmse, .fun = mean, .desc = FALSE, .na_rm = FALSE))
  
  x_error_data_rmse_incid_ordered <- x_error_data %>% 
    group_by(Incid_rmse_groups) %>%
    mutate(Country = fct_reorder(Country, Acc_vs_pred_monthly_incid_rmse, .fun = mean, .desc = FALSE, .na_rm = FALSE))
  
  #----- Ordering by MAE
  x_error_data_mae_cases_ordered <- x_error_data %>% 
    group_by(Cases_mae_groups) %>%
    mutate(Country = fct_reorder(Country, Acc_vs_pred_monthly_cases_mae, .fun = mean, .desc = FALSE, .na_rm = FALSE))
  
  x_error_data_mae_incid_ordered <- x_error_data %>% 
    group_by(Incid_mae_groups) %>%
    mutate(Country = fct_reorder(Country, Acc_vs_pred_monthly_incid_mae, .fun = mean, .desc = FALSE, .na_rm = FALSE))
  
  #--------------- Plotting all country heatmaps 
  
  #----- Cases RMSE ordered 
  acc_vs_pred_monthly_cases_rmse_heatmap <- x_error_data %>% 
    mutate(Country = fct_reorder(Country, Acc_vs_pred_monthly_cases_rmse, .fun = mean, .desc = FALSE, .na_rm = FALSE)) %>% 
    ggplot(.) +
    geom_tile(aes(x = Month_to_predict, y = season_nMonth, fill = Acc_vs_pred_monthly_cases_rmse)) +
    geom_vline(mapping = aes(xintercept = Average_season_peak_month, linetype = "Peak month")) +
    scale_x_continuous(breaks = 1:12) +
    scale_y_continuous(breaks = 1:12) +
    facet_wrap(~ Country, scales = "free") +
    scale_linetype_manual(values = c("Peak month" = "dashed")) +
    guides(linetype = guide_legend(title = NULL)) +
    scale_fill_gradient(low = "cyan", high = "magenta") +
    theme_minimal() +
    plot_theme +
    labs(x = "Month to predict", y = "Season Month", fill = "RMSE", linetype = "Peak month") +
    ggtitle("RMSE of predicted monthly cases")
  
  #----- Incid RMSE ordered 
  acc_vs_pred_monthly_incid_rmse_heatmap <- x_error_data %>% 
    mutate(Country = fct_reorder(Country, Acc_vs_pred_monthly_incid_rmse, .fun = mean, .desc = FALSE, .na_rm = FALSE)) %>% 
    ggplot(.) +
    geom_tile(aes(x = Month_to_predict, y = season_nMonth, fill = Acc_vs_pred_monthly_incid_rmse)) +
    geom_vline(mapping = aes(xintercept = Average_season_peak_month, linetype = "Peak month")) +
    scale_x_continuous(breaks = 1:12) +
    scale_y_continuous(breaks = 1:12) +
    facet_wrap(~ Country, scales = "free") +
    scale_linetype_manual(values = c("Peak month" = "dashed")) +
    guides(linetype = guide_legend(title = NULL)) + 
    scale_fill_gradient(low = "cyan", high = "magenta") +
    theme_minimal() +
    plot_theme +
    labs(x = "Month to predict", y = "Season Month", fill = "RMSE", linetype = "Peak\nMonth") +
    ggtitle("RMSE of predicted monthly incidence")
  
  #----- Cases MAE ordered 
  acc_vs_pred_monthly_cases_mae_heatmap <- x_error_data %>% 
    mutate(Country = fct_reorder(Country, Acc_vs_pred_monthly_cases_mae, .fun = mean, .desc = FALSE, .na_rm = FALSE)) %>% 
    ggplot(.) +
    geom_tile(aes(x = Month_to_predict, y = season_nMonth, fill = Acc_vs_pred_monthly_cases_mae)) +
    geom_vline(mapping = aes(xintercept = Average_season_peak_month, linetype = "Peak month")) +
    scale_x_continuous(breaks = 1:12) +
    scale_y_continuous(breaks = 1:12) +
    scale_linetype_manual(values = c("Peak month" = "dashed")) +
    guides(linetype = guide_legend(title = NULL)) +
    facet_wrap(~ Country, scales = "free") +
    scale_fill_gradient(low = "cyan", high = "magenta") +
    theme_minimal() +
    plot_theme +
    labs(x = "Month to predict", y = "Season Month", fill = "MAE", linetype = "Peak\nMonth") +
    ggtitle("MAE of predicted monthly cases")
  
  #----- Incid MAE ordered
  acc_vs_pred_monthly_incid_mae_heatmap <- x_error_data %>% 
    mutate(Country = fct_reorder(Country, Acc_vs_pred_monthly_incid_mae, .fun = mean, .desc = FALSE, .na_rm = FALSE)) %>% 
    ggplot(.) +
    geom_tile(aes(x = Month_to_predict, y = season_nMonth, fill = Acc_vs_pred_monthly_incid_mae)) +
    geom_vline(mapping = aes(xintercept = Average_season_peak_month, linetype = "Peak month")) +
    scale_x_continuous(breaks = 1:12) +
    scale_y_continuous(breaks = 1:12) +
    scale_linetype_manual(values = c("Peak month" = "dashed")) +
    guides(linetype = guide_legend(title = NULL)) +
    facet_wrap(~ Country, scales = "free") +
    scale_fill_gradient(low = "cyan", high = "magenta") +
    theme_minimal() +
    plot_theme +
    labs(x = "Month to predict", y = "Season Month", fill = "MAE", linetype = "Peak month") +
    ggtitle("MAE of predicted monthly incidence")
  
  #--------------- Plotting country heatmaps split into bands
  
  #----- Cases RMSE ordered
  Case_rmse_plots <- list()
  for (group in unique(x_error_data_rmse_cases_ordered$Cases_rmse_groups)){
    df_subset <- x_error_data_rmse_cases_ordered %>% filter(Cases_rmse_groups == group)
    p <- ggplot(df_subset) +
      geom_tile(aes(x = Month_to_predict, y = season_nMonth, fill = Acc_vs_pred_monthly_cases_rmse)) +
      geom_vline(mapping = aes(xintercept = Average_season_peak_month, linetype = "Peak month")) + 
      scale_x_continuous(breaks = 1:12) +
      scale_y_continuous(breaks = 1:12) +
      scale_linetype_manual(values = c("Peak month" = "dashed")) +
      guides(linetype = guide_legend(title = NULL)) +
      facet_wrap(~ Country, scales = "free") +
      scale_fill_gradient(low = "cyan", high = "magenta") +
      theme_minimal() +
      plot_theme +
      labs(x = "Month to predict", y = "Season Month", fill = "RMSE", linetype = "Peak\nMonth") +
      ggtitle("RMSE of predicted monthly cases")
    Case_rmse_plots[[paste0("Group", group)]] <- p
  }
  
  #----- Incidence RMSE ordered 
  Incid_rmse_plots <- list()
  for (group in unique(x_error_data_rmse_incid_ordered$Incid_rmse_groups)){
    df_subset <- x_error_data_rmse_incid_ordered %>% filter(Incid_rmse_groups == group)
    p <- ggplot(df_subset) +
      geom_tile(aes(x = Month_to_predict, y = season_nMonth, fill = Acc_vs_pred_monthly_incid_rmse)) +
      geom_vline(mapping = aes(xintercept = Average_season_peak_month, linetype = "Peak month")) +
      scale_x_continuous(breaks = 1:12) +
      scale_y_continuous(breaks = 1:12) +
      scale_linetype_manual(values = c("Peak month" = "dashed")) +
      guides(linetype = guide_legend(title = NULL)) +
      facet_wrap(~ Country, scales = "free") +
      scale_fill_gradient(low = "cyan", high = "magenta") +
      theme_minimal() +
      plot_theme +
      labs(x = "Month to predict", y = "Season Month", fill = "RMSE", linetype = "Peak\nMonth") + 
      ggtitle("RMSE of iterative predictions of monthly incidence")
    Incid_rmse_plots[[paste0("Group", group)]] <- p
  }
  
  #----- Cases MAE ordered 
  Case_mae_plots <- list()
  for (group in unique(x_error_data_mae_cases_ordered$Cases_mae_groups)){
    df_subset <- x_error_data_mae_cases_ordered %>% filter(Cases_mae_groups == group)
    p <- ggplot(df_subset) +
      geom_tile(aes(x = Month_to_predict, y = season_nMonth, fill = Acc_vs_pred_monthly_cases_mae)) +
      geom_vline(mapping = aes(xintercept = Average_season_peak_month, linetype = "Peak month")) +
      scale_x_continuous(breaks = 1:12) +
      scale_y_continuous(breaks = 1:12) +
      scale_linetype_manual(values = c("Peak month" = "dashed")) +
      guides(linetype = guide_legend(title = NULL)) +
      facet_wrap(~ Country, scales = "free") +
      scale_fill_gradient(low = "cyan", high = "magenta") +
      theme_minimal() +
      plot_theme +
      labs(x = "Month to predict", y = "Season Month", fill = "MAE", linetype = "Peak\nMonth") + 
      ggtitle("MAE of iterative predictions of monthly cases")
    Case_mae_plots[[paste0("Group", group)]] <- p
  }
  
  #----- Incidence MAE ordered
  Incid_mae_plots <- list()
  for (group in unique(x_error_data_mae_incid_ordered$Incid_mae_groups)){
    df_subset <- x_error_data_mae_incid_ordered %>% filter(Incid_mae_groups == group)
    p <- ggplot(df_subset) +
      geom_tile(aes(x = Month_to_predict, y = season_nMonth, fill = Acc_vs_pred_monthly_incid_mae)) +
      geom_vline(mapping = aes(xintercept = Average_season_peak_month, linetype = "Peak month")) +
      scale_x_continuous(breaks = 1:12) +
      scale_y_continuous(breaks = 1:12) +
      scale_linetype_manual(values = c("Peak month" = "dashed")) +
      guides(linetype = guide_legend(title = NULL)) +
      facet_wrap(~ Country, scales = "free") +
      scale_fill_gradient(low = "cyan", high = "magenta") +
      theme_minimal() +
      plot_theme +
      labs(x = "Month to predict", y = "Season Month", fill = "MAE") + 
      ggtitle("MAE of iterative predictions of monthly incidence")
    Incid_mae_plots[[paste0("Group", group)]] <- p
  }
  
  
  #--------------- Preparing results 
  
  results <- list(Plot_data = x_error_data,
                  rmse_cases_heatmap = acc_vs_pred_monthly_cases_rmse_heatmap,
                  rmse_incid_heatmap = acc_vs_pred_monthly_incid_rmse_heatmap,
                  mae_cases_heatmap = acc_vs_pred_monthly_cases_mae_heatmap,
                  mae_incid_heatmap = acc_vs_pred_monthly_incid_mae_heatmap,
                  split_rmse_cases_heatmap = Case_rmse_plots,
                  split_rmse_incid_heatmap = Incid_rmse_plots,
                  split_mae_cases_heatmap = Case_mae_plots,
                  split_mae_incid_heatmap = Incid_mae_plots)
  
  return(results)
  
}