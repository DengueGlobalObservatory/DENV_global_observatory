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

Visualise_monthly_prop_iterative_monthly_incid_prediction_performance_desired_use_case_fun <- function(x){
  
  # Define theme 
  plot_theme <- theme(plot.title = element_text(size = 12),
                      legend.title = element_text(size = 10),
                      legend.text = element_text(size = 9),
                      axis.text = element_text(size = 9),
                      axis.title = element_text(size = 10))
  
  #--------------- Correlation of actual vs predicted monthly cases 
  
  #----- Heatmap
  
  x_cor_data <- x %>% 
    na.omit() %>% 
    ungroup() %>% 
    group_by(Country, iso3, season_nMonth, Month_to_predict) %>% 
    mutate(Acc_vs_pred_monthly_cor = cor(Actual_monthly_cases, Predicted_monthly_cases)) %>% 
    select(Country, iso3, season_nMonth, Month_to_predict, Acc_vs_pred_monthly_cor, mean_low_month) %>%
    distinct() %>%
    arrange(Acc_vs_pred_monthly_cor) 
  x_cor_data$Country = factor(x_cor_data$Country, levels = unique(x_cor_data$Country))
  
  acc_vs_pred_monthly_cases_cor_heatmap <- ggplot(x_cor_data) + 
    geom_tile(mapping = aes(x = Month_to_predict, y = season_nMonth, fill = Acc_vs_pred_monthly_cor)) + 
  #  geom_vline(data = x_cor_data, aes(xintercept = mean_low_month)) +
    scale_x_continuous(breaks = 1:12) +
    scale_y_continuous(breaks = 1:12) +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limits = c(-1, 1)) +
    theme_minimal() + 
    plot_theme +
    facet_wrap(~ Country, scales = "free") +
    labs(x = "Month to predict", y = "Season Month", fill = "Correlation") + 
    ggtitle("Comparing iterative predictions of monthly cases with observed cases (cor)")
  
  #--------------- RMSE between actual and predicted monthly cases 

  #----- Incid
  x_rmse_data_incid_ordered <- x %>% 
    na.omit() %>% 
    ungroup() %>% 
    group_by(Country, iso3, season_nMonth, Month_to_predict) %>% 
    mutate(Acc_vs_pred_monthly_incid_rmse = rmse(Actual_monthly_cases_per_100000_pop, Predicted_monthly_cases_per_100000_pop),
           Acc_vs_pred_monthly_cases_rmse = rmse(Actual_monthly_cases, Predicted_monthly_cases)) %>% 
    select(Country, iso3, season_nMonth, Month_to_predict, Acc_vs_pred_monthly_cases_rmse, Acc_vs_pred_monthly_incid_rmse, mean_low_month) %>% 
    distinct() %>%
    arrange(Acc_vs_pred_monthly_incid_rmse) 
  x_rmse_data_incid_ordered$Country = factor(x_rmse_data_incid_ordered$Country, levels = unique(x_rmse_data_incid_ordered$Country))
  
  acc_vs_pred_monthly_incid_rmse_heatmap <- ggplot(x_rmse_data_incid_ordered) + 
    geom_tile(mapping = aes(x = Month_to_predict, y = season_nMonth, fill = Acc_vs_pred_monthly_incid_rmse)) + 
    #    geom_vline(data = x_rmse_data, aes(xintercept = mean_low_month)) +
    scale_x_continuous(breaks = 1:12) +
    scale_y_continuous(breaks = 1:12) +
    theme_minimal() +
    plot_theme +
    facet_wrap(~ Country, scales = "free") +
    labs(x = "Month to predict", y = "Season Month", fill = "RMSE") + 
    ggtitle("Comparing iterative predictions of monthly cases with observed incidence (RMSE)")
  
  acc_vs_pred_monthly_incid_rmse_heatmap_minus_St_Barthelemy <-   x_rmse_data_incid_ordered %>% 
    filter(iso3 != "BLM") %>%
    ggplot(.) + 
    geom_tile(mapping = aes(x = Month_to_predict, y = season_nMonth, fill = Acc_vs_pred_monthly_incid_rmse)) + 
    #    geom_vline(data = x_rmse_data, aes(xintercept = mean_low_month)) +
    scale_x_continuous(breaks = 1:12) +
    scale_y_continuous(breaks = 1:12) +
    theme_minimal() +
    plot_theme +
    facet_wrap(~ Country, scales = "free") +
    labs(x = "Month to predict", y = "Season Month", fill = "RMSE") + 
    ggtitle("Comparing iterative predictions of monthly cases with observed incidence (RMSE)")
  
  
  #----- Cases
  x_rmse_data_cases_ordered <- ungroup(x_rmse_data_incid_ordered) %>% 
    arrange(Acc_vs_pred_monthly_cases_rmse) 
  x_rmse_data_cases_ordered$Country = factor(x_rmse_data_cases_ordered$Country, levels = unique(x_rmse_data_cases_ordered$Country))
  
  acc_vs_pred_monthly_cases_rmse_heatmap <- ggplot(x_rmse_data_cases_ordered) + 
    geom_tile(mapping = aes(x = Month_to_predict, y = season_nMonth, fill = Acc_vs_pred_monthly_cases_rmse)) + 
#    geom_vline(data = x_rmse_data, aes(xintercept = mean_low_month)) +
    scale_x_continuous(breaks = 1:12) +
    scale_y_continuous(breaks = 1:12) +
    theme_minimal() +
    plot_theme +
    facet_wrap(~ Country, scales = "free") +
    labs(x = "Month to predict", y = "Season Month", fill = "RMSE") + 
    ggtitle("Comparing iterative predictions of monthly cases with observed cases (RMSE)")
  
  acc_vs_pred_monthly_cases_rmse_heatmap_minus_Brazil <- x_rmse_data_cases_ordered %>% 
    filter(iso3 != "BRA") %>% 
    ggplot(.) + 
    geom_tile(mapping = aes(x = Month_to_predict, y = season_nMonth, fill = Acc_vs_pred_monthly_cases_rmse)) + 
    #    geom_vline(data = x_rmse_data, aes(xintercept = mean_low_month)) +
    scale_x_continuous(breaks = 1:12) +
    scale_y_continuous(breaks = 1:12) +
    theme_minimal() +
    plot_theme +
    facet_wrap(~ Country, scales = "free") +
    labs(x = "Month to predict", y = "Season Month", fill = "RMSE") + 
    ggtitle("Comparing iterative predictions of monthly cases with observed cases (RMSE)")
  
  #--------------- Preparing results 
  
  results <- list(cor_heatmap = acc_vs_pred_monthly_cases_cor_heatmap,
                  rmse_cases_heatmap = acc_vs_pred_monthly_cases_rmse_heatmap,
                  rmse_cases_heatmap_minus_brazil = acc_vs_pred_monthly_cases_rmse_heatmap_minus_Brazil, 
                  rmse_incid_heatmap = acc_vs_pred_monthly_incid_rmse_heatmap,
                  rmse_incid_heatmap_minus_st_barthelemy = acc_vs_pred_monthly_incid_rmse_heatmap_minus_St_Barthelemy)
  
  return(results)
  
}