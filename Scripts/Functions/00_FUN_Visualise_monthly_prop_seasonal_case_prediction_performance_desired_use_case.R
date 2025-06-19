#' ---
#' title: "00_FUN_Visualise_monthly_prop_seasonal_case_prediction_performance_desired_use_case"
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
#' 05-06-2025: Added code to plot incidence error.

Visualise_monthly_prop_seasonal_case_prediction_performance_desired_use_case <- function(x){
  
  # Define theme 
  plot_theme <- theme(plot.title = element_text(size = 12),
                      legend.title = element_text(size = 10),
                      legend.text = element_text(size = 9),
                      axis.text = element_text(size = 9),
                      axis.title = element_text(size = 10))
  
  #--------------- Identify peak month
  
  x_peak_month <- x %>% 
    ungroup() %>% 
    group_by(Country, season) %>% 
    slice_max(order_by = Cases_clean, n = 1) %>% 
    rename(Peak_month = season_nMonth)
  
  #- Calculate average peak month 
  circular_mean <- function(month_number){
    
    # Convert months to angles 
    angles <- (month_number - 1) * 30
    
    # Create a circular object
    circ_months <- circular(angles, units = "degrees", modulo = "2pi")
    
    # Calculate circular mean and standard deviation
    circ_mean <- mean(circ_months)
    
    # Convert the mean angle back to month number
    # as.numeric(circ_mean) gives the mean angle in degrees
    mean_month <- ((as.numeric(circ_mean) %% 360) / 30) + 1
    mean_month <- ifelse(mean_month > 12, mean_month - 12, mean_month)
    mean_month_rounded <- round(mean_month)
    
    mean_month_rounded <- ifelse(mean_month_rounded == 0, 12, mean_month_rounded)
    
    return(mean_month_rounded)
    
  }
  
  x_ave_peak_month <- x_peak_month %>% 
    ungroup() %>% 
    group_by(Country, season) %>%
    filter(sum(Cases_clean) > 0) %>%
    mutate(No_of_obs = n()) %>% 
    mutate(within_season_mean_peak_month = case_when(No_of_obs > 1 ~ circular_mean(Peak_month),
                                                     No_of_obs == 1  ~ Peak_month)) %>%
    select(Country, season, within_season_mean_peak_month) %>%
    distinct() %>%
    ungroup() %>% 
    group_by(Country) %>%
    summarise(Average_season_peak_month = circular_mean(within_season_mean_peak_month))

 #--------------- Add in peak month + calculate RMSE of total seasonal cases and incidence
 
  x_rmse_data <- x %>% 
    ungroup() %>% 
    group_by(Country, iso3, season_nMonth) %>% 
    summarise(Acc_vs_pred_total_cases_rmse = rmse(Total_season_cases, Predicted_seasonal_cases),
              Acc_vs_pred_total_incid_rmse = rmse(Actual_seasonal_total_cases_per_100000_pop, Predicted_seasonal_total_cases_per_100000_pop)) %>% 
    ungroup() %>% 
    filter(!is.na(Country)) %>%
    left_join(., x_ave_peak_month, by = "Country")
  
  x_rmse_data_cases_ordered <- x_rmse_data %>% 
    arrange(Acc_vs_pred_total_cases_rmse) %>%
    mutate(Country = fct_reorder(Country, Acc_vs_pred_total_cases_rmse, .fun = mean, .desc = FALSE, .na_rm = FALSE)) 
  
  x_rmse_data_incid_ordered <- x_rmse_data %>% 
    arrange(Acc_vs_pred_total_incid_rmse) %>%
    mutate(Country = fct_reorder(Country, Acc_vs_pred_total_incid_rmse, .fun = mean, .desc = FALSE, .na_rm = FALSE)) 
  
 #--------------- Plotting cases 
  
 # Scatterplot
 
 acc_vs_pred_season_total_cases_rmse_scatterplot <- ggplot(x_rmse_data_cases_ordered) + 
   geom_point(mapping = aes(x = season_nMonth, y = Acc_vs_pred_total_cases_rmse)) + 
   scale_x_continuous(breaks = 1:12) +
   geom_vline(mapping = aes(xintercept = Average_season_peak_month, linetype = "Peak month")) +
   scale_linetype_manual(values = c("Peak month" = "dashed")) +
   guides(linetype = guide_legend(title = NULL)) +
   theme_minimal() +
   plot_theme +
   facet_wrap(~ Country, scales = "free") +
   labs(x = "Season Month", y = "RMSE", linetype = "Peak month") +
   ggtitle("RMSE of actual vs predicted total seasonal cases") 
 
 # Heatmap
 pred_vs_acc_season_total_cases_rmse_plot <- ggplot(x_rmse_data_cases_ordered) + 
   geom_tile(mapping = aes(x = season_nMonth, y = Country, fill = Acc_vs_pred_total_cases_rmse)) + 
   scale_fill_gradient(low = "cyan", high = "magenta") +
   scale_x_continuous(breaks = 1:12) +
   labs(x = "", y = "Country", fill = "RMSE") +
   ggtitle("RMSE of actual vs predicted total seasonal cases") +
   theme_minimal() +
   plot_theme
 
 #--------------- Plotting incidence 
 
 # Scatterplot
 acc_vs_pred_season_total_incid_rmse_scatterplot <- ggplot(x_rmse_data_incid_ordered) + 
   geom_point(mapping = aes(x = season_nMonth, y = Acc_vs_pred_total_incid_rmse)) + 
   scale_x_continuous(breaks = 1:12) +
   geom_vline(mapping = aes(xintercept = Average_season_peak_month, linetype = "Peak month")) +
   scale_linetype_manual(values = c("Peak month" = "dashed")) +
   guides(linetype = guide_legend(title = NULL)) +
   theme_minimal() +
   plot_theme +
   facet_wrap(~ Country, scales = "free") +
   labs(x = "Season Month", y = "RMSE", linetype = "Peak month") +
   ggtitle("RMSE of actual vs predicted total seasonal incidence") 
 
 # Heatmap
 
 pred_vs_acc_season_total_incid_rmse_plot <- ggplot(x_rmse_data_incid_ordered) + 
   geom_tile(mapping = aes(x = season_nMonth, y = Country, fill = Acc_vs_pred_total_incid_rmse)) + 
   scale_fill_gradient(low = "cyan", high = "magenta") +
   scale_x_continuous(breaks = 1:12) +
   labs(x = "", y = "Country", fill = "RMSE") +
   ggtitle("RMSE of actual vs predicted total seasonal incidence") +
   theme_minimal() +
   plot_theme
 
 #--------------- Combining plots 
 
 # Cases 
 Combined_cases_plot <- plot_grid(pred_vs_acc_season_total_cases_rmse_plot, acc_vs_pred_season_total_cases_rmse_scatterplot, ncol = 2)
 
 # Incidence
 Combined_incidence_plot <- plot_grid(pred_vs_acc_season_total_incid_rmse_plot, acc_vs_pred_season_total_incid_rmse_scatterplot, ncol = 2)
 
 #--------------- Preparing results 
 
 results <- list(rmse_data = x_rmse_data,
                 rmse_cases_scatterplot = acc_vs_pred_season_total_cases_rmse_scatterplot,
                 rmse_cases_heatmap = pred_vs_acc_season_total_cases_rmse_plot,
                 rmse_incid_scatterplot = acc_vs_pred_season_total_incid_rmse_scatterplot,
                 rmse_incid_heatmap = pred_vs_acc_season_total_incid_rmse_plot,
                 Combined_cases_plot = Combined_cases_plot,
                 Combined_incidence_plot = Combined_incidence_plot)
 
 return(results)
 
}
  
