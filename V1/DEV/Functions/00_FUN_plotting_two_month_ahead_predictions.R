#' ---
#' title: "00_FUN_plotting_two_month_ahead_predictions"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' 
#' Timeline:

plotting_two_month_ahead_predictions <- function(ave_seasonal_profiles, predictions_incid){
  
  #---------------------- Preparing data for plotting 
  
  #----- Predictions
  predictions_clean <- predictions_incid %>% 
    mutate(Calendar_year_month_abb = factor(month.abb[Calendar_year_month], levels = month.abb))
    
  #----- Average seasonal profiles 
  ave_seasonal_profiles <- ave_seasonal_profiles %>% 
    mutate(Calendar_year_month_abb = factor(month.abb[Calendar_year_month], levels = month.abb)) 
  
  #--- Scaling average monthly proportions to max cases observed in season (facilitates comparison between current year/ season and baseline)
  scaling_factor <- predictions_clean %>%
    ungroup() %>% 
    group_by(Country, iso3) %>% 
    slice_max(Predicted_monthly_cases) %>%
    select(Country, iso3, Predicted_monthly_cases) %>%
    rename(Scaling_factor = Predicted_monthly_cases)
  
  ave_seasonal_profiles_scaled <- ave_seasonal_profiles %>%
    distinct() %>%
    right_join(., scaling_factor, by = c("Country", "iso3")) %>%
    ungroup() %>%
    mutate(ave_monthly_prop_scaled =  Ave_monthly_proportion * Scaling_factor,
           CI95_monthly_proportion_lower_scaled = CI95_monthly_proportion_lower * Scaling_factor,
           CI95_monthly_proportion_upper_scaled = CI95_monthly_proportion_upper * Scaling_factor,
           leg_name = "Average seasonal profile") %>%
    arrange(Country, iso3, Calendar_year_month)
  
  #----- Plotting seasonal baseline, data to date and prediction 

  #----- Define theme 
  barplot_theme <-   theme(plot.margin = unit(c(0, 0, 0, 0), "lines"),
                           axis.text.y = element_text(size = 9),
                           legend.text = element_text(size = 9),
                           legend.title = element_text(size = 9)) 
  
  pred_observed_cols <- scale_color_manual(values = c("Observed" = "#047EB7",
                                                      "Predicted" = "#5e4b8b"))
  
  #----- Linear lineplot
  
  proportion_of_cases_by_month_and_region <- ggplot() + 
    # Plot predictions
    geom_line(predictions_clean, 
              mapping = aes(x = Calendar_year_month_abb, y = Predicted_monthly_cases, group = Country, color = Data_status)) +
    geom_errorbar(predictions_clean, 
                  mapping = aes(x = Calendar_year_month_abb, ymin = Predicted_monthly_cases_lower_CI95, ymax = Predicted_monthly_cases_upper_CI95), linetype = "dashed") + 
    pred_observed_cols +
    # Plot average seasonal profiles
    geom_ribbon(data = ave_seasonal_profiles_scaled, aes(x = Calendar_year_month_abb, ymin = CI95_monthly_proportion_lower_scaled, ymax = CI95_monthly_proportion_upper_scaled, 
                                                     group = 1), fill = "black", alpha = 0.2) +
    geom_line(ave_seasonal_profiles_scaled, mapping = aes(x = Calendar_year_month_abb, y = ave_monthly_prop_scaled, group = Country, color = leg_name, linetype = leg_name)) +
    scale_y_continuous(name = "Monthly cases",
                       sec.axis = sec_axis(~ . / unique(scaling_factor$Scaling_factor), "Average seasonal profile")) +
    scale_linetype_manual(values = c("Observed" = "solid", "Predicted" = "solid", "Average seasonal profile" = "dashed")) +
    labs(x = "Month", col = "Data status", linetype = NULL) +
    theme_minimal() + 
    barplot_theme
  
  #----- Radial lineplot
  
  proportion_of_cases_by_month_and_region_radial <- ggplot() + 
    geom_line(predictions_clean, 
              mapping = aes(x = Calendar_year_month_abb, y = Predicted_monthly_cases, group = Country, color = Data_status)) +
    pred_observed_cols +
    geom_ribbon(data = ave_seasonal_profiles_scaled, aes(x = Calendar_year_month_abb, ymin = CI95_monthly_proportion_lower_scaled, ymax = CI95_monthly_proportion_upper_scaled, 
                                                         group = Country), fill = "black", alpha = 0.2) +
    geom_line(ave_seasonal_profiles_scaled, mapping = aes(x = Calendar_year_month_abb, y = ave_monthly_prop_scaled, group = Country, color = leg_name, linetype = leg_name)) +
    scale_y_continuous(name = "Monthly cases",
                       sec.axis = sec_axis(~ . / unique(scaling_factor$Scaling_factor), "Average seasonal profile")) +
    scale_linetype_manual(values = c("Observed" = "solid", "Predicted" = "solid", "Average seasonal profile" = "dashed")) +
    labs(x = "Month", col = "Data status", linetype = NULL) +
    coord_polar(theta = "x") +
    theme_minimal() + 
    barplot_theme
  
  #---------------------- Return results
  
  results <- list(linear_lineplot = proportion_of_cases_by_month_and_region,
                  radial_lineplot = proportion_of_cases_by_month_and_region_radial)
  
  return(results)
  
}