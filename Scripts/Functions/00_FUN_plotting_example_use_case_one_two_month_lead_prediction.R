#' ---
#' title: "00_FUN_plotting_example_use_case_one_two_month_lead_prediction"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Visualise results from LOOCV of total seasonal actual vs predicted cases. 
#' 
#' Timeline:
#' =========
#' 20-06-2025: Initial commit. 


plotting_example_use_case_one_two_month_lead_prediction <- function(x, iso3_to_plot){
  
  #----- Filter input df for target countries
  x_desired_countries <- x %>%
    filter(iso3 %in% iso3_to_plot)
  
  #----- Filter for target season (most recent)
  x_desired_countries_target_season <- x_desired_countries %>% 
    ungroup() %>% 
    group_by(Country) %>% 
    mutate(season_end_year = as.numeric(str_sub(season, -4))) %>% 
    filter(season_end_year == max(season_end_year)) %>%
    mutate(Predicted_monthly_cases_lower_CI = Predicted_monthly_cases - CI95_Predicted_monthly_cases,
           Predicted_monthly_cases_upper_CI = Predicted_monthly_cases + CI95_Predicted_monthly_cases) %>%
    mutate(Predicted_monthly_cases_lower_CI = case_when(Predicted_monthly_cases_lower_CI < 0 ~ 0,
                                                        TRUE ~ Predicted_monthly_cases_lower_CI)) %>%
    mutate(Pred_lead = paste0(Pred_lead, " month lead"))
 
  #----- Plot target season
  lineplot_theme <- theme(plot.title = element_text(size = 12),
                          legend.title = element_text(size = 10),
                          legend.text = element_text(size = 8),
                          axis.title = element_text(size = 10),
                          axis.text = element_text(size = 8))   
  
  target_season_plot <- ggplot(x_desired_countries_target_season) + 
    geom_line(mapping = aes(x = Month_to_predict, y = Actual_monthly_cases, color = "Observed")) + 
    geom_line(mapping = aes(x = Month_to_predict, y = Predicted_monthly_cases, color = "Predicted")) + 
    geom_errorbar(mapping = aes(x = Month_to_predict, ymin = Predicted_monthly_cases_lower_CI, ymax = Predicted_monthly_cases_upper_CI), linetype = "dashed") + 
    scale_color_manual(values = c("Observed" = "red",
                                  "Predicted" = "blue")) +
    facet_wrap(Country ~ Pred_lead, scales = "free_y") +
    scale_x_continuous(breaks = seq(1:12)) + 
    labs(x = "Month", y = "Monthly cases", color = "") +
    ggtitle("Fig. 2: Sample use case of monthly proportion for prediction") +
    theme_minimal() +
    lineplot_theme

  return(target_season_plot)
 
}