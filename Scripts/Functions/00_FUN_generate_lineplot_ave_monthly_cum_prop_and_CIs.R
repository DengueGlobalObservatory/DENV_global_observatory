#' ---
#' title: "00_FUN_generate_lineplot_ave_monthly_cum_prop_and_CIs"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Generate a lineplot showing the average monthly cumulative proportion of cases and the 95% CIs. Facet by country. 
#' 
#' Timeline:
#' =========
#' 20-06-2025: Initial commit. 

generate_lineplot_ave_monthly_cum_prop_and_CIs <- function(x){
  
  #----- Prepare data 
  x_monthly_prop <- x %>% 
    ungroup() %>% 
    group_by(Country, season) %>%
    mutate(Monthly_proportion_of_cases = Cases_clean / sum(Cases_clean)) %>% 
    arrange(season_nMonth) %>%
    mutate(Cum_monthly_proportion_of_cases = cumsum(Monthly_proportion_of_cases))
  
  x_ave_monthly_prop <- x_monthly_prop %>% 
    ungroup() %>% 
    group_by(Country, season_nMonth) %>%
    mutate(Average_cum_monthly_proportion_of_cases = mean(Cum_monthly_proportion_of_cases),
           SD_cum_monthly_proportion_of_cases = sd(Cum_monthly_proportion_of_cases)) %>% 
    ungroup() %>% 
    mutate(CI95 = 1.96 * SD_cum_monthly_proportion_of_cases) %>%
    mutate(lower_CI95 = Average_cum_monthly_proportion_of_cases - CI95, 
           upper_CI95 = Average_cum_monthly_proportion_of_cases + CI95) %>%
    mutate(lower_CI95 = case_when(lower_CI95 < 0 ~ 0,
                                  TRUE ~ lower_CI95),
           upper_CI95 = case_when(upper_CI95 > 1 ~ 1,
                                  TRUE ~ upper_CI95))
    
  #----- Plotting
  
  lineplot_theme <- theme(plot.title = element_text(size = 12),
                          legend.title = element_text(size = 10),
                          legend.text = element_text(size = 8),
                          axis.title = element_text(size = 10),
                          axis.text = element_text(size = 8))   
  
  lineplot <- ggplot(x_ave_monthly_prop) + 
    geom_line(mapping = aes(x = season_nMonth, y = Average_cum_monthly_proportion_of_cases)) + 
    geom_errorbar(mapping = aes(x = season_nMonth, 
                                ymin = lower_CI95, ymax = upper_CI95), width = 0.2, linetype = "dashed") + 
    scale_x_continuous(breaks = 1:12) +
    labs(x = "Season month", y = "Cumulative proportion of cases") +
    ggtitle("Average cumulative monthly proportion of cases observed within a dengue season") +
    facet_wrap(~ Country) + 
    theme_minimal() + 
    lineplot_theme
  
  return(lineplot)
}