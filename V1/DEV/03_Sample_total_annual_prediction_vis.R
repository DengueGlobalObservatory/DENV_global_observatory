#' ---
#' title: "03_Sample_total_annual_prediction_vis"
#' author: "K Joshi"
#' 
#' ---
#'
#' Overview:
#' ========
#' Generate sample plot showing how monthly proportion forecasting method works. 
#' 
#' Timeline:
#' ========
#' 11-09-2025: Initial commit.              

library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(cowplot)
library(countrycode)

#--------------- Load average seasonal profiles - CHANGE TO FIT INTO PIPELINE SCRIPT
#' Load the most up to date version of the national ave seasonal profiles. 
avg_season_dirs <- list.dirs("V1/DEV/02_identify_seasonal_baseline/", recursive = FALSE, full.names = FALSE)  
avg_season_dirs_dates <- as.Date(avg_season_dirs, format = "%Y-%m-%d")

if (length(avg_season_dirs) == 1) {
  target_data <- avg_season_dirs_dates
} else if (length(avg_season_dirs) > 1) {
  target_data <- avg_season_dirs_dates[which.max(avg_season_dirs_dates)]
} else if (length(avg_season_dirs) == 0) {
  stop("No data in national data directory.")
}

avg_season <- read_csv(paste0("V1/DEV/02_identify_seasonal_baseline/", target_data , "/National_average_seasonal_profile.csv"))
full_data_interpolated <- read_csv("V1/DEV/02_Combining_WHO_and_OpenDengue_data/2025-09-08/National_clean_data.csv") # HARD CODED - change to load most recently available data...

#--------------- Load WHO data and clean
WHO_data <- read_excel("Data/WHO/dengue-global-data-2025-09-08.xlsx") # CHANGE TO LOAD MOST UP TO DATE DATA 

# Filter for 2025 and filter for countries with ave seasonal profiles available. 
WHO_data_2025 <- WHO_data %>% 
  filter(
    year(date) == 2025,
    iso3 %in% avg_season$iso3
  ) %>%
  mutate(
    Month = month(date)
  )

#--------------- Fill missing data to date + project case counts for rest of year 

monthly_data_complete <- avg_season %>% 
  ungroup() %>% 
  
  # Join average season to year to fill missing data to date. 
  left_join(
    .,
    WHO_data_2025, 
    join_by("Calendar_year_month" == Month, "iso3")
  ) %>% 
  distinct() %>%
  arrange(Country, 
          iso3, 
          Calendar_year_month) %>%
  
  # Assign var based on whether cases are observed 
  group_by(Country, iso3) %>% 
  mutate(
    Observed_cum_cases = cumsum(cases),
    Data_status = case_when(is.na(cases) ~ "Predicted",
                            !is.na(cases) ~ "Observed")
  ) %>%
  
  # Identify most recent month with observations by country
  group_by(Country, 
           iso3, 
           Data_status) %>%
  mutate(most_recent_month = case_when(
    Calendar_year_month == max(Calendar_year_month) & !is.na(cases) ~ "Most_recent",
    Calendar_year_month != max(Calendar_year_month) & !is.na(cases) ~ "Not_most_recent",
    is.na(cases) ~ "No_data")
  ) %>%
  
  # Calculate predicted total seasonal cases using cases observed to date 
  mutate(
    Predicted_total_seasonal_cases = 
      case_when(most_recent_month == "Most_recent" ~ Observed_cum_cases / Ave_cum_monthly_proportion,
                most_recent_month == "Not_most_recent" ~ NA,
                most_recent_month == "No_data" ~ NA)
  ) %>%
  ungroup() %>% 
  group_by(Country, iso3) %>% 
  
  # Is there any data available for that season?
  mutate(
    Any_data_available = 
      case_when(
        length(unique(na.omit(Predicted_total_seasonal_cases))) == 0 ~ "No_data_available",
        length(unique(na.omit(Predicted_total_seasonal_cases))) > 0 ~ "Data_available"), 
    
    # Fill predicted total seasonal case col, where no data available enter NA.
    Predicted_total_seasonal_cases = 
      case_when(Any_data_available == "No_data_available" ~ Predicted_total_seasonal_cases,
                Any_data_available == "Data_available" ~ first(na.omit(Predicted_total_seasonal_cases))
      ),
    
    # Generate predictions for rest of year   
    complete_cases = case_when(!is.na(cases) ~ cases,
                               is.na(cases) ~ round(Predicted_total_seasonal_cases * Ave_monthly_proportion)),
    complete_cum_cases = cumsum(complete_cases)
    ) %>% 
  
  # Remove entries from predicted data col to only be two months ahead 
  dplyr::select(
    # Identifiers
    Country, iso3, Calendar_year_month, season_nMonth, 
    
    # Observed and complete cases + whether cases are observed or predicted, 
    cases, complete_cases, Data_status, 
    
    # Cumulative cases to date 
    Observed_cum_cases, complete_cum_cases,
    
    # Average season 
    Ave_season_monthly_cases, Ave_season_monthly_cum_cases, Ave_monthly_proportion, Ave_cum_monthly_proportion,
    
    # Most recent month 
    most_recent_month
  ) %>%
  ungroup()

#--------------- Visualise data 

generate_sample_total_annual_prediction_vis <- function(monthly_data, 
                                                        target_country_iso3){
  
  # Filter monthly predictions for target country
  monthly_data_filtered <- monthly_data %>% 
    filter(
      iso3 %in% target_country_iso3
    ) 
  
  if(nrow(monthly_data_filtered) == 0){
    stop("No data available after filtering monthly_data for target country")
  }

  # Add in observation to make space spanning last obs and first pred point red
  monthly_data_filtered <- monthly_data_filtered %>% 
    filter(most_recent_month == "Most_recent") %>% 
    mutate(Data_status = "Predicted") %>% 
    rbind(., monthly_data_filtered)
  
  # Assign plot theme 
  plot_theme <- theme(plot.title = element_text(size = 15),
                      legend.title = element_text(size = 15),
                      legend.text = element_text(size = 13),
                      axis.title = element_text(size = 13),
                      axis.text.y = element_text(size = 10),
                      axis.text.x = element_text(size = 10,
                        angle = 90, vjust = 0.5, hjust = 1)
                      )  
  
  # Assign plot colours 
  plot_col_theme <- scale_color_manual(
    values = c("Observed" = "blue",
               "Predicted" = "red",
               "Average season" = "black"),
    name = NULL
  )
  
  # Identify max plot limit for predicted season 
  y_limits <- range(monthly_data_filtered$complete_cum_cases, na.rm = TRUE)
  
  # Plot current season  
  current_season <- ggplot(monthly_data_filtered) + 
    geom_line(
      mapping = aes(x = Calendar_year_month, 
                    y = Observed_cum_cases, 
                    color = "Observed"),
      size = 1.01
      ) +  
    plot_col_theme +
    coord_cartesian(ylim = y_limits) +
    labs(x = "", 
         y = "Cases",
         title = "Current") + 
    scale_x_continuous(breaks = seq(1:12),
                       labels = month.abb[]) +
    guides(color = "none") + 
    theme_minimal() +
    plot_theme 

  # Plot average season
  avg_season <- ggplot(monthly_data_filtered) + 
    geom_line(
      mapping = aes(x = Calendar_year_month, 
                    y = Ave_cum_monthly_proportion, 
                    color = "Average season"),
      size = 1.01
    ) +  
    labs(x = "", 
         y = "Average cumulative proportion of cases", 
         title = "Average") + 
    scale_x_continuous(breaks = seq(1:12),
                       labels = month.abb[]) +
    guides(color = "none") + 
    plot_col_theme +
    theme_minimal() +
    plot_theme 

  # Plot predictions and average season 
  pred_season <- ggplot(monthly_data_filtered) + 
    geom_line(
      mapping = aes(x = Calendar_year_month, 
                    y = complete_cum_cases, 
                    color = Data_status),
      size = 1.01
    ) +  
    labs(x = "", 
         y = "Cases",
         title = "Predicted") + 
    scale_x_continuous(breaks = seq(1:12),
                       labels = month.abb[]) +
    plot_col_theme + 
    theme_minimal() +
    plot_theme 
  
  # Define spacer for between plots 
  spacer <- ggplot() + theme_void()
  spacer_width <- 0.1
  
  # Combine plots 
  combined_plot <- wrap_plots(current_season, 
                              spacer,
                              avg_season, 
                              spacer,
                              pred_season,
                              widths = c(1, spacer_width, 1, spacer_width, 1),
                              nrow = 1) 
  
  combined_plot_final <- ggdraw(
    combined_plot
    ) + 
    
    # Add header
    draw_label(
      paste0(countrycode(target_country_iso3, "iso3c", "country.name"), " dengue season"),
      x = 0.01, hjust = 0, 
      y = 1.03, vjust = 1,
      size = 18
      ) + 
    
    # Extend top of plot 
    theme(plot.margin = margin(t = 30))

  return(combined_plot_final)  

}

sample_total_annual_case_prediction <- generate_sample_total_annual_prediction_vis(monthly_data_complete,
                                                                                   "THA")
sample_total_annual_case_prediction

#--------------- Saving 
dir_to_save <- paste0("V1/DEV/03_Sample_total_annual_prediction_vis/", Sys.Date())
dir.create(dir_to_save)

# Save sample visualisation  
ggsave(sample_total_annual_case_prediction,
       filename = paste0(dir_to_save, "/THA_sample_total_annual_case_pred.png"),
       device = "png",
       width = 45,
       height = 20,
       unit = "cm",
       dpi = 300)

