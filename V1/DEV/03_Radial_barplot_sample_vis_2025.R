#' ---
#' title: "03_Radial_barplot_sample_vis_2025"
#' author: "K Joshi"
#' 
#' ---
#'
#' Overview:
#' ========
#' Generate sample radial barplots with SD based traffic light system for 2025 predictions. 
#' 
#' Timeline:
#' ========
#' 08-09-2025: Initial commit.
#' 09-09-2025: Changed plot from traffic light system to radial lineplots. 
#'             Changed from forecasting two months ahead to using proportions to fill missing data to date. 
#'             Added code to colour season by severity above baseline. 
#' 10-09-2025: Added code to compare fit distribution with empirical distribution.            

library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(scales)
library(countrycode)
library(purrr)

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
WHO_data <- read_excel("Data/WHO_dengue_data/dengue-global-data-2025-09-08.xlsx") # CHANGE TO LOAD MOST UP TO DATE DATA 

# Filter for 2025 and filter for countries with ave seasonal profiles available. 
WHO_data_2025 <- WHO_data %>% 
  filter(
    year(date) == 2025,
    iso3 %in% avg_season$iso3
    ) %>%
  mutate(
    Month = month(date)
    )

#--------------- Fill missing data to date + identify percentile of most recent month cum cases

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
  
  # Define previous month as last period requiring data filling.  
  mutate(End_prediction_month =  month(Sys.Date()) - 1) %>%

  # Assign var based on whether cases are observed 
  group_by(Country, iso3) %>% 
  mutate(
    Observed_cum_cases = cumsum(cases),
    Data_status = case_when(is.na(cases) ~ "Unobserved",
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
    
    # Where data is missing fill using predictions 
    complete_cases = case_when(!is.na(cases) ~ cases,
                               is.na(cases) & Calendar_year_month <= End_prediction_month ~ round(Predicted_total_seasonal_cases * Ave_monthly_proportion),
                               Calendar_year_month > End_prediction_month ~ NA),
    complete_cum_cases = cumsum(complete_cases),
    
    # Identify percentile of most recent month cum cases within negbin dist
    percentile_most_recent = pnbinom(q = complete_cum_cases, 
                                     size = nb_size, 
                                     mu = nb_mean) * 100
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
    Ave_season_monthly_cases, Ave_season_monthly_cum_cases, 
    
    # Negative binomial distribution parameters 
    nb_mean, nb_size,
    
    # Percentile position of the observed cases to date within negbin dist 
    percentile_most_recent
    ) %>%
  ungroup()
  
#--------------- Compare empirical and fit distribution  

compare_empirical_and_fit_dist <- function(monthly_data_complete,
                                           full_data_interpolated,
                                           target_iso3){
  
  # Filter data for target country and target month (previous complete month)
  previous_month <- month(Sys.Date()) - 1
  
  full_data_filtered <- full_data_interpolated %>% 
    filter(iso3 == target_iso3 & 
             Month == previous_month) 
  
  if(nrow(full_data_filtered) == 0){
    stop("No data in full_data_interpolated following filtering for target country.")
  }
  
  monthly_data_filtered <- monthly_data_complete %>% 
    filter(iso3 == target_iso3,
           Calendar_year_month == previous_month)
  
  if(nrow(monthly_data_filtered) == 0){
    stop("No data in monthly_data_complete following filtering for target country.")
  }
  
  # Simulate neg bin dist of cumulative cases observed in month X in previous years 
  nb_dist_sim <- data.frame(samples = 
                              rnbinom(n = 1000,
                                      size = unique(monthly_data_filtered$nb_size),
                                      mu = unique(monthly_data_filtered$nb_mean)
                                      )
                            )
  
  # Assign plot theme 
  plot_theme <- theme(
    plot.title = element_text(size = 12),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10)
    )  
  
  # Generate histogram of previous years cum cases and overlay sim dist 
  comparison_plot <- ggplot() + 
    
    # Histogram of previous years 
    geom_histogram(
      data = full_data_filtered,
      mapping = aes(x = Cases_clean, y = after_stat(density), fill = "Empirical"),
      alpha = 0.4) + 
    
    # Overlay simulated distribution 
    geom_density(
      data = nb_dist_sim,
      mapping = aes(x = samples, fill = "Simulated"),
      alpha = 0.4) + 
    
    # Plot settings 
    scale_fill_manual(
      values = c("Empirical" = "red",
                 "Simulated" = "blue")
    ) + 
    labs(
      x = "Cases", 
      y = "Density",
      title = countrycode(target_iso3, "iso3c", "country.name")
      ) +
    theme_minimal() + 
    plot_theme
  
  return(comparison_plot)
}

compare_empirical_and_fit_dist_safe <- safely(compare_empirical_and_fit_dist, 
                                              otherwise = NULL)

target_iso3 <- c("BRA", "THA", "COL", "PRY", "AFG")

dist_fit_plots <- map(
  target_iso3,
  ~ compare_empirical_and_fit_dist_safe(
    monthly_data_complete,
    full_data_interpolated,
    target_iso3 = .x
  )
)
names(dist_fit_plots) <- target_iso3

#--------------- Visualise data 

generate_target_season_plot <- function(monthly_data, 
                                        full_data_interpolated, 
                                        target_country_iso3, radial){
  
  # Filter monthly predictions for target country
  monthly_data_filtered <- monthly_data %>% 
    filter(iso3 %in% target_country_iso3)
  
  if(nrow(monthly_data_filtered) == 0){
    stop("No data available after filtering")
  }
  
  # Filter full data for previous year for target country 
  previous_year <- year(Sys.Date()) - 1
  previous_year_data <- full_data_interpolated %>% 
    filter(
      iso3 %in% target_country_iso3 & 
        Year %in% previous_year
    )
  
  # Assign plot theme 
  plot_theme <- theme(plot.title = element_text(size = 12),
                      legend.title = element_text(size = 11),
                      legend.text = element_text(size = 10),
                      axis.title = element_text(size = 10),
                      axis.text = element_text(size = 8))  
  
  target_season_plot <- ggplot() + 
    
    # Average season
    geom_line(
      data = monthly_data_filtered, 
      mapping = aes(
        x = Calendar_year_month, 
        y = Ave_season_monthly_cases, 
        color = "Average season")
    ) + 
    
    scale_color_manual(
      name = NULL,
      values = c("Average season" = "black")
    )  + 
    
    ggnewscale::new_scale_color() +
    
    # Current year plotting
    geom_line(
      data = monthly_data_filtered, 
      mapping = aes(
        x = Calendar_year_month, 
        y = complete_cases, 
        color = percentile_most_recent)
    ) + 
    
    scale_color_gradient(
      name = "Current season severity",
      low = "cyan",
      high = "purple",
      limits = c(0, 100)
    ) +
    
    ggnewscale::new_scale_color() +
    
    # Previous year
    geom_line(
      data = previous_year_data, 
      mapping = aes(x = Month, 
                    y = Cases_clean,
                    color = "Previous year")
    ) +
    
    scale_color_manual(
      name = NULL,
      values = c("Previous year" = "green4")
    ) + 
    
    # Axis labels
    labs(x = "Month", 
         y = "Monthly cases", 
         title = paste0(countrycode(target_country_iso3, "iso3c", "country.name"))) +
    
    scale_x_continuous(breaks = 1:12, labels = month.abb[]) +
    scale_y_continuous(labels = label_comma()) +
    
    # Themes
    theme_minimal() + 
    plot_theme
  
  if(radial == TRUE){
    # Convert to radial format
    target_season_plot_radial <- target_season_plot + 
      geom_tile(
        data = monthly_data_filtered,
        mapping = aes(x = Calendar_year_month, y = Ave_season_monthly_cases),
        alpha = 0
      ) +
      coord_polar(theta = "x", start = -pi/12) + 
      labs(x = "")
    
    return(target_season_plot_radial)
    
  } else if(radial == FALSE){
    
    return(target_season_plot)
  }
  
  
}



#--------------- Saving 
dir_to_save <- paste0("V1/DEV/03_Radial_barplot_sample_vis_2025/", Sys.Date())
dir.create(dir_to_save)

# Save dist comparison plots 
map2(dist_fit_plots,
     names(dist_fit_plots),
    ~ {
      
      # If no plot in result return error message
      if(is.null(.x$result)){
        print(paste0("No figure for ", countrycode(.y, "iso3c", "country.name")))
      } 
      # If there is a plot in result save 
      else if(!is.null(.x$result)){
        
        # Define dir to save 
        dir_to_save <- paste0("V1/KJ_attic/Dist_fit_comparison/", Sys.Date())
        dir.create(dir_to_save)
        
        # Define filename to save 
        filename_to_save <- paste0(dir_to_save,
                                   "/", 
                                   countrycode(.y, 
                                               "iso3c", 
                                               "country.name"),
                                   "Negative_binomial_dist_comparison.png")
        
        # Save image
        ggsave(.x$result, 
               filename = filename_to_save,
               device = "png", 
               dpi = 300)
      }
    }
    )

# Save target season sample visualisation  
ggsave(target_season_plot,
       filename = paste0(dir_to_save, "/THA_sample_linear_barplot_2025.png"),
       device = "png",
       dpi = 300)

