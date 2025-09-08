#' ---
#' title: "03_Radial_barplot_sample_vis_TMP"
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
#'

library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(scales)

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

#--------------- Generate predictions
current_month <- month(Sys.Date())
prediction_months <- ((current_month + 1):(current_month + 2) - 1) %% 12 + 1

monthly_predictions <- avg_season %>% 
  ungroup() %>% 
  
  # Join average season to year to predict data
  left_join(
    .,
    WHO_data_2025, 
    join_by("Calendar_year_month" == Month, "iso3")
  ) %>% 
  distinct() %>%
  arrange(Country, 
          iso3, 
          Calendar_year_month) %>%
  
  # Filter for future time window of two months 
  # mutate(End_prediction_month =  month(Sys.Date()) + 2) %>%
  # filter(Calendar_year_month <= End_prediction_month) %>%
  
  filter(Calendar_year_month <= current_month |
           (Calendar_year_month > current_month | Calendar_year_month %in% prediction_months)) %>% 
  
  # Assign var based on whether cases are observed 
  group_by(Country, iso3) %>% 
  mutate(Actual_cases_to_date = cumsum(cases)) %>%
  mutate(Data_status = case_when(is.na(cases) ~ "Predicted",
                                 !is.na(cases) ~ "Observed")) %>%
  
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
      case_when(most_recent_month == "Most_recent" ~ Actual_cases_to_date / Ave_cum_monthly_proportion,
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
        length(unique(na.omit(Predicted_total_seasonal_cases))) > 0 ~ "Data_available")
  ) %>% 
  
  # Fill predicted total seasonal case col, where no data available fill with NA. Else fill with predicted value
  mutate(Predicted_total_seasonal_cases = 
           case_when(Any_data_available == "No_data_available" ~ Predicted_total_seasonal_cases,
                     Any_data_available == "Data_available" ~ first(na.omit(Predicted_total_seasonal_cases))
           )
  ) %>%
  mutate(
    Pred_cases_lower_95CI = Predicted_total_seasonal_cases * lower_95CI,
    Pred_cases = Predicted_total_seasonal_cases * Ave_monthly_proportion,
    Pred_cases_upper_95CI = Predicted_total_seasonal_cases * upper_95CI,
  ) %>% 
  select(
    # Identifiers
    Country, iso3, Calendar_year_month, season_nMonth, 
    
    # Observed cases + whether cases are observed or predicted, 
    cases, Data_status, 
    
    # Predictions
    Pred_cases_lower_95CI,
    Pred_cases, 
    Pred_cases_upper_95CI,
    
    # Average season 
    Ave_season_cases_two_sd_lower, Ave_season_cases_one_sd_lower, 
    Ave_season_cases, 
    Ave_season_cases_one_sd_upper, Ave_season_cases_two_sd_upper
  ) %>%
  
  mutate(
    # Where cases are observed input NA for predictions, else retain predictions      
    across(
      starts_with("Pred"),
      ~ case_when(
        Data_status == "Observed" ~ NA,
        Data_status == "Predicted" ~ .
      )),
    # Where the lower bound of cases is < 0 change to 0. 
    across(
      ends_with("lower"),
      ~ case_when(. < 0 ~ 0,
                  TRUE ~ .)
    ),
    cases_to_plot = case_when(
      Data_status == "Observed" ~ cases,
      Data_status == "Predicted" ~ Pred_cases
    )
  ) %>%
  ungroup()

#--------------- Visualise data 

plot_theme <- theme(plot.title = element_text(size = 12),
                    legend.title = element_text(size = 10),
                    legend.text = element_text(size = 8),
                    axis.title = element_text(size = 10),
                    axis.text = element_text(size = 8))   

monthly_predictions_THA <- monthly_predictions %>% 
  filter(iso3 == "THA")

monthly_predictions_THA <- monthly_predictions_THA %>%
  bind_rows(
    filter(., Calendar_year_month == 1) %>%
      mutate(Calendar_year_month = 13)
  )

target_season_plot <- ggplot(monthly_predictions_THA) + 
  # Average season 
  geom_line(
    mapping = aes(x = Calendar_year_month, y = Ave_season_cases), color = "white") +
  
  # Average season lower bounds
  geom_ribbon(
    mapping = aes(x = Calendar_year_month, 
                  ymin = Ave_season_cases_two_sd_lower, ymax = Ave_season_cases, fill = "- Two SD"), alpha = 0.4) + 
  
  # Average season upper bounds 
  geom_ribbon(
    mapping = aes(x = Calendar_year_month, 
                  ymin = Ave_season_cases, ymax = Ave_season_cases_one_sd_upper, fill = "+ One SD"), alpha = 0.4) + 
  geom_ribbon(
    mapping = aes(x = Calendar_year_month, 
                  ymin = Ave_season_cases_one_sd_upper, ymax = Ave_season_cases_two_sd_upper, fill = "+ Two SD"), alpha = 0.4) + 
  
  # Traffic light colours
  scale_fill_manual(
    name = "Distance from\nseasonal baseline",
    values = c("- Two SD" = "green4",
               "+ One SD" = "orange",
               "+ Two SD" = "red3"),
    breaks = c("- Two SD", 
               "+ One SD", 
               "+ Two SD")
  )  + 
  
  # Current season plotting
  geom_line(aes(x = Calendar_year_month, y = cases_to_plot, color = Data_status, group = 1)) + 
  
  # Observed and predicted colours
  scale_color_manual(
    name = NULL,
    values = c("Observed" = "black",
               "Predicted" = "blue")
  ) +
  
  # Current season plotting: Predicted cases error 
  geom_ribbon(mapping = 
                aes(x = Calendar_year_month, 
                    ymin = Pred_cases_lower_95CI, ymax = Pred_cases_upper_95CI, fill = "Predicted cases 95% CIs"), alpha = 0.4) +
  
  # 95% CIs around predictions colours
  ggnewscale::new_scale_fill() +
  scale_fill_manual(
    name = NULL,
    values = c("Predicted cases 95% CIs" = "blue")
  ) +
  
  # Axis labels
  labs(x = "Month", y = "Monthly cases") +
  
  # scale_x_continuous(breaks = 1:12, limits = c(1, 13)) +
  scale_x_continuous(breaks = seq(1.5, 12.5, 1), labels = 1:12, limits = c(1, 13)) +
  scale_y_continuous(labels = label_comma()) +
  
  # Themes
  theme_minimal() + 
  plot_theme

target_season_plot

# Convert to radial format
target_season_plot_radial <- target_season_plot+ 
  geom_tile(
    mapping = aes(x = Calendar_year_month, y = Ave_season_cases)
    ) +
  coord_polar(theta = "x") 

#--------------- Saving 
dir_to_save <- paste0("V1/DEV/03_Radial_barplot_sample_vis_TMP/", Sys.Date())
dir.create(dir_to_save)

ggsave(target_season_plot,
       filename = paste0(dir_to_save, "/THA_sample_barplot_2025.png"),
       device = "png",
       dpi = 300)

ggsave(target_season_plot_radial,
       filename = paste0(dir_to_save, "/THA_sample_radial_barplot_2025.png"),
       device = "png",
       dpi = 300)
    
    
    