# 03_nowcast_validation_summary_2.R

library(tidyverse)
library(cowplot)

# --- Paths / constants -------------------------------------------------------
out_dir <- "Output/validation"
validation_detail_path <- file.path(out_dir, "validation_detail.csv")
if (!file.exists(validation_detail_path)) {
  stop("Missing ", validation_detail_path, " — run 03_nowcast_validation_ind.R first.")
}

validation_detail <- read_csv(validation_detail_path, show_col_types = FALSE)

# ----------- Evaluate lead time performance ----------

validation_detail <- validation_detail %>%
  mutate(
    lead_time = prediction_month - cutoff_month
  )

# ----- identify outliners -----#

validation_out <- validation_detail %>%
  filter(scaled_squared_error > 15 )
# this exclused the top 5% of error 



summary_lead <- validation_detail %>%
  group_by(lead_time) %>%
  dplyr::summarise(
    n_seasons = n_distinct(season),
    n_predictions = n(),
    MAE = mean(abs(absolute_error), na.rm = TRUE),
    MAPE = mean(absolute_percent_error, na.rm = TRUE),
    RMSE = sqrt(mean(squared_error, na.rm = TRUE)),
    uMAPE = mean(scaled_absolute_percent_error, na.rm = TRUE),
    medMAPE = median(scaled_absolute_percent_error), 
    uRMSE = sqrt(mean(scaled_squared_error, na.rm = TRUE)),
    medRMSE = median(scaled_squared_error),
    .groups = "drop"
  )


summary_lead_filter <- validation_detail %>%
    filter(scaled_squared_error < 15 ) %>%
  group_by(lead_time) %>%
  dplyr::summarise(
    n_seasons = n_distinct(season),
    n_predictions = n(),
    MAE = mean(abs(absolute_error), na.rm = TRUE),
    MAPE = mean(absolute_percent_error, na.rm = TRUE),
    RMSE = sqrt(mean(squared_error, na.rm = TRUE)),
    uMAPE = mean(scaled_absolute_percent_error, na.rm = TRUE),
    medMAPE = median(scaled_absolute_percent_error), 
    uRMSE = sqrt(mean(scaled_squared_error, na.rm = TRUE)),
    medRMSE = median(scaled_squared_error),
    .groups = "drop"
  )

ggplot( validation_detail, aes(x = mean_actual_predicted_month, y = scaled_squared_error))+
  geom_point()


 fig_leadtime <- summary_lead %>%
   ggplot( )+
    geom_line(aes(x = lead_time, y = uRMSE)) +
    geom_line(aes( x= lead_time , y = uMAPE), linetype = "dashed") +
    theme_cowplot() + 
    ylab("uRMSE, dashline = uMAPE") +
    xlab("Lead Time")

summary_lead_region <- validation_detail %>%
  group_by(lead_time, Region ) %>%
  dplyr::summarise(
    n_seasons = n_distinct(season),
    n_predictions = n(),
    MAE = mean(abs(absolute_error), na.rm = TRUE),
    MAPE = mean(absolute_percent_error, na.rm = TRUE), 
    medAPE = median(absolute_percent_error, na.rm = TRUE),
    RMSE = sqrt(mean(squared_error, na.rm = TRUE)),
    MRE_signed = mean(relative_error, na.rm = TRUE),
    MRE_abs = mean(abs(relative_error), na.rm = TRUE),
    .groups = "drop"
  )


# ------------ Evaluate by time in season ------------- #

summary_season_month <- validation_detail %>%
  group_by(prediction_month) %>%
  dplyr::summarise(
    n_seasons = n_distinct(season),
    n_predictions = n(),
    MAE = mean(abs(absolute_error), na.rm = TRUE),
    MAPE = mean(absolute_percent_error, na.rm = TRUE),
    RMSE = sqrt(mean(squared_error, na.rm = TRUE)),
    uMAPE = mean(scaled_absolute_percent_error, na.rm = TRUE),
    uRMSE = sqrt(mean(scaled_squared_error, na.rm = TRUE)),
    .groups = "drop"
  )


 fig_seasonM <- validation_detail %>%
   filter ( lead_time ==1) %>%
  group_by(prediction_month) %>%
  dplyr::summarise(
    n_seasons = n_distinct(season),
    n_predictions = n(),
    MAE = mean(abs(absolute_error), na.rm = TRUE),
    MAPE = mean(absolute_percent_error, na.rm = TRUE),
    RMSE = sqrt(mean(squared_error, na.rm = TRUE)),
    uMAPE = mean(scaled_absolute_percent_error, na.rm = TRUE),
    uRMSE = sqrt(mean(scaled_squared_error, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
   ggplot( )+
    geom_line(aes(x = prediction_month, y = uRMSE)) +
    geom_line(aes( x= prediction_month , y = uMAPE), linetype = "dashed") +
    theme_cowplot() + 
    ylab("uRMSE, dashline = uMAPE") +
    xlab("Season Month")


 fig_seasonM_lt1_region <- validation_detail %>%
   filter (lead_time ==1) %>%
  group_by(prediction_month, Region) %>%
  dplyr::summarise(
    n_seasons = n_distinct(season),
    n_predictions = n(),
    MAE = mean(abs(absolute_error), na.rm = TRUE),
    MAPE = mean(absolute_percent_error, na.rm = TRUE),
    RMSE = sqrt(mean(squared_error, na.rm = TRUE)),
    uMAPE = mean(scaled_absolute_percent_error, na.rm = TRUE),
    uRMSE = sqrt(mean(scaled_squared_error, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
   ggplot( )+
    geom_line(aes(x = prediction_month, y = uRMSE)) +
    geom_line(aes( x= prediction_month , y = uMAPE), linetype = "dashed") +
    facet_wrap(~Region, ncol = 2) + 
    theme_cowplot() + 
    ylab("uRMSE, dashline = uMAPE") +
    xlab("Season Month")

grid_seasonM <- plot_grid(fig_seasonM, fig_seasonM_lt1_region, 
  nrow = 1 )

summary_region <- validation_detail %>%
  group_by( Region ) %>%
  dplyr::summarise(
    n_seasons = n_distinct(season),
    n_predictions = n(),
    MAE = mean(abs(absolute_error), na.rm = TRUE),
    MAPE = mean(absolute_percent_error, na.rm = TRUE), 
    medAPE = median(absolute_percent_error, na.rm = TRUE),
    RMSE = sqrt(mean(squared_error, na.rm = TRUE)),
    MRE_signed = mean(relative_error, na.rm = TRUE),
    MRE_abs = mean(abs(relative_error), na.rm = TRUE),
    .groups = "drop"
  )


ggplot(data = validation_detail, aes( x = season, y = absolute_percent_error)) +
  geom_boxplot()


# -------------- Evaluate country ----------------

summary_country <- validation_detail %>%
  group_by(iso3, country, Region) %>%
  dplyr::summarise(
    n_seasons = n_distinct(season),
    n_predictions = n(),
    MAE = mean(abs(absolute_error), na.rm = TRUE),
    MAPE = mean(absolute_percent_error, na.rm = TRUE),
    RMSE = sqrt(mean(squared_error, na.rm = TRUE)),
    uMAPE = mean(scaled_absolute_percent_error, na.rm = TRUE),
    uRMSE = sqrt(mean(scaled_squared_error, na.rm = TRUE)),
    MRE_signed = mean(relative_error, na.rm = TRUE),
    MRE_abs = mean(abs(relative_error), na.rm = TRUE),
    .groups = "drop"
  )