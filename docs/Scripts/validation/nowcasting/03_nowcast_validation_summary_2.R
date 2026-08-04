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

# countries currently in the GDO
country_list<- read_csv("pages/country/country-config.csv") %>%
  select(iso3, country_name, region)


# ----------- Evaluate lead time performance ----------

validation_detail <- validation_detail %>%
  mutate(
    lead_time = prediction_month - cutoff_month
  )
## --------- Global summary ----------
# All countris 
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

# Filter to countries in the GDO
summary_lead_GDO <- validation_detail %>%
  filter( country %in% country_list$country_name) %>%
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

# Filter to countries NOT in the GDO
summary_lead_N_GDO <- validation_detail %>%
  filter( !country %in% country_list$country_name) %>%
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

### ----- Plots ----

 summary_lead %>%
  ggplot( )+
  geom_line(aes(x = lead_time, y = uMAPE)) +
  geom_line(aes( x= lead_time , y = medMAPE), linetype = "dashed") +
  theme_cowplot() + 
  ylab("uMAPE, dashline = meduMAPE") +
  xlab("Lead Time")

fig_leadtime <- summary_lead_GDO %>%
  ggplot( )+
  geom_line(aes(x = lead_time, y = uMAPE)) +
  geom_line(aes( x= lead_time , y = medMAPE), linetype = "dashed") +
  theme_cowplot() + 
  ylab("uMAPE, dashline = meduMAPE") +
  xlab("Lead Time")


summary_lead_N_GDO %>%
  ggplot( )+
  geom_line(aes(x = lead_time, y = uMAPE)) +
  geom_line(aes( x= lead_time , y = medMAPE), linetype = "dashed") +
  theme_cowplot() + 
  ylab("uMAPE, dashline = meduMAPE") +
  xlab("Lead Time")

## --------- Regional summary ----------

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
    uMAPE = mean(scaled_absolute_percent_error, na.rm = TRUE),
    medMAPE = median(scaled_absolute_percent_error), 
    .groups = "drop"
  )


summary_lead_region_GDO <- validation_detail %>%
  filter( country %in% country_list$country_name) %>%
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
    uMAPE = mean(scaled_absolute_percent_error, na.rm = TRUE),
    medMAPE = median(scaled_absolute_percent_error), 
    .groups = "drop"
  )
### ----- Plots ----

Summary_lead_region %>%
  ggplot( )+
  geom_line(aes(x = lead_time, y = uMAPE)) +
  geom_line(aes( x= lead_time , y = medMAPE), linetype = "dashed") +
  theme_cowplot() + 
  ylab("uMAPE, dashline = meduMAPE") +
  xlab("Lead Time") +
  facet_wrap(~Region, ncol = 4) 

fig_leadtime_region <-  summary_lead_region_GDO %>%
  ggplot( )+
  geom_line(aes(x = lead_time, y = uMAPE)) +
  geom_line(aes( x= lead_time , y = medMAPE), linetype = "dashed") +
  theme_cowplot() + 
  ylab("uMAPE, dashline = meduMAPE") +
  xlab("Lead Time") +
  facet_wrap(~Region, ncol = 4) 

## --------- Country summary ----------

summary_lead_country <- validation_detail %>%
  group_by(lead_time, country) %>%
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


summary_lead_country_GDO <- validation_detail %>%
  filter( country %in% country_list$country_name) %>%
  group_by(lead_time, country, Region) %>%
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

summary_lead_country_N_GDO <- validation_detail %>%
  filter( !country %in% country_list$country_name) %>%
  group_by(lead_time, country, Region) %>%
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
  

summary_lead_country_GDO %>%
  ggplot( )+
  geom_line(aes(x = lead_time, y = uMAPE, group = country, colour = country)) +
  theme_cowplot() + 
  theme(legend.position = "bottom") +
  ylab("uMAPE, dashline = meduMAPE") +
  xlab("Lead Time") +
  facet_wrap(~Region)



# how many big misses were there at lead time = 1: 

validation_detail %>%
  filter( country %in% country_list$country_name) %>%
  filter(lead_time < 4) %>%
  filter(scaled_absolute_percent_error > 1.5) %>%
  # observations = 2855 (12.2% of lead time = 1)
  distinct(country)
  # 63 countries 

summary_lead_country_GDO %>%
  filter( country %in% country_list$country_name) %>%
  filter(lead_time == 1) %>%
  filter(uMAPE > 1.5) %>%
  distinct(country)
# 10 countries 


t <- validation_detail %>%
  filter(scaled_absolute_percent_error <= 1.5) %>%
  filter( country %in% country_list$country_name) %>%
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

# how many big misses were there at lead time = 1: 

print(summary_lead_country_GDO %>%
  filter( country %in% country_list$country_name) %>%
  filter(lead_time > 8 ) %>% # 201
  filter(uMAPE > 1.5) %>% # 59
  distinct(country), n=22)
# 22 countries 


validation_detail %>%
  filter(scaled_absolute_percent_error <= 1.5) %>%
  filter( country %in% country_list$country_name) %>%
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

# GDO plot
grid_leadtime <- plot_grid(fig_leadtime, fig_leadtime_region, 
                          nrow = 2 )

ggsave2( "/Users/lshks26/Dropbox/DMMG/DENV_dashboard/GDO_mauscript_1/Figures/FIG5_leadtime_APE.pdf",
         grid_leadtime)



# ------------ Evaluate by time in season ------------- 

## ----- Global ------

# Global - all lead times 
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
    medMAPE = median(scaled_absolute_percent_error), 
    .groups = "drop"
  )

# Global - 1 month lead times 
summary_season_month_lt1<- validation_detail %>%
  filter (lead_time ==1) %>%
  group_by(prediction_month) %>%
  dplyr::summarise(
    n_seasons = n_distinct(season),
    n_predictions = n(),
    MAE = mean(abs(absolute_error), na.rm = TRUE),
    MAPE = mean(absolute_percent_error, na.rm = TRUE),
    RMSE = sqrt(mean(squared_error, na.rm = TRUE)),
    uMAPE = mean(scaled_absolute_percent_error, na.rm = TRUE),
    uRMSE = sqrt(mean(scaled_squared_error, na.rm = TRUE)),
    medMAPE = median(scaled_absolute_percent_error), 
    .groups = "drop"
  )


# Global GDO - all lead times 
summary_season_month_GDO <- validation_detail %>%
  filter( country %in% country_list$country_name) %>%
  group_by(prediction_month) %>%
  dplyr::summarise(
    n_seasons = n_distinct(season),
    n_predictions = n(),
    MAE = mean(abs(absolute_error), na.rm = TRUE),
    MAPE = mean(absolute_percent_error, na.rm = TRUE),
    RMSE = sqrt(mean(squared_error, na.rm = TRUE)),
    uMAPE = mean(scaled_absolute_percent_error, na.rm = TRUE),
    uRMSE = sqrt(mean(scaled_squared_error, na.rm = TRUE)),
    medMAPE = median(scaled_absolute_percent_error), 
    .groups = "drop"
  )

# Global GDO - 1 month lead times 
summary_season_month_lt1_GDO <- validation_detail %>%
  filter( country %in% country_list$country_name) %>%
  filter (lead_time ==1) %>%
  group_by(prediction_month) %>%
  dplyr::summarise(
    n_seasons = n_distinct(season),
    n_predictions = n(),
    MAE = mean(abs(absolute_error), na.rm = TRUE),
    MAPE = mean(absolute_percent_error, na.rm = TRUE),
    RMSE = sqrt(mean(squared_error, na.rm = TRUE)),
    uMAPE = mean(scaled_absolute_percent_error, na.rm = TRUE),
    uRMSE = sqrt(mean(scaled_squared_error, na.rm = TRUE)),
    medMAPE = median(scaled_absolute_percent_error), 
    .groups = "drop"
  )

    ### --- Plots ------
    
    summary_season_month%>%
      ggplot( )+
      geom_line(aes(x = as.integer(prediction_month), y = uMAPE)) +
      geom_line(aes( x= as.integer(prediction_month) , y = medMAPE), linetype = "dashed") +
      theme_cowplot() + 
      ylab("uMAPE, dashline = meduMAPE") +
      xlab("Season Month") + 
      ylim(0,1.5)
    
    
    summary_season_month_lt1%>%
      ggplot( )+
      geom_line(aes(x = as.integer(prediction_month), y = uMAPE)) +
      geom_line(aes( x= as.integer(prediction_month) , y = medMAPE), linetype = "dashed") +
      theme_cowplot() + 
      ylab("uMAPE, dashline = meduMAPE") +
      xlab("Season Month") + 
      ylim(0,2)
    
    
    
    
    summary_season_month_GDO%>%
      ggplot( )+
      geom_line(aes(x = as.integer(prediction_month), y = uMAPE)) +
      geom_line(aes( x= as.integer(prediction_month) , y = medMAPE), linetype = "dashed") +
      theme_cowplot() + 
      ylab("uMAPE, dashline = meduMAPE") +
      xlab("Season Month") + 
      ylim(0,1.5)
    
    
    fig_seasonM <- summary_season_month_lt1_GDO%>%
      ggplot( )+
      geom_line(aes(x = as.integer(prediction_month), y = uMAPE)) +
      geom_line(aes( x= as.integer(prediction_month) , y = medMAPE), linetype = "dashed") +
      theme_cowplot() + 
      ylab("uMAPE, dashline = meduMAPE") +
      xlab("Season Month") + 
      ylim(0,2)



## ---- Regional ----


# Regional - 1 month lead times 
summary_region_season_month_lt1<- validation_detail %>%
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
    medMAPE = median(scaled_absolute_percent_error), 
    .groups = "drop"
  )


# Regional GDO - 1 month lead times 
summary_regions_season_month_lt1_GDO <- validation_detail %>%
  filter( country %in% country_list$country_name) %>%
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
    medMAPE = median(scaled_absolute_percent_error), 
    .groups = "drop"
  )


summary_region_season_month_lt1 %>%
  ggplot( )+
  geom_line(aes(x = as.integer(prediction_month), y = uMAPE)) +
  geom_line(aes( x= as.integer(prediction_month) , y = medMAPE), linetype = "dashed") +
  facet_wrap(~Region, ncol = 4) + 
  theme_cowplot() + 
  ylab("") +
  xlab("Season Month") + 
  ylim(0,2)


fig_seasonM_lt1_region <- summary_regions_season_month_lt1_GDO %>%
   ggplot( )+
   geom_line(aes(x = as.integer(prediction_month), y = uMAPE)) +
   geom_line(aes( x= as.integer(prediction_month) , y = medMAPE), linetype = "dashed") +
    facet_wrap(~Region, ncol = 4) + 
    theme_cowplot() + 
   ylab("") +
   xlab("Season Month") +
  ylim(0,2)


# GDO paper plot
grid_seasonM <- plot_grid(fig_seasonM, fig_seasonM_lt1_region, 
  nrow = 2 )

ggsave2( "/Users/lshks26/Dropbox/DMMG/DENV_dashboard/GDO_mauscript_1/Figures/FIG6_seasonMonth_APE.pdf",
         grid_seasonM)

### ------------ country ----------------

summary_season_month_country_GDO <- validation_detail %>%
  filter( country %in% country_list$country_name) %>%
  filter (lead_time ==1) %>%
  group_by(prediction_month, iso3, country, Region) %>%
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


summary_season_month_country_GDO %>% # 737 total
  filter(uMAPE > 1.5) %>% # 66 observations (9.1%)
  distinct(country)
# 23 countries 


validation_detail %>%
  filter( country %in% country_list$country_name) %>%
  filter (lead_time ==1) %>% # 8580 total
  filter(scaled_absolute_percent_error < 1.5) %>% # 7590 observations (89.1%)
  group_by(prediction_month) %>%
  summarise(
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


