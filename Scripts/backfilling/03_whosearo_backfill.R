#' ---------------------------------------------------------------------------
#' 02_dengue_rf.R
#' ---------------------------------------------------------------------------
#' 
#' 

library(lubridate)
library(cowplot)

# region add format
source("Assets/Stable/OD_maps/fn_OD_region.R")

# ---- WHO----
##---- import data set ----

who <- download_and_standardise("WHO")

who$ISO_A0 <- who$iso3
who <- add_od_regions(who)

##---- Review data -----

str(who)

# select required data columns 

who <- who %>%
  dplyr::select(iso3,country,s,t,tr,total_den, od_region, date)
    # note that s = year, t = month , R_t (need to change) is reporting time 


##---- Split into reporting and validated data ----

# this is the most recent version of the data 
validation_date <- max(who$tr)
# for the reportig data only data from 1 year prior can be used 
max_reporting_date <- validation_date- years(1) 

# final data
v_who <- who %>%
  filter( tr == validation_date) %>%
  mutate(total_den_F = total_den) %>%
  select(-c(total_den,tr))

# reporting data 

r_who <- who %>%
  filter( tr < max_reporting_date)

## ---- calculate delay ----

# the delay in reporting is the differnce between the "date" and the "tr".
## it is reported in months 

r_who <- r_who %>%
  mutate(
    d = as.integer(round(as.numeric(tr - as.Date(date)) / 30.44)),
    d_scale = "month"
  )

## ---- add final case data ----

d_who <- r_who %>%
  left_join(v_who)

## ----- calculate impact of delay on reporting (at each observation) ----

d_who <- d_who %>%
  filter( d <= 12) %>% # limit to within a year 
  mutate(
    case_diff = total_den_F - total_den,
    diff_ratio = case_diff / total_den_F,
    case_complete = total_den / total_den_F,
    rf = total_den_F/total_den
  )

# quick look
d_who %>%
  ggplot(aes(x =d, y = rf, colour = country))+
    geom_smooth() +
    geom_point() +
    geom_hline(yintercept = 1) +
    theme_cowplot() +
    theme(
    legend.position = "blank") 

d_who %>%
  ggplot(aes( x= d, y = diff_ratio, colour = country))+ 
  geom_smooth()


## --- country/dealy summaries ----

d_who_summary <- d_who %>%
  filter(is.finite(rf)) %>%
  filter (rf < 100) %>% # remove extream outliners and non-"normal" reporting
  group_by(iso3,country, d) %>%
  summarise(
    od_region = dplyr::first(od_region),
    u_rf    = mean(rf, na.rm = TRUE),
    med_rf  = median(rf, na.rm = TRUE),
    max_rf  = max(rf, na.rm = TRUE),
    min_rf  = min(rf, na.rm = TRUE),
    sd_rf   = sd(rf, na.rm = TRUE),
    n_rf    = n(),
    .groups = "drop"
  )

# view summary 

d_who_summary %>%
  ggplot(aes( x= d, y=u_rf, colour = country))+ 
  geom_line() + 
  # geom_ribbon(
  #   aes(
  #     ymax = (med_rf + 2*sd_rf),
  #     ymin = (med_rf - 2*sd_rf),
  #     fill = country
  #     ), alpha = 0.5) + 
  facet_wrap(~who_region) + 
  theme_cowplot()


# Publication Figure

country_order <- d_who_summary %>%
  filter(u_rf >= 0.95, u_rf <= 1.05) %>%
  group_by(iso3, country, od_region) %>%
  summarise(d_near1 = min(d), .groups = "drop")


d_who_summary %>%
  filter(od_region != "Other") %>%
  left_join(country_order, by = c("iso3", "country", "od_region")) %>%
  mutate(
    d_near1 = if_else(is.na(d_near1), Inf, as.numeric(d_near1)), 
    Mean_Reporting_Factor= case_when(
      u_rf >= 0.95 & u_rf <= 1.05 ~ "Near 1",
      u_rf < 0.99 ~ "Over-reporting (less than 0.95)",
      u_rf > 1.05 ~ "Under-reporting (greater than 1.05)"
    )) %>%
  ggplot(aes(x = as.factor(d), y = reorder(country, d_near1, FUN = function(x) -min(x)), fill = Mean_Reporting_Factor)) +
    geom_tile(color = "white", size = 0.3) +
    theme_cowplot()+
    labs(fill = "Mean Reporting Factor", x = "Delay (months)", y = "") +
  scale_fill_manual(
    values = c(
      "Under-reporting (greater than 1.05)" = "#F2E55C",  # Light yellow
      "Near 1" = "#5F8F73",                               # Medium green
      "Over-reporting (less than 0.9)" = "#8C3B12"       # Dark orange
    )) +
    theme(
      legend.position = "top"    ) +
  facet_grid(
    od_region ~ ., 
    scales = "free_y", 
    space = "free_y"
  ) +
  theme(
    strip.text.y = element_text(angle = 0, size = 8, face = "bold"),
    strip.background = element_rect(colour = "grey80", fill = "grey95"),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 8),
    legend.title = element_text("Mean Reporting Factor", size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.35, "cm")
  ) 

## --- calculated empirical rf with uncertainty ----

# ---- SEARO----
##---- import data set ----

searo <- download_and_standardise("SEARO")

searo <- searo %>%
  mutate(iso3 = iso3c)
searo$ISO_A0 <- searo$iso3
searo <- add_od_regions(searo)

##---- Review data -----

str(searo)

# select required data columns

searo <- searo %>%
  dplyr::select(iso3, country, s, t, tr, total_den, od_region) %>%
  mutate(date = make_date(year = s, month = t, day = 1))

##---- Split into reporting and validated data ----

validation_date <- max(searo$tr)
max_reporting_date <- validation_date - years(1)

v_searo <- searo %>%
  filter(tr == validation_date) %>%
  mutate(total_den_F = total_den) %>%
  select(-c(total_den, tr))

r_searo <- searo %>%
  filter(tr < max_reporting_date)

## ---- calculate delay ----

r_searo <- r_searo %>%
  mutate(
    d = as.integer(round(as.numeric(tr - as.Date(date)) / 30.44)),
    d_scale = "month"
  )

## ---- add final case data ----

d_searo <- r_searo %>%
  left_join(v_searo)

## ----- calculate impact of delay on reporting (at each observation) ----

d_searo <- d_searo %>%
  filter(d <= 12) %>%
  mutate(
    case_diff = total_den_F - total_den,
    diff_ratio = case_diff / total_den_F,
    case_complete = total_den / total_den_F,
    rf = total_den_F / total_den
  )

d_searo %>%
  ggplot(aes(x = d, y = rf, colour = country)) +
  geom_smooth() +
  geom_point() +
  geom_hline(yintercept = 1) +
  theme_cowplot() +
  theme(legend.position = "blank")

d_searo %>%
  ggplot(aes(x = d, y = diff_ratio, colour = country)) +
  geom_smooth()

## --- country/delay summaries ----

d_searo_summary <- d_searo %>%
  filter(is.finite(rf)) %>%
  filter(rf < 100) %>%
  group_by(iso3, country, d) %>%
  summarise(
    od_region = dplyr::first(od_region),
    u_rf = mean(rf, na.rm = TRUE),
    med_rf = median(rf, na.rm = TRUE),
    max_rf = max(rf, na.rm = TRUE),
    min_rf = min(rf, na.rm = TRUE),
    sd_rf = sd(rf, na.rm = TRUE),
    n_rf = n(),
    .groups = "drop"
  )

d_searo_summary %>%
  ggplot(aes(x = d, y = u_rf, colour = country)) +
  geom_line() +
  facet_wrap(~od_region) +
  theme_cowplot()

# Publication Figure (SEARO)

country_order_searo <- d_searo_summary %>%
  filter(u_rf >= 0.95, u_rf <= 1.05) %>%
  group_by(iso3, country, od_region) %>%
  summarise(d_near1 = min(d), .groups = "drop")

d_searo_summary %>%
  filter(od_region != "Other") %>%
  left_join(country_order_searo, by = c("iso3", "country", "od_region")) %>%
  mutate(
    d_near1 = if_else(is.na(d_near1), Inf, as.numeric(d_near1)),
    Mean_Reporting_Factor = case_when(
      u_rf >= 0.95 & u_rf <= 1.05 ~ "Near 1",
      u_rf < 0.99 ~ "Over-reporting (less than 0.95)",
      u_rf > 1.05 ~ "Under-reporting (greater than 1.05)"
    )
  ) %>%
  ggplot(aes(
    x = as.factor(d),
    y = reorder(country, d_near1, FUN = function(x) -min(x)),
    fill = Mean_Reporting_Factor
  )) +
  geom_tile(color = "white", size = 0.3) +
  theme_cowplot() +
  labs(fill = "Mean Reporting Factor", x = "Delay (months)", y = "") +
  scale_fill_manual(
    values = c(
      "Under-reporting (greater than 1.05)" = "#F2E55C",
      "Near 1" = "#5F8F73",
      "Over-reporting (less than 0.9)" = "#8C3B12"
    )
  ) +
  theme(legend.position = "top") +
  facet_grid(
    od_region ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  theme(
    strip.text.y = element_text(angle = 0, size = 8, face = "bold"),
    strip.background = element_rect(colour = "grey80", fill = "grey95"),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.35, "cm")
  )
