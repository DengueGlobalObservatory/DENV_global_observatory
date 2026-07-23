#' ---------------------------------------------------------------------------
#' 02_dengue_rf.R
#' ---------------------------------------------------------------------------
#' 
#' 

library(lubridate)
library(cowplot)
library(dplyr)
library(ggplot2)
library(ISOweek)
library(stringr)

# region add format
source("Assets/Stable/OD_maps/fn_OD_region.R")
source("Scripts/backfilling/FUNCTIONS/00_FUN_whosearo_backfill.R")

# ---- Analysis parameters ----
# Broad row-level window (months) and RF summary cutoff (months; summaries use d < this)
max_delay_months_completeness <- 18L
max_delay_months_rf_summary <- 3L
max_delay_weeks_completeness <- as.integer(
  ceiling(max_delay_months_completeness * 30.44 / 7)
)


# countries currently in the GDO
country_list<- read_csv("pages/country/country-config.csv") %>%
  select(iso3, country_name, region)


# -------------------------------- WHO ---------------------------------------------------------------
##---- import data set ----

who <- download_and_standardise("WHO")
who_raw <- who
who <- who %>%
  mutate(iso3 = iso3c)
who$ISO_A0 <- who$iso3
who <- add_od_regions(who)

# WHO uses non-standard iso3 MDR for Autonomous Region of Madeira (see 01_select_historic_data.R).
# countrycode cannot map MDR, so add_od_regions assigns "Other"; override name and region here.
who <- who %>%
  mutate(
    country = if_else(iso3 == "MDR", "Autonomous Region of Madeira", country),
    od_region = if_else(
      iso3 == "MDR",
      get_od_regions("PRT")$od_region[1],
      od_region
    )
  )

# s = calendar year, t = month, tr = reporting snapshot date
who <- who %>%
  select(iso3, country, s, t, tr, total_den, od_region)

##---- Split into reporting and final data ----
# Final: latest snapshot (assumed stable).
# Reporting: earlier snapshots for country-months at least one year old.

validation_date <- max(who$tr)
max_reporting_date <- validation_date - years(1)

v_who <- who %>%
  filter(tr == validation_date) %>%
  mutate(total_den_F = total_den) %>%
  select(-c(total_den, tr))

r_who <- who %>%
  filter(tr < max_reporting_date)

## ---- calculate delay ----
# Delay in months between the case month (s, t) and the reporting snapshot (tr).

r_who <- r_who %>%
  mutate(
    d = as.integer(round(
      as.numeric(tr - make_date(year = s, month = t, day = 1)) / 30.44
    )),
    d_scale = "month"
  )

## ---- join partial and final counts ----

d_who <- r_who %>%
  left_join(v_who, by = c("iso3", "country","od_region", "s", "t"))

## ---- classify zero / missing pairs and derived metrics ----
# Zero pairs are kept in the data but handled differently from ratio analysis:
#   both_zero         — no transmission in partial or final count
#   zero_to_positive  — under-reported at this delay (ratio undefined)
#   positive_to_zero  — count revised down to zero in final data
#   both_positive     — eligible for reporting-factor (rf) calculation
#   missing           — no partial count, final count, or failed join

d_who <- d_who %>%
  filter(d <= max_delay_months_completeness) %>%
  mutate(
    zero_class = case_when(
      is.na(total_den) | is.na(total_den_F)      ~ "missing",
      total_den == 0 & total_den_F == 0          ~ "both_zero",
      total_den == 0 & total_den_F > 0           ~ "zero_to_positive",
      total_den > 0 & total_den_F == 0           ~ "positive_to_zero",
      TRUE                                       ~ "both_positive"
    ),
    case_class = case_when(
      total_den_F > 5                            ~ ">5",
      total_den_F <= 5 & total_den_F >0          ~ "<=5",
      total_den_F == 0                           ~ "zero"

    ),
    final_positive = !is.na(total_den_F) & total_den_F > 0,
    partial_positive = !is.na(total_den) & total_den > 0,
    case_diff = total_den_F - total_den,
    diff_ratio = if_else(total_den_F > 0, case_diff / total_den_F, NA_real_),
    case_complete = if_else(total_den_F > 0, total_den / total_den_F, NA_real_),
    rf = if_else(zero_class == "both_positive", total_den_F / total_den, NA_real_)
  )

## ---- summary tables  ----

# Overall makeup of zero classes across all delays <= max_delay_months_completeness
who_zero_summary <- d_who %>%
  count(zero_class, name = "n") %>%
  mutate(pct = 100 * n / sum(n))

# Zero-class counts by country 
who_zero_by_country_delay <- d_who %>%
  count(iso3, country, od_region, zero_class, name = "n") %>%
  group_by(iso3, country, od_region) %>%
  mutate(pct_within_delay = 100 * n / sum(n)) %>%
  ungroup()

d_who_sum <- d_who %>%
  filter( zero_class == "both_positive") %>%
  group_by(country, d)%>%
  summarise(
    n = n()
  )

# Part A: detection / concordance metrics (zeros included), all delays <= completeness window
who_detection_summary <- d_who %>%
  group_by(iso3, country, od_region, d) %>%
  summarise(
    n_months = n(),
    n_both_zero = sum(zero_class == "both_zero"),
    n_zero_to_positive = sum(zero_class == "zero_to_positive"),
    n_positive_to_zero = sum(zero_class == "positive_to_zero"),
    n_both_positive = sum(zero_class == "both_positive"),
    n_missing = sum(zero_class == "missing"),
    pct_both_zero = 100 * mean(zero_class == "both_zero"),
    pct_zero_to_positive = 100 * mean(zero_class == "zero_to_positive"),
    pct_final_positive = 100 * mean(final_positive),
    detection_rate = if_else(
      sum(final_positive) > 0,
      100 * sum(partial_positive & final_positive) / sum(final_positive),
      NA_real_
    ),
    .groups = "drop"
  )



# Part B: reporting-factor magnitudes on both_positive rows only (RF summary delay window)
# Summarised three ways to compare the effect of small final counts

## by country
who_rf_summary_country <- bind_rows(
  d_who %>%
    filter( country %in% country_list$country_name) %>%
    filter(zero_class == "both_positive", d < max_delay_months_rf_summary) %>%
    group_by(iso3, country, od_region) %>%
    summarise(
      rf_stratum = "d_lt_3",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    ),
  d_who %>%
    filter( country %in% country_list$country_name) %>%
    filter(zero_class == "both_positive", d >= max_delay_months_rf_summary) %>%
    group_by(iso3, country, od_region) %>%
    summarise(
      rf_stratum = "d_gte_3",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    ))


## by region and with delay categories (< 3 vs > 3 months)
who_rf_summary_region <- bind_rows(
  d_who %>%
    filter( country %in% country_list$country_name) %>%
    filter(zero_class == "both_positive", d < max_delay_months_rf_summary) %>%
    group_by(od_region) %>%
    summarise(
      rf_stratum = "d_lt_3",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    ),
  d_who %>%
    filter( country %in% country_list$country_name) %>%
    filter(zero_class == "both_positive", d >= max_delay_months_rf_summary) %>%
    group_by(od_region) %>%
    summarise(
      rf_stratum = "d_gte_3",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    )
)


## globally
who_rf_summary_global <- bind_rows(
  d_who %>%
    filter( country %in% country_list$country_name) %>%
    filter(zero_class == "both_positive", d < max_delay_months_rf_summary) %>%
    summarise(
      rf_stratum = "d_lt_3",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    ),
  d_who %>%
    filter( country %in% country_list$country_name) %>%
    filter(zero_class == "both_positive", d >= max_delay_months_rf_summary) %>%
    summarise(
      rf_stratum = "d_gte_3",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    )
)


## globally - sensitivity (w/o Sudan and Eritrea)
who_rf_summary_global_sen <- bind_rows(
  d_who %>%
    filter( country %in% country_list$country_name) %>%
    filter (!country %in% c("Sudan", "Eritrea")) %>%
    filter(zero_class == "both_positive", d < max_delay_months_rf_summary) %>%
    summarise(
      rf_stratum = "d_lt_3",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    ),
  d_who %>%
    filter( country %in% country_list$country_name) %>%
    filter (!country %in% c("Sudan", "Eritrea")) %>%
    filter(zero_class == "both_positive", d >= max_delay_months_rf_summary) %>%
    summarise(
      rf_stratum = "d_gte_3",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    )
)

## ---- figures  ----

# Table for forest plot: global estimate plus one row per region, by case stratum
who_rf_forest_data <- bind_rows(
  who_rf_summary_global %>% mutate(od_region = "Global"),
  who_rf_summary_region
) %>%
  mutate(
    rf_stratum = factor(
      rf_stratum,
      levels = c("d_lt_3", "d_gte_3"),
      labels = c("< 3 months", "≥ 3 months")
    ),
    rf_lo = u_rf - sd_rf,
    rf_hi = u_rf + sd_rf
  ) %>%
  filter(!is.na(u_rf)) %>%
  mutate(
    od_region = factor(
      od_region,
      levels = c("Global", sort(setdiff(unique(od_region), "Global")))
    )
  )
# Forest plot: mean RF (RF summary delay window) with +/- 1 SD whiskers
who_rf_forest_plot <- who_rf_forest_data %>%
  mutate(
    y_base = as.numeric(od_region),
    y_plot = y_base + case_when(
      rf_stratum == "< 3 months" ~  0.15,
      rf_stratum == "≥ 3 months" ~ -0.15
    )
  ) %>%
  ggplot(aes(x = u_rf, y = y_plot, color = rf_stratum)) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.3, colour = "grey40") +
  geom_errorbarh(
    aes(xmin = rf_lo, xmax = rf_hi),
    height = 0.08,
    linewidth = 0.4
  ) +
  geom_point(size = 2) +
  scale_y_continuous(
    breaks = sort(unique(as.numeric(who_rf_forest_data$od_region))),
    labels = levels(who_rf_forest_data$od_region)
  ) +
  scale_color_manual(
    values = c(
      "< 3 months" = "grey30",
      "≥ 3 months" = "#b2182b"
    ),
    name = ""
  ) +
  labs(
    x = "",
    y = NULL,
    color = "Delay"
  ) +
  theme_cowplot() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.35, "cm"),
    axis.text.y = element_text(size = 10)
  )

## --- E, ME and NA plots 
# Table for forest plot: 
who_EMENA__rf_forest_data <-who_rf_summary_country %>%
  filter(od_region == "Europe, Middle East & North Africa") %>%
  mutate(
    rf_stratum = factor(
      rf_stratum,
      levels = c("d_lt_3", "d_gte_3"),
      labels = c("< 3 months", "≥ 3 months")
    ),
    rf_lo = u_rf - sd_rf,
    rf_hi = u_rf + sd_rf
  ) %>%
  filter(!is.na(u_rf)) 
# Forest plot: mean RF (RF summary delay window) with +/- 1 SD whiskers
who_EMENA__rf_forest_plot <- who_EMENA__rf_forest_data %>%
  mutate(
    y_base = as.numeric(as.factor(country)),
    y_plot = y_base + case_when(
      rf_stratum == "< 3 months" ~  0.15,
      rf_stratum == "≥ 3 months" ~ -0.15
    )
  ) %>%
  ggplot(aes(x = u_rf, y = y_plot, color = rf_stratum)) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.3, colour = "grey40") +
  geom_errorbarh(
    aes(xmin = rf_lo, xmax = rf_hi),
    height = 0.08,
    linewidth = 0.4
  ) +
  geom_point(size = 2) +
  scale_y_continuous(
    breaks = sort(unique(as.numeric(as.factor(who_EMENA__rf_forest_data$country)))),
    labels = levels(as.factor(who_EMENA__rf_forest_data$country))
  ) +
  scale_color_manual(
    values = c(
      "< 3 months" = "grey30",
      "≥ 3 months" = "#b2182b"
    ),
    name = ""
  ) +
  labs(
    x = "Reporting factor (final cases/ case reported at delay)",
    y = NULL,
    color = "Delay"
  ) +
  theme_cowplot() +
  theme(
    legend.position = "blank",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.35, "cm"),
    axis.text.y = element_text(size = 10)
  )

## --- SSA plots 
# Table for forest plot: 
who_SSA_rf_forest_data <-who_rf_summary_country %>%
  filter(od_region == "Sub-Saharan Africa") %>%
  mutate(
    rf_stratum = factor(
      rf_stratum,
      levels = c("d_lt_3", "d_gte_3"),
      labels = c("< 3 months", "≥ 3 months")
    ),
    rf_lo = u_rf - sd_rf,
    rf_hi = u_rf + sd_rf
  ) %>%
  filter(!is.na(u_rf)) 
# Forest plot: mean RF (RF summary delay window) with +/- 1 SD whiskers
who_SSA__rf_forest_plot <- who_SSA_rf_forest_data %>%
  mutate(
    y_base = as.numeric(as.factor(country)),
    y_plot = y_base + case_when(
      rf_stratum == "< 3 months" ~  0.15,
      rf_stratum == "≥ 3 months" ~ -0.15
    )
  ) %>%
  ggplot(aes(x = u_rf, y = y_plot, color = rf_stratum)) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.3, colour = "grey40") +
  geom_errorbarh(
    aes(xmin = rf_lo, xmax = rf_hi),
    height = 0.08,
    linewidth = 0.4
  ) +
  geom_point(size = 2) +
  scale_y_continuous(
    breaks = sort(unique(as.numeric(as.factor(who_SSA_rf_forest_data$country)))),
    labels = levels(as.factor(who_SSA_rf_forest_data$country))
  ) +
  scale_color_manual(
    values = c(
      "< 3 months" = "grey30",
      "≥ 3 months" = "#b2182b"
    ),
    name = ""
  ) +
  labs(
    x = "",
    y = NULL,
    color = "Delay"
  ) +
  theme_cowplot() +
  theme(
    legend.position = "blank",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.35, "cm"),
    axis.text.y = element_text(size = 10)
  )

who_rf_plot <- plot_grid(who_rf_forest_plot, who_EMENA__rf_forest_plot, who_SSA__rf_forest_plot, nrow = 1, labels = "AUTO")

# --------------------------------- PAHO -------------------------------------------------------------------
##---- import data set ----

# Fast load: uses DENV_data_delay PAHO_crawler_dataPROC when present, else cached/GitHub
# Default snapshot window is 30 months (18 months analysis + 12 months reporting lag)
paho <- download_and_standardise(
  "PAHO",
  use_cache = TRUE,
  refresh_cache = FALSE
)
paho_raw <- paho
paho <- paho %>%
  mutate(iso3 = iso3c)
paho$ISO_A0 <- paho$iso3
paho <- add_od_regions(paho)
paho <- paho %>%
  mutate(country = if_else(iso3 == "MAF", "Saint Martin", country)) %>%
  filter( od_region != "Other") %>%
  select(iso3, country, s, t, tr, total_den, od_region)

##---- Split into reporting and final data ----
# PAHO is weekly; reporting snapshots are at least 52 weeks before validation.

validation_date_paho <- max(paho$tr)
max_reporting_date_paho <- validation_date_paho - weeks(52)

v_paho <- paho %>%
  filter(tr == validation_date_paho) %>%
  mutate(total_den_F = total_den) %>%
  select(-c(total_den, tr))

r_paho <- paho %>%
  filter(tr < max_reporting_date_paho)

if (nrow(r_paho) == 0) {
  stop(
    "PAHO: no reporting snapshots (tr < ", max_reporting_date_paho,
    "). Cannot compare partial and final counts; analysis stopped."
  )
}

## ---- calculate delay (weeks) ----

r_paho <- r_paho %>%
  mutate(
    onset_date = ISOweek::ISOweek2date(
      paste0(s, "-W", stringr::str_pad(t, 2, pad = "0"), "-1")
    ),
    d_week = as.integer(round(as.numeric(difftime(tr, onset_date, units = "weeks")))),
    d_scale = "week"
  )

## ---- join partial and final counts (weekly) ----

d_paho_weekly <- r_paho %>%
  left_join(v_paho, by = c("iso3", "country", "od_region", "s", "t"))

## ---- weekly RF and zero classification ----

d_paho_weekly <- d_paho_weekly %>%
  filter(d_week <= max_delay_weeks_completeness) %>%
  mutate(
    zero_class = case_when(
      is.na(total_den) | is.na(total_den_F)      ~ "missing",
      total_den == 0 & total_den_F == 0          ~ "both_zero",
      total_den == 0 & total_den_F > 0           ~ "zero_to_positive",
      total_den > 0 & total_den_F == 0           ~ "positive_to_zero",
      TRUE                                       ~ "both_positive"
    ),
    ew_year = s,
    ew = t,
    final_positive = !is.na(total_den_F) & total_den_F > 0,
    partial_positive = !is.na(total_den) & total_den > 0,
    case_diff = total_den_F - total_den,
    rf = if_else(zero_class == "both_positive", total_den_F / total_den, NA_real_)
  )

## ---- assign weeks to months (3-day week-end rule) ----

d_paho_monthly <- d_paho_weekly %>%
  paho_assign_epiweek_to_month() %>%
  mutate(
    d = as.integer(round(as.numeric(tr - month_date) / 30.44)),
    d_scale = "month"
  )

## ---- summary tables (stored as objects) ----

d_paho_m_sum <- d_paho_monthly %>%
  filter( zero_class == "both_positive") %>%
  group_by(country, d)%>%
  summarise(
    n = n()
  )

paho_zero_summary <- d_paho_monthly %>%
  count(zero_class, name = "n") %>%
  mutate(pct = 100 * n / sum(n))

paho_zero_by_country <- d_paho_monthly %>%
  count(iso3, country, od_region, zero_class, name = "n") %>%
  group_by(iso3, country, od_region) %>%
  mutate(pct_within_delay = 100 * n / sum(n)) %>%
  ungroup()

paho_detection_summary <- d_paho_monthly %>%
  group_by(iso3, country, od_region, d) %>%
  summarise(
    n_obs = n(),
    n_both_zero = sum(zero_class == "both_zero"),
    n_zero_to_positive = sum(zero_class == "zero_to_positive"),
    n_positive_to_zero = sum(zero_class == "positive_to_zero"),
    n_both_positive = sum(zero_class == "both_positive"),
    n_missing = sum(zero_class == "missing"),
    pct_both_zero = 100 * mean(zero_class == "both_zero"),
    pct_zero_to_positive = 100 * mean(zero_class == "zero_to_positive"),
    pct_final_positive = 100 * mean(final_positive),
    detection_rate = if_else(
      sum(final_positive) > 0,
      100 * sum(partial_positive & final_positive) / sum(final_positive),
      NA_real_
    ),
    .groups = "drop"
  )


paho_rf_summary_region <- bind_rows(
  d_paho_monthly %>%
    filter( country %in% country_list$country_name) %>%
    filter(zero_class == "both_positive", d < max_delay_months_rf_summary) %>%
    group_by(od_region) %>%
    summarise(
      rf_stratum = "d_lt_3",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    ),
  d_paho_monthly %>%
    filter( country %in% country_list$country_name) %>%
    filter(zero_class == "both_positive", d >= max_delay_months_rf_summary) %>%
    group_by(od_region) %>%
    summarise(
      rf_stratum = "d_gte_3",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    )
)

# PAHO ~Country summary 


paho_rf_summary_country <- bind_rows(
  d_paho_monthly %>%
    filter( country %in% country_list$country_name) %>%
    filter(zero_class == "both_positive", d < max_delay_months_rf_summary) %>%
    group_by(country, od_region)%>%
    summarise(
      rf_stratum = "d_lt_3",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    ),
  d_paho_monthly %>%
    filter( country %in% country_list$country_name) %>%
    filter(zero_class == "both_positive", d >= max_delay_months_rf_summary) %>%
    group_by(country, od_region)%>%
    summarise(
      rf_stratum = "d_gte_3",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    )
)


# There are 2 country with strange rf patterns - ARG and URG
## both means increase after 3 months but only URG sd also increase 
## I need to check if this is true for the weekly data as well

### it is more or less the same - less than 10% error

paho_rf_summary_country_W <- bind_rows(
  d_paho_weekly %>%
    filter( country %in% country_list$country_name) %>%
    filter(zero_class == "both_positive", d_week < 3) %>%
    group_by(country, od_region)%>%
    summarise(
      rf_stratum = "d_lt_3",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    ),
  d_paho_weekly %>%
    filter( country %in% country_list$country_name) %>%
    filter(zero_class == "both_positive", d_week >= 3) %>%
    group_by(country, od_region)%>%
    summarise(
      rf_stratum = "d_gte_3",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    )
)




## I will plot the delay x rf for these: 


d_paho_weekly %>%
  filter( iso3 == "ARG" | iso3 == "URY") %>%
  ggplot(aes(x = d_week, y = rf, colour = iso3)) +
  geom_point() + 
  geom_smooth()



d_paho_weekly %>%
  filter( iso3 == "BRA" ) %>%
  ggplot(aes(x = d_week, y = rf, colour = iso3)) +
  geom_point() + 
  geom_smooth()


U <- d_paho_weekly %>%
  filter( iso3 == "URY" ) %>%
  group_by(ew) %>%
  ggplot(aes(x = d_week, y = rf, colour = as.factor(ew), group = as.factor(ew))) +
  geom_smooth() +
  theme_cowplot()

A <- d_paho_weekly %>%
  filter( iso3 == "ARG" ) %>%
  group_by(ew) %>%
  ggplot(aes(x = d_week, y = rf, colour = as.factor(ew), group = as.factor(ew))) +
  geom_smooth() +
  theme_cowplot()

supp_fig_paho_rf <- plot_grid(A, U, nrow = 1, labels = "AUTO")

d_paho_weekly %>%
  filter( iso3 == "BRA" ) %>%
  ggplot(aes(x = d_week, y = ew, fill = rf)) +
  geom_tile()


# PAHO wide summary 

paho_rf_summary_paho <- bind_rows(
  d_paho_monthly %>%
    filter( country %in% country_list$country_name) %>%
    filter(zero_class == "both_positive", d < max_delay_months_rf_summary) %>%
    summarise(
      rf_stratum = "d_lt_3",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    ),
  d_paho_monthly %>%
    filter( country %in% country_list$country_name) %>%
    filter(zero_class == "both_positive", d >= max_delay_months_rf_summary) %>%
    summarise(
      rf_stratum = "d_gte_3",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    )
)
## ---- WHO vs PAHO delay comparison (shared countries) ----

who_paho_shared_iso3 <- intersect(unique(d_who$iso3), unique(d_paho_monthly$iso3))

who_paho_rf_compare <- bind_rows(
  d_who %>%
    filter( country %in% country_list$country_name) %>%
    filter(iso3 %in% who_paho_shared_iso3, zero_class == "both_positive", d < max_delay_months_rf_summary) %>%
    mutate(
      data_source = "WHO",
      delay = d,
      delay_unit = "month",
      month_s = s,
      month_t = t,
      ew_year = NA_integer_,
      ew = NA_integer_
    ),
  d_paho_monthly %>%
    filter( country %in% country_list$country_name) %>%
    filter(iso3 %in% who_paho_shared_iso3, zero_class == "both_positive", d < max_delay_months_rf_summary) %>%
    mutate(data_source = "PAHO", delay = d, delay_unit = "month")
) %>%
  select(
    data_source, iso3, country, od_region, delay, delay_unit,
    month_s, month_t, ew_year, ew, total_den, total_den_F, rf, zero_class
  )

who_paho_rf_summary_country <- who_paho_rf_compare %>%
  group_by(data_source, iso3, country, od_region) %>%
  summarise(
    u_rf = mean(rf, na.rm = TRUE),
    med_rf = median(rf, na.rm = TRUE),
    sd_rf = sd(rf, na.rm = TRUE),
    n_rf = n(),
    .groups = "drop"
  )

who_paho_rf_compare_plot_data <- who_paho_rf_summary_country %>%
  mutate(
    data_source = factor(data_source, levels = c("WHO", "PAHO")),
    rf_lo = u_rf - sd_rf,
    rf_hi = u_rf + sd_rf
  )

## ---- figures (stored as objects) ----

paho_rf_forest_data <- bind_rows(
  paho_rf_summary_paho %>% mutate(od_region = "PAHO"),
  paho_rf_summary_region
) %>%
  mutate(
    rf_stratum = factor(
      rf_stratum,
      levels = c("d_lt_3", "d_gte_3"),
      labels = c("< 3 months", "≥ 3 months")
    ),
    rf_lo = u_rf - sd_rf,
    rf_hi = u_rf + sd_rf
  ) %>%
  filter(!is.na(u_rf)) %>%
  mutate(
    od_region = factor(
      od_region,
      levels = c("PAHO", sort(setdiff(unique(od_region), "PAHO")))
    )
  )

paho_rf_forest_plot <- paho_rf_forest_data %>%
  mutate(
    y_base = as.numeric(od_region),
    y_plot = y_base + case_when(
      rf_stratum == "< 3 months" ~  0.15,
      rf_stratum == "≥ 3 months" ~ -0.15
    )
  ) %>%
  ggplot(aes(x = u_rf, y = y_plot, color = rf_stratum)) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.3, colour = "grey40") +
  geom_errorbarh(
    aes(xmin = rf_lo, xmax = rf_hi),
    height = 0.08,
    linewidth = 0.4
  ) +
  # xlim(-50,100) +
  geom_point(size = 2) +
  scale_y_continuous(
    breaks = sort(unique(as.numeric(paho_rf_forest_data$od_region))),
    labels = levels(paho_rf_forest_data$od_region)
  ) +
  scale_color_manual(
    values = c(
      "< 3 months" = "grey30",
      "≥ 3 months" = "#b2182b"
    ),
    name = ""
  ) +
  labs(
    x = "Reporting factor (final cases/ case reported at delay)",
    y = NULL,
    color = "Delay"
  ) +
  theme_cowplot() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.35, "cm"),
    axis.text.y = element_text(size = 10)
  )

who_paho_rf_compare_plot <- who_paho_rf_compare_plot_data %>%
  ggplot(aes(x = u_rf, y = country, color = data_source)) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.3, colour = "grey40") +
  geom_errorbarh(
    aes(xmin = rf_lo, xmax = rf_hi),
    position = position_dodge(width = 0.6),
    height = 0.2,
    linewidth = 0.35
  ) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 2
  ) +
  scale_color_manual(
    values = c(
      "WHO"  = "#2166ac",
      "PAHO" = "#b2182b"
    ),
    name = NULL
  ) +
  facet_wrap(~od_region, scales = "free_y") +
  labs(
    x = paste0(
      "Reporting factor (final cases / case reported at delay; d < ",
      max_delay_months_rf_summary, " months)"
    ),
    y = NULL
  ) +
  theme_cowplot() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 9)
  )



# --------------------------------- SEARO ------------------------------------------------------------------
##---- import data set ----

searo <- download_and_standardise("SEARO")
searo_raw <- searo
searo <- searo %>%
  mutate(iso3 = iso3c)
searo$ISO_A0 <- searo$iso3
searo <- add_od_regions(searo)

searo <- searo %>%
  select(iso3, country, s, t, tr, total_den, od_region)

##---- Split into reporting and final data ----

validation_date <- max(searo$tr)
max_reporting_date <- validation_date - years(1)

v_searo <- searo %>%
  filter(tr == validation_date) %>%
  mutate(total_den_F = total_den) %>%
  select(-c(total_den, tr))

r_searo <- searo %>%
  filter(tr < max_reporting_date)

if (nrow(r_searo) == 0) {
  stop(
    "SEARO: no reporting snapshots (tr < ", max_reporting_date,
    "). Cannot compare partial and final counts; analysis stopped."
  )
}

## ---- calculate delay ----

r_searo <- r_searo %>%
  mutate(
    d = as.integer(round(
      as.numeric(tr - make_date(year = s, month = t, day = 1)) / 30.44
    )),
    d_scale = "month"
  )

## ---- join partial and final counts ----

d_searo <- r_searo %>%
  left_join(v_searo, by = c("iso3", "country", "od_region", "s", "t"))

## ---- classify zero / missing pairs and derived metrics ----

d_searo <- d_searo %>%
  filter(d <= max_delay_months_completeness) %>%
  mutate(
    zero_class = case_when(
      is.na(total_den) | is.na(total_den_F)      ~ "missing",
      total_den == 0 & total_den_F == 0          ~ "both_zero",
      total_den == 0 & total_den_F > 0           ~ "zero_to_positive",
      total_den > 0 & total_den_F == 0           ~ "positive_to_zero",
      TRUE                                       ~ "both_positive"
    ),
    case_class = case_when(
      total_den_F > 5                            ~ ">5",
      total_den_F <= 5 & total_den_F > 0         ~ "<=5",
      total_den_F == 0                           ~ "zero"
    ),
    final_positive = !is.na(total_den_F) & total_den_F > 0,
    partial_positive = !is.na(total_den) & total_den > 0,
    case_diff = total_den_F - total_den,
    diff_ratio = if_else(total_den_F > 0, case_diff / total_den_F, NA_real_),
    case_complete = if_else(total_den_F > 0, total_den / total_den_F, NA_real_),
    rf = if_else(zero_class == "both_positive", total_den_F / total_den, NA_real_)
  )

## ---- summary tables (stored as objects) ----

searo_zero_summary <- d_searo %>%
  count(zero_class, name = "n") %>%
  mutate(pct = 100 * n / sum(n))

searo_zero_by_country <- d_searo %>%
  count(iso3, country, od_region, zero_class, name = "n") %>%
  group_by(iso3, country, od_region) %>%
  mutate(pct_within_delay = 100 * n / sum(n)) %>%
  ungroup()

searo_detection_summary <- d_searo %>%
  group_by(iso3, country, od_region, d) %>%
  summarise(
    n_months = n(),
    n_both_zero = sum(zero_class == "both_zero"),
    n_zero_to_positive = sum(zero_class == "zero_to_positive"),
    n_positive_to_zero = sum(zero_class == "positive_to_zero"),
    n_both_positive = sum(zero_class == "both_positive"),
    n_missing = sum(zero_class == "missing"),
    pct_both_zero = 100 * mean(zero_class == "both_zero"),
    pct_zero_to_positive = 100 * mean(zero_class == "zero_to_positive"),
    pct_final_positive = 100 * mean(final_positive),
    detection_rate = if_else(
      sum(final_positive) > 0,
      100 * sum(partial_positive & final_positive) / sum(final_positive),
      NA_real_
    ),
    .groups = "drop"
  )

# Part B: RF on both_positive rows (RF summary delay window), three case strata

searo_rf_summary_country <- bind_rows(
  d_searo %>%
    filter(zero_class == "both_positive", d < max_delay_months_rf_summary) %>%
    group_by(iso3, country, od_region) %>%
    summarise(
      rf_stratum = "all_both_positive",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    ),
  d_searo %>%
    filter(zero_class == "both_positive", d < max_delay_months_rf_summary, total_den_F > 5) %>%
    group_by(iso3, country, od_region) %>%
    summarise(
      rf_stratum = "final_gte_5",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    ),
  d_searo %>%
    filter(zero_class == "both_positive", d < max_delay_months_rf_summary, total_den_F <= 5) %>%
    group_by(iso3, country, od_region) %>%
    summarise(
      rf_stratum = "final_lte_5",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    )
)

# SEARO-wide regional summary (equivalent to Global in WHO)
searo_rf_summary_region <- bind_rows(
  d_searo %>%
    filter(zero_class == "both_positive", d < max_delay_months_rf_summary) %>%
    summarise(
      rf_stratum = "all_both_positive",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    ),
  d_searo %>%
    filter(zero_class == "both_positive", d < max_delay_months_rf_summary, total_den_F > 5) %>%
    summarise(
      rf_stratum = "final_gte_5",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    ),
  d_searo %>%
    filter(zero_class == "both_positive", d < max_delay_months_rf_summary, total_den_F <= 5) %>%
    summarise(
      rf_stratum = "final_lte_5",
      u_rf = mean(rf, na.rm = TRUE),
      med_rf = median(rf, na.rm = TRUE),
      min_rf = min(rf, na.rm = TRUE),
      max_rf = max(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      n_rf = n(),
      .groups = "drop"
    )
)

## ---- figures (stored as objects) ----

# Forest plot: SEARO regional estimate plus one row per country
searo_rf_forest_data <- bind_rows(
  searo_rf_summary_region %>% mutate(country = "SEARO"),
  searo_rf_summary_country %>% select(country, rf_stratum, u_rf, med_rf, min_rf, max_rf, sd_rf, n_rf)
) %>%
  mutate(
    rf_stratum = factor(
      rf_stratum,
      levels = c("all_both_positive", "final_gte_5", "final_lte_5"),
      labels = c("All both positive", "Final > 5 cases", "Final <= 5 cases")
    ),
    rf_lo = u_rf - sd_rf,
    rf_hi = u_rf + sd_rf
  ) %>%
  filter(!is.na(u_rf)) %>%
  mutate(
    country = factor(
      country,
      levels = c("SEARO", sort(setdiff(unique(country), "SEARO")))
    )
  )

searo_rf_forest_plot <- searo_rf_forest_data %>%
  mutate(
    y_base = as.numeric(country),
    y_plot = y_base + case_when(
      rf_stratum == "Final > 5 cases"  ~  0.25,
      rf_stratum == "Final <= 5 cases" ~  0.00,
      TRUE                             ~ -0.25
    )
  ) %>%
  ggplot(aes(x = u_rf, y = y_plot, color = rf_stratum)) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.3, colour = "grey40") +
  geom_errorbarh(
    aes(xmin = rf_lo, xmax = rf_hi),
    height = 0.08,
    linewidth = 0.4
  ) +
  geom_point(size = 2) +
  scale_y_continuous(
    breaks = sort(unique(as.numeric(searo_rf_forest_data$country))),
    labels = levels(searo_rf_forest_data$country)
  ) +
  scale_color_manual(
    values = c(
      "All both positive" = "grey30",
      "Final > 5 cases"   = "#2166ac",
      "Final <= 5 cases"  = "#b2182b"
    ),
    name = "Case stratum"
  ) +
  labs(
    x = "Reporting factor (final / partial)",
    y = NULL,
    color = "Case stratum"
  ) +
  theme_cowplot() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.35, "cm"),
    axis.text.y = element_text(size = 8)
  )
