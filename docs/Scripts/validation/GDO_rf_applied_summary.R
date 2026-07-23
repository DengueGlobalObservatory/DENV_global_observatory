#' ---
#' title: "03 GDO reporting delay correction analysis"
#' author: "K. M. Susong"
#' date:  "`r format(Sys.Date(), '%B %d, %Y')`"
#' ---

#' Overview: 
#' =========
#' 
#' in June-2026 reporting delay corrections were applied to the GDO for WHO and 
#' PAHO sources. 
#' 
#' Analysis of this corretion was complete for the GDO paper 
#' 
#' Timeline
#' ========
#' 
#' **16-June-2026:** Inital analysis using data from 10-June-2026
#' 


backfill_df <- read_csv("Output/2026_06_11_V2/DENV_cases_backfill_output.csv")
backfill_df$ISO_A0 <- backfill_df$iso3
backfill_df <- add_od_regions(backfill_df)
# WHO uses non-standard iso3 MDR for Autonomous Region of Madeira (see 01_select_historic_data.R).
# countrycode cannot map MDR, so add_od_regions assigns "Other"; override name and region here.
backfill_df <- backfill_df %>%
  mutate(
    country = if_else(iso3 == "MDR", "Autonomous Region of Madeira", country),
    od_region = if_else(
      iso3 == "MDR",
      get_od_regions("PRT")$od_region[1],
      od_region
    )
  )

# Total cases added to-date due to correction
cases_summary_global_2026 <- backfill_df %>%
  filter(Year == 2026) %>%
  group_by(Year) %>%
  reframe(
    raw       = sum(raw_cases, na.rm = T),
    corrected = sum(corrected_cases, na.rm = T), 
    applied   = sum(cases, na.rm = T),
    diff      = applied - raw
  )

# Country-Months with corrections
bf_applied_df <- backfill_df %>%
  filter(Year == 2026) %>%
  filter(correction_applied == T) %>%
  filter(rf != 1)
# 127 country months (30.6%)

bf_applied_df %>%
  distinct(country)
# 46 countries (55.4%)

bf_applied_df %>%
  distinct(od_region)
# 8 (100%)


bf_applied_df %>%
  summarise(
    mean_applied_rf = mean(rf, na.rm  = T),
    sd_applied_rf = sd(rf, na.rm = T), 
    n = n()
  )


bf_applied_df %>%
  filter(rf < 1.1) %>%
  filter(rf > 0.9)
# error of 10% or less -- 80 (62.9%)

bf_applied_df %>%
  filter(rf >= 1.1 & rf < 1.5 | rf > 0.5 & rf < 0.9) 
# error of 10-50% -- 42 (33%)

t <- bf_applied_df %>%
  filter(rf >= 1.5 | rf < 0.5 ) 
# error of 50%+  -- 5 (3.9%)