#' ---
#' title: "EVALUATION: 02_PAHO_monthly_cases_and_source_selection"
#' author: "K M Susong"
#' 
#' ---
#'
#' Overview
#' ========
#' 

# ----- Evaluate the PAHO correction and monthly calulation method ------ #

# # apply backfilling and define monthly cumulative cases 
# paho_monthly_cumm <- PAHO_cumm_monthly(paho)
# 
# # Calculate monthly cases:
# paho_month <- PAHO_incid_monthly(paho_monthly_cumm)


# check results:

country_error_df <- paho_month %>%
  filter(computed_monthly_cases < 0) %>%
  select(
    country,
    date,
    year,
    month,
    computed_monthly_cases,
    missing_reason
  ) %>%
  filter( year > 2023)
# %>%
#   select(
#     country
#   ) %>%
#   unique() %>%
#   as.vector()

country_error_df_cumm <- paho_monthly_cumm %>%
  filter(country == country_error$country)

country_error_df_month <- paho_month %>%
  filter(country == country_error$country)

country_error_df_cumm %>%
  filter( year >2023) %>%
  ggplot( aes(x =onset_date , y= total_monthly_cases, fill = country) )+
  geom_col() +
  facet_wrap(~country, scales = "free")


country_error_df_paho %>%
  filter( year == 2024) %>%
  ggplot( aes(x =EW , y= total_den, fill = country) )+
  geom_col() +
  facet_wrap(~country, scales = "free")


# ----- Evaluate the PAHO weekly calcualtions ( using PAHO_incid_weekly(paho) ) ------ #
# Filter for any negative or missing weekly case counts
issues <- paho_week %>%
  filter(is.na(computed_weekly_cases) | computed_weekly_cases < 0) %>%
  filter( year > 2023)

print(issues)

missing_weeks <- paho_week %>%
  group_by(country, year) %>%
  complete(EW = full_seq(EW, 1)) %>%
  filter(is.na(computed_weekly_cases)) %>%
  filter(year > 2023)

print(missing_weeks)

paho_week %>%
  filter( year == 2024) %>%
  ggplot( aes(x = EW, y = computed_weekly_cases)) +
  geom_col() +
  facet_wrap(~ country, scales = "free_y") +
  labs(title = "Weekly Computed Cases by Country-Year", y = "Weekly Cases", x = "Epi Week")



# ----- Evaluate the new ungroups fucntions for corrections and monthly calculation 
