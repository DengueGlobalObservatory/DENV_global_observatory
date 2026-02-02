### How to plot base ####

demo_data <- read.csv("Output/2025_12_02/DENV_cases_nowcast_output.csv")

#format
current_year <- as.integer(format(Sys.Date(), "%Y"))

# Use the system date to define "current" and "recent" months ---
current_month <- as.integer(format(Sys.Date(), "%m"))
recent_month <- current_month - 1
if (recent_month == 0) recent_month <- 12  # handle January wrap-around

# Correct case data to NA for dates after the current month 

demo_data <- demo_data %>%
  mutate(
    is_future = (Year > current_year) | (Year == current_year & Month > 8),
    cases = if_else(is_future, NA_real_, cases),
    cum_todate_cases_calendar = if_else(is_future, NA_real_, cum_todate_cases_calendar),
    cum_todate_cases_season   = if_else(is_future, NA_real_, cum_todate_cases_season)
  ) 
### select data for map 

# Create named list of all countries’ most recent data
demo_data_select <- demo_data %>%
  filter(Year == current_year) %>%
  filter(country == "Samoa")

make_radial_plot(demo_data_select)

ggsave("Assets/Stable/demo_plot.png", dpi = 500)
