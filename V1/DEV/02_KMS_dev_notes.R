library(data.table)


m <- fread("id   season pa month sampled occupancy
           1  spring  1     3       1   present
           2  spring  1     4       1   present
           3  spring  1     5       1   present
           4  summer  1     6       1   present
           5  summer  1     7       1   present
           6  summer  1     8       1   present
           7  winter  0    12       1    absent
           8  winter  0     1       0    absent
           9  winter  0     2       0    absent
           10   fall  1     9       1   present
           11   fall  1    10       1   present
           12   fall  1    11       1   present")

# reshape from wide to long (as preferred by ggplot)
ml <- melt(m, measure.vars = c("pa", "sampled"))
# create factors to ensure desired order
ml[, variable := factor(variable, levels = c("pa", "sampled"))]


ml[, start := as.Date("2016-01-01") + base::months(month - 1L)]
# last day of month = begin of next month - 1 day
ml[, end := as.Date("2016-01-01") + base::months(month) - 1L]



ml_c <- ml %>% 
  filter(variable == "sampled") %>%
  arrange(start) %>%
  mutate(
    count = rnbinom(n = 12, size =3.284865e-01, mu = 36836.62500 ),
    count_next_month = lead(count, 1),
    count_next_month = ifelse(is.na(count_next_month), count, count_next_month)) %>%
  select(month, start, end, count,count_next_month)

ml_c_simple  <- ml %>% 
  filter(variable == "sampled") %>%
  arrange(start) %>%
  mutate(
    count = c(1,2,3,4,5,6,7,8,9,10,11,12),
    count_next_month = lead(count, 1),
    count_next_month = ifelse(is.na(count_next_month), count, count_next_month)) %>%
  select(month, start, end, count,count_next_month)



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


full_data_average_seasonTHA <- full_data_average_season %>%
  filter( Country == "Thailand")

monthly_data_filtered <- data_frame(
  Country = rep("Thailand", 12),
  iso3 = rep("THA", 12),
  Calendar_year_month = seq(1:12),
  season_nMonth = seq(1:12),
  
  cases = c(800,900,800,700,1200,2500,3000,3800, NA, NA, NA,NA),
  complete.cases = c(800,900,800,700,1200,2500,3000,3800, NA, NA, NA,NA),
  Data_status = c("observed","observed","observed","observed","observed","observed","observed","observed",NA, NA,NA,NA),
  
  Ave_season_monthly_cases = (full_data_average_seasonTHA$Ave_season_monthly_cases),
  Ave_season_monthly_cum_cases = full_data_average_seasonTHA$Ave_season_monthly_cum_cases,
  
  nb_mean =full_data_average_seasonTHA$nb_mean,
  nb_size = full_data_average_seasonTHA$nb_size,
  
  percentile_most_recent = rep(50,12)
  
  
)


monthly_data_filtered_plot_ready <- monthly_data_filtered %>%
mutate(
  start = make_date(2020, Calendar_year_month, 1),
  end = (make_date(2020, Calendar_year_month, 1) %m+% months(1)) - days(1), 
  cases_next_month = lead(cases, 1),
  cases_next_month = ifelse(is.na(cases_next_month), cases, cases_next_month),
  Ave_season_monthly_cases_next_month = lead(Ave_season_monthly_cases, 1),
  Ave_season_monthly_cases_next_month = ifelse(is.na(Ave_season_monthly_cases_next_month), Ave_season_monthly_cases, Ave_season_monthly_cases_next_month)

)
  

ggplot(monthly_data_filtered_plot_ready, aes(x = start, xend = end, y = Ave_season_monthly_cases, yend = Ave_season_monthly_cases_next_month )) + 
  geom_segment() +
  scale_y_discrete( breaks = NULL) +
  scale_x_date(date_breaks = "1 month", date_labels = month.abb,
               limits = range(c(monthly_data_filtered_plot_ready$start, monthly_data_filtered_plot_ready$end))) +
  # scale_size(guide = FALSE) +
  coord_polar() +
  theme_bw() + 
  xlab(NULL) + 
  ylab(NULL)


monthly_data_complete <- read.csv("/Users/lshks26/Dropbox/DMMG/DENV_dashboard/DENV_global_observatory/V1/DEV/monthly_data_complete_10092025.csv")


monthly_data_filtered <- monthly_data_complete %>% 
  filter( iso3 %in% "MTQ") %>%
  mutate(
    start = make_date(2020, Calendar_year_month, 1),
    end = (make_date(2020, Calendar_year_month, 1) %m+% months(1)) - days(1), 
    cases_next_month = lead(complete_cases, 1),
    cases_next_month = ifelse(is.na(cases_next_month), complete_cases, cases_next_month),
    Ave_season_monthly_cases_next_month = lead(Ave_season_monthly_cases, 1),
    Ave_season_monthly_cases_next_month = ifelse(is.na(Ave_season_monthly_cases_next_month),first(Ave_season_monthly_cases), Ave_season_monthly_cases_next_month)
  )    

percentile_color_value <- monthly_data_filtered %>%
  drop_na(percentile_most_recent ) %>%
  slice_tail() %>%
  select(percentile_most_recent)







####### Extract example data for 6 countries ########

ex_countries <- c("BRA", "THA", "MEX", "PRY", "AUS", "BLZ")

example_data_monthly <- monthly_data_complete %>%
  filter(iso3 %in% ex_countries)


write.csv(example_data_monthly,"~/Dropbox/DMMG/DENV_dashboard/Demo_materials/V1_monthly_data_example.csv" )
