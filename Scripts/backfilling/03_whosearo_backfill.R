#' ---------------------------------------------------------------------------
#' 02_dengue_rf.R
#' ---------------------------------------------------------------------------
#' 
#' 

library(lubridate)

# ---- WHO----
##---- import data set ----

who <- download_and_standardise("WHO")

##---- Review data -----

str(who)

# select required data columns 

who <- who %>%
  dplyr::select(iso3,country,s,t,tr,total_den, who_region, date)
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
  left_join(v_who) %>%
  filter( d < 36)

## ----- calculate impact of delay on reporting (at each observation) ----

d_who <- d_who %>%
  mutate(
    case_diff = total_den_F - total_den,
    diff_ratio = case_diff / total_den_F,
    case_complete = total_den / total_den_F,
    rf = total_den_F/total_den
  )


## --- view inital trend in delays ----

d_who %>%
  ggplot(aes(x =d, y = rf, colour = country, linetype =  who_region, group = date))+
  geom_line() +
  theme(
    legend.position = "blank")+
  geom_hline(yintercept = 1) +
  ylim(0,10)

## --- calculated empirical rf with uncertainty ----

# ---- SEARO----


searo<- download_and_standardise("SEARO")

str(searo)

searo <- searo %>%
  mutate(
    iso3 = iso3c
  ) %>%
  dplyr::select(iso3,country,s,t,tr,total_den) %>%
  # note that s = year, t = month , R_t (need to change) is reporting time 
  # i also need to add a real date col for the t
  mutate(
    date = make_date(year = s, month = t, day = 1)
  )

# this is the most recent version of the data 
validation_date <- max(searo$R_t)
# for the reportig data only data from 1 year prior can be used 
max_reporting_date <- validation_date- years(1) 

v_searo <- searo %>%
  filter( R_t == validation_date)
