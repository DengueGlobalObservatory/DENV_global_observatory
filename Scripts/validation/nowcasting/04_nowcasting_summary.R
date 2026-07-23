# Nowcasting summary values 

output_20260611 <- read.csv("Output/2026_06_11_V2/DENV_cases_nowcast_output.csv")

output_20260601 <- read.csv("Output/2026_06_01/DENV_cases_nowcast_output.csv")


output %>%
  filter(Year == 2026) %>%
  filter(source == "Estimates") %>%
  summarise(
    cases = sum(cases), 
    n = n()
  )


output_20260601 %>%
  filter(Year == 2026) %>%
  filter(source == "Estimates") %>%
  summarise(
    cases = sum(cases), 
    n = n()
  )
# estimated cases : 84403 (0.06)



output_20260601 %>%
  filter(Year == 2026) %>%
  summarise(
    cases = sum(cases, na.rm = T), 
    n = n()
  )
# total cases: 1377587


totals <- output_20260601 %>%
  filter(Year == 2026) %>%
  group_by(Region) %>%
  summarise(
    total_cases = sum(cases, na.rm = T), 
  )


combine  <- output_20260601 %>%
  filter(Year == 2026) %>%
  filter(source == "Estimates") %>%
  group_by(Region) %>%
  summarise(
    estimated_cases = sum(cases), 
  ) %>%
  left_join(totals) %>%
  mutate(
    percent_region_total = round(estimated_cases/ total_cases *100, 2),
    percent_estiamted_total = round(estimated_cases / 84403 * 100, 2),
    percent_of_all_cases= round(total_cases / 1377587 * 100, 2)
  )


34894/84403
