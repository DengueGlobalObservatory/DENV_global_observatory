#' ---
#' title: "00 Which_years_to_keep_between_WHO_and_OpenDengue_fun"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#'
#' Combine coverage dfs from WHO and OpenDengue. Identify which year to keep. 
#' 
#' Timeline:
#' =========
#' 14-05-2025: Prepared function.
#'

Which_years_to_keep_between_WHO_and_OpenDengue_fun <- function(OpenDengue_coverage_df, WHO_coverage_df){
  
  Comparing_coverage_in_both_countries <- full_join(OpenDengue_coverage_df, WHO_coverage_df, 
                                                    by = c("Year", "iso3"), suffix = c(".OD", ".WHO"))
  
  Comparing_coverage_in_both_countries_res <- Comparing_coverage_in_both_countries %>% 
    mutate(Which_to_keep = case_when(is.na(Counts_by_year) ~ "OpenDengue",
                                     is.na(National_extract_counts) ~ "WHO",
                                     National_extract_counts == 51 & Counts_by_year == 12 ~ "Either",
                                     National_extract_counts == 52 & Counts_by_year == 12 ~ "Either",
                                     National_extract_counts == 53 & Counts_by_year == 12 ~ "Either",
                                     TRUE ~ NA)) %>%
    mutate(OD_National_extract_counts_monthly_T_res = case_when(T_res == "Month" ~ National_extract_counts,
                                                                T_res == "Week" ~ National_extract_counts*(12/52))) %>%
    mutate(Which_to_keep = case_when(is.na(Which_to_keep) & OD_National_extract_counts_monthly_T_res > Counts_by_year ~ "OpenDengue",
                                     is.na(Which_to_keep) & OD_National_extract_counts_monthly_T_res < Counts_by_year ~ "WHO",
                                     is.na(Which_to_keep) & OD_National_extract_counts_monthly_T_res == Counts_by_year ~ "Either",
                                     TRUE ~ Which_to_keep)) 
  
  Comparing_coverage_in_both_countries_res_clean <- Comparing_coverage_in_both_countries_res %>%
    group_by(iso3) %>%
    mutate(OpenDengue_count = sum(Which_to_keep == "OpenDengue", na.rm = TRUE),
           WHO_counts = sum(Which_to_keep == "WHO", na.rm = TRUE)) %>%
    mutate(Which_to_keep_clean = case_when(Which_to_keep == "Either" & OpenDengue_count > WHO_counts ~ "OpenDengue",
                                           Which_to_keep == "Either" & OpenDengue_count < WHO_counts ~ "WHO",
                                           Which_to_keep == "Either" & OpenDengue_count == WHO_counts ~ "WHO",
                                           TRUE ~ Which_to_keep)) 
  
  return(Comparing_coverage_in_both_countries_res_clean)
}