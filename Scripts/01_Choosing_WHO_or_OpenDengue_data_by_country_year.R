#' ---
#' title: "01 Choosing WHO or OpenDengue data"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
#' Compare data coverage by year between WHO and OpenDengue datasets. Select years from source with higher coverage.
#' 
#' Timeline:
#' =========
#' 14-05-2025: Listed countries unique to each source, and those included in both. 
#'             For those in both, missing data up to four weeks or one month was interpolated and coverage compared. 
#' 19-06-2025: Corrected sourced functions filepath.

library(dplyr)
library(tidyverse)
library(readr)
library(countrycode)
library(readxl)
library(imputeTS)

#--------------- Loading data
OD_national_extract <- read_csv("Data/OpenDengue_V1_2/National_extract_V1_2.csv")
WHO_data <- read_excel("Data/WHO_dengue_data/dengue-global-data-2025-05-14.xlsx")

#--------------- Cleaning data 
source("Scripts/Functions/00_FUN_WHO_data_cleaning.R", local = environment())
WHO_data_clean <- WHO_data_cleaning(WHO_data)

#---------------------- Listing countries unique to one source + in both - national 

# Filter out reporting
OD_national_extract_filtered <- OD_national_extract %>%
  filter(T_res != "Year")

# Countries only in OpenDengue 
Countries_only_in_OpenDengue_national_extract <- OD_national_extract_filtered$adm_0_name[!OD_national_extract_filtered$ISO_A0 %in% WHO_data_clean$iso3] %>% 
  unique()

# Countries only in WHO 
Countries_only_in_WHO_national_S_res <- WHO_data_clean$country[!WHO_data_clean$iso3 %in% OD_national_extract_filtered$ISO_A0] %>% 
  unique()

# Countries in both 
Countries_in_both_OpenDengue_and_WHO <- OD_national_extract_filtered$adm_0_name[OD_national_extract_filtered$ISO_A0 %in% WHO_data_clean$iso3] %>% 
  unique()
Countries_in_both_df <- data.frame(Country = Countries_in_both_OpenDengue_and_WHO, 
                                   iso3 = countrycode(Countries_in_both_OpenDengue_and_WHO, "country.name", "iso3c")) %>% 
  mutate(iso3 = case_when(Country == "SAINT MARTIN" ~ "MAF",
                          TRUE ~ iso3))

#' Countries in both WHO and OpenDengue databases:
#' AFGHANISTAN, ANGUILLA, ANTIGUA AND BARBUDA, ARGENTINA, ARUBA, AUSTRALIA, BAHAMAS, BANGLADESH, BARBADOS, BELIZE, BERMUDA, BOLIVIA, BONAIRE, SAINT EUSTATIUS AND SABA, 
#' BRAZIL, CAMBODIA, CAYMAN ISLANDS, CHILE, CHINA, COLOMBIA, COOK ISLANDS, COSTA RICA, CUBA, CURACAO, DOMINICA, DOMINICAN REPUBLIC, ECUADOR, EL SALVADOR, FIJI, 
#' FRENCH GUIANA, FRENCH POLYNESIA, GRENADA, GUADELOUPE, GUATEMALA, GUYANA, HAITI, HONDURAS, JAMAICA, KIRIBATI, LAO PEOPLE'S DEMOCRATIC REPUBLIC, MALAYSIA, MALDIVES, 
#' MARSHALL ISLANDS, MARTINIQUE, MEXICO, MICRONESIA (FEDERATED STATES OF), MONTSERRAT, NEW CALEDONIA, NICARAGUA, NIUE, NORTHERN MARIANA ISLANDS, PAKISTAN, PALAU, PANAMA, 
#' PARAGUAY, PERU, PITCAIRN, PUERTO RICO, SAINT BARTHELEMY, SAINT KITTS AND NEVIS, SAINT LUCIA, SAINT MARTIN, SAINT VINCENT AND THE GRENADINES, SAMOA, SAUDI ARABIA, 
#' SINGAPORE, SINT MAARTEN, SOLOMON ISLANDS, SRI LANKA, SUDAN, SURINAME, THAILAND, TOKELAU, TONGA, TRINIDAD AND TOBAGO, TURKS AND CAICOS ISLANDS, TUVALU, 
#' UNITED STATES OF AMERICA, URUGUAY, VANUATU, VENEZUELA, VIET NAM, VIRGIN ISLANDS (UK), VIRGIN ISLANDS (US), WALLIS AND FUTUNA

#' Countries with data in just OpenDengue 
#' AMERICAN SAMOA, BRUNEI DARUSSALAM, GUAM, HONG KONG, INDONESIA, NAURU, PHILIPPINES, TAIWAN, YEMEN

#' Countries with data in just WHO 
#' Canada, Nepal, Côte d'Ivoire, Mauritania, Seychelles, Ethiopia, Sao Tome and Principe, Togo, Cabo Verde, Mauritius, Angola, Benin, Burkina Faso, Burundi, Cameroon, 
#' Central African Republic, Chad, Congo, Eritrea, Gabon, Gambia, Ghana, Guinea, Guinea-Bissau, Kenya, Liberia, Madagascar, Malawi, Mali, Mozambique, Namibia, Niger, 
#' Rwanda, Senegal, Sierra Leone, South Sudan, Uganda, United Republic of Tanzania, Zambia, Zimbabwe, Albania, Andorra, Armenia, Austria, Azerbaijan, Belarus, Belgium, 
#' Bosnia and Herzegovina, Bulgaria, Croatia, Cyprus, Czechia, Denmark, Estonia, Finland, France, Georgia, Germany, Greece, Holy See, Hungary, Iceland, Ireland, Israel, 
#' Italy, Kazakhstan, Kyrgyzstan, Latvia, Liechtenstein, Lithuania, Luxembourg, Malta, Mayotte, Monaco, Montenegro, Netherlands (Kingdom of the), North Macedonia, Norway, 
#' Poland, Portugal, Republic of Moldova, Romania, Russian Federation, Réunion, San Marino, Serbia, Slovakia, Slovenia, Spain, Sweden, Switzerland, Tajikistan, Turkmenistan, 
#' Türkiye, Ukraine, United Kingdom of Great Britain and Northern Ireland, Uzbekistan, India, Myanmar, Equatorial Guinea, Botswana 

#---------------------- Data coverage for countries in both WHO and OpenDengue databases - OpenDengue

# Extract data for countries in both
OD_data_for_countries_in_both <- OD_national_extract_filtered %>% 
  filter(ISO_A0 %in% Countries_in_both_df$iso3)

#----- Identify highest continuity data source by year + where duplicate counts exist select the source with the highest continuity. 
source("Scripts/Functions/00_FUN_deduplicating_OpenDengue_data.R", local = environment())
OD_data_for_countries_in_both_clean <- deduplicating_OpenDengue_data_FUN(OD_data_for_countries_in_both)

#----- Interpolating missing OpenDengue data
source("Scripts/Functions/00_FUN_interpolating_missing_OpenDengue_data.R", local = environment())
OD_data_for_countries_in_both_clean_split <- split(OD_data_for_countries_in_both_clean, OD_data_for_countries_in_both_clean$ISO_A0)
OD_data_for_countries_in_both_interpolated_split <- Map(interpolating_missing_OpenDengue_data_FUN, OD_data_for_countries_in_both_clean_split)

#----- Data coverage of OpenDengue countries in both
source("Scripts/Functions/00_FUN_OpenDengue_interpolated_cases_data_coverage.R", local = environment())
OD_countries_in_both_coverage <- Map(OpenDengue_interpolated_cases_data_coverage_FUN, OD_data_for_countries_in_both_interpolated_split)
OD_countries_in_both_coverage_df <- Reduce(rbind, OD_countries_in_both_coverage) %>% as.data.frame() %>%
  mutate(iso3 = countrycode(adm_0_name, "country.name", "iso3c")) %>% 
  mutate(iso3 = case_when(adm_0_name == "SAINT MARTIN" ~ "MAF",
                          TRUE ~ iso3))

#---------------------- Data coverage for countries in both WHO and OpenDengue databases - WHO

# Extract data for countries in both
WHO_data_for_countries_in_both <- WHO_data_clean %>% 
  filter(iso3 %in% Countries_in_both_df$iso3)

#----- Interpolating missing WHO data
source("Scripts/Functions/00_FUN_interpolating_missing_WHO_data.R", local = environment())
WHO_data_for_countries_in_both_split <- split(WHO_data_for_countries_in_both, WHO_data_for_countries_in_both$iso3)
WHO_data_for_countries_in_both_interpolated_split <- Map(interpolating_missing_WHO_data_FUN, WHO_data_for_countries_in_both_split)

#----- Data coverage of WHO countries in both 
source("Scripts/Functions/00_FUN_WHO_interpolated_cases_data_coverage.R", local = environment())
WHO_data_for_countries_in_both_coverage <- Map(WHO_interpolated_cases_data_coverage_fun, WHO_data_for_countries_in_both_interpolated_split)
WHO_data_for_countries_in_both_coverage_df <- Reduce(rbind, WHO_data_for_countries_in_both_coverage) %>% as.data.frame() %>%
  mutate(iso3 = countrycode(adm_0_name, "country.name", "iso3c"))

#--------------------------- Comparing coverage between countries in both WHO and OD databases 
source("Scripts/Functions/00_FUN_Which_years_to_keep_between_WHO_and_OpenDengue.R", local = environment())
Comparing_coverage_between_WHO_and_OpenDengue <- Which_years_to_keep_between_WHO_and_OpenDengue_fun(OD_countries_in_both_coverage_df, 
                                                                                                    WHO_data_for_countries_in_both_coverage_df)
