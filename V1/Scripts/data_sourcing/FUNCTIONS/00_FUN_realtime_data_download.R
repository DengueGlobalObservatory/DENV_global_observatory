#' ---
#' title: "00_FUN_realtime_data_download"
#' author: "K M Susong"
#' 
#' ---
#' 
#' This script provides utility functions to ingest, clean, normalize, and compile PAHO dengue surveillance data.
#' Functions are grouped into PAHO and WHO components for extensibility.
#' 
#' Included functions 
#' ==================
#' 
#' **PAHO crawler**
#' 
#' need to write - API function 
#' 
#'  remove_total()             = Removes total summary rows from ID/country/serotype
#'  read_and_clean_PAHO()      = Load files, clean columns, convert types
#'  normalize_country()        = add english county name and iso3
#'  compile_PAHO()             = download, check and compile a week worth of download
#'  
#' **WHO crawler**
#' 
#' need to write - API function 
#'  
#'  **SEARO crawler**
#'  
#' need to write - API function 
#' 

# Required Libraries
library(dplyr)        # Data manipulatio
library(plyr)         # Counting, ordering (used in encode_fix)
library(lubridate)    # Date parsing and manipulation
library(data.table)   # Fast data.frame operations and rbindlist
library(readr)        # Writing CSVs with write_csv
library(stringr)      # String matching and cleaning
library(tibble)       # Creating tibbles for structured data
library(countrycode)  # add ISO3 codes


#' ───────────────────────────────────────────────────────────────
#' PAHO dengue dashboard functions
#' ───────────────────────────────────────────────────────────────



#' **remove_total()**
#' Removes total summary rows from ID/country/serotype
#' 
#' @param df dataframe from PAHO dengue dashboard

remove_total <- function(df) {
  df %>%
    filter(
      !ID == "Grand Total"
    )
}




#' **read_and_clean_PAHO()** 
#' Load files, clean columns, convert types
#'
#' @param file_path file path to PAHO dengue dashboard data 
#' @param download_week the date of download for files of interest 
#' 

read_and_clean_PAHO <- function(file_path, download_week) {
  tryCatch({
  # open file (tab seperated, UTF-16LE)
  df <- read_tsv(file_path, locale = locale(encoding = "UTF-16LE"))
  # remove sum row
  df <- remove_total(df)
  # remove extra year col
  df <- df %>%
    select(!Año...5)
  
  # Define column name mappings
  col_map <- list(
    # English → standard
    ID = "ID",
    Country.or.Subregion = "country",
    Serotype = "serotype",
    Year = "year",
    In...Out.of.Subregions = "in_out",
    `Epi..Week..a.` = "EW",
    `Total.of.Dengue.Cases..b.` = "total_den",
    `Incidence.Rate..c.` = "incidence",
    Laboratory.Confirmed = "lab_confirmed",
    `X..Lab.Conf..x100.` = "percent_Lconf",
    `Severe.Dengue..d.` = "sev_den",
    `X.SD.D..x100..e.` = "SD_D",
    Deaths = "deaths",
    `CFR..f.` = "CFR",
    `Population.X.1000.` = "pop",
    
    # Spanish → standard
    `País.o.Subregion` = "country",
    `Serotipo` = "serotype",
    `Año...4` = "year",
    `In...Out.of.Subregions` = "in_out",
    `Semana.Epidemiológica..a.` = "EW",
    `Total.de.Casos.de.Dengue..b.` = "total_den",
    `Tasa.de.Incidencia..c.` = "incidence",
    `Confirmados.Laboratorio` = "lab_confirmed",
    `%..Lab.Conf..x100.` = "percent_Lconf",
    `Dengue.Grave..d.` = "sev_den",
    `(DG.D).x100..e.` = "SD_D",
    `Muertes` = "deaths",
    `Letalidad..f.` = "CFR",
    `Población.X.1000.` = "pop"
  )
  
  # Flatten to named character vector for rename_with
  col_map_flat <- setNames(unname(unlist(col_map)), names(col_map))
  
  # Normalize names
  names(df) <- make.names(names(df))
  # confirm that file col names match expected col names 
  matched_cols <- intersect(names(col_map_flat), names(df))
  if (length(matched_cols) == 0) stop("❌ No expected columns matched in: ", basename(file_path))
  
  
  # Rename using mapping
  df <- df %>%
    rename_with(~ col_map_flat[.x], .cols = matched_cols) %>%
    # str update
    mutate(
      ext_date = download_week,
      ID = as.factor(ID)
    ) %>%
    # remove rows where EW == NA
    filter(!is.na(EW))
  return(df)
  }, error = function(e) {
    message("❌ Failed to read/clean: ", basename(file_path), " → ", e$message)
    return(NULL)
  })
}

#' **normalize_country()**
#'
#' This function standardizes Spanish country names to English and appends ISO3 codes.
#'
#' @param df A data frame containing a country column in Spanish
#' @param col The name of the country column to normalize (default = "country")
#'
#' @return A data frame with columns `country_sp`, `country` (English), and `iso3` (ISO3 code)


normalize_country <- function(df, col = "country") {
  # Spanish → English mapping
  country_map <- c(
    "Canadá" = "Canada",
    "Estados Unidos de América" = "United States",
    "Belice" = "Belize",
    "Costa Rica" = "Costa Rica",
    "El Salvador" = "El Salvador",
    "Guatemala" = "Guatemala",
    "Honduras" = "Honduras",
    "México" = "Mexico",
    "Nicaragua" = "Nicaragua",
    "Panamá" = "Panama",
    "Bolivia" = "Bolivia",
    "Colombia" = "Colombia",
    "Ecuador" = "Ecuador",
    "Perú" = "Peru",
    "Venezuela" = "Venezuela",
    "Argentina" = "Argentina",
    "Brasil" = "Brazil",
    "Chile" = "Chile",
    "Paraguay" = "Paraguay",
    "Uruguay" = "Uruguay",
    "República Dominicana" = "Dominican Republic",
    "Puerto Rico" = "Puerto Rico",
    "Anguila" = "Anguilla",
    "Antigua y Barbuda" = "Antigua and Barbuda",
    "Aruba" = "Aruba",
    "Bahamas" = "Bahamas",
    "Barbados" = "Barbados",
    "Bermuda" = "Bermuda",
    "Bonaire, San Eustaquio y Saba" = "Bonaire, Sint Eustatius and Saba",
    "Curazao" = "Curaçao",
    "Granada" = "Grenada",
    "Guadalupe" = "Guadeloupe",
    "Guyana" = "Guyana",
    "Islas Caimán" = "Cayman Islands",
    "Isla de San Martín (Francia)" = "Saint Martin (French part)",
    "Isla de San Martín (Holanda)" = "Sint Maarten (Dutch part)",
    "Islas Turcas y Caicos" = "Turks and Caicos Islands",
    "Islas Vírgenes (EUA)" = "United States Virgin Islands",
    "Islas Vírgenes (RU)" = "British Virgin Islands",
    "Jamaica" = "Jamaica",
    "Martinica" = "Martinique",
    "Montserrat" = "Montserrat",
    "Saint Kitts y Nevis" = "Saint Kitts and Nevis",
    "San Bartolomé" = "Saint Barthelemy",
    "San Vicente y las Granadinas" = "Saint Vincent and the Grenadines",
    "Santa Lucía" = "Saint Lucia",
    "Suriname" = "Suriname",
    "Trinidad y Tobago" = "Trinidad and Tobago",
    "Guayana Francesa" = "French Guiana",
    "Cuba" = "Cuba",
    "Dominica" = "Dominica"
  )
  
  # Replace country names
  df$country_sp <- df$country
  df$country <- dplyr::recode(df[[col]], !!!country_map)
  
  # Add ISO3 code (using countrycode)
  df$iso3 <- countrycode(df$country, origin = "country.name", destination = "iso3c")
  
  # Optional: Warn about any unmatched ISO codes
  if (any(is.na(df$iso3c))) {
    warn_countries <- unique(df$country[is.na(df$iso3c)])
    warning("⚠️ Some country names could not be matched to ISO3:\n  - ", paste(warn_countries, collapse = ", "))
  }
  
  return(df)
}



#' **compile_PAHO()**
#'
#' This function compiles a week's worth of encoded PAHO dengue data into a single cleaned CSV.
#'
#' @param dir_input Path to encoded input directory
#' @param sub_dirs_ls Subdirectory name (relative path)
#' @param download_week Download date (e.g., "20240601")
#' @param final_dir Output directory for cleaned CSV
#'
#' @return NULL (writes file to disk)

dir_input <- 


compile_PAHO <- function(dir_input, sub_dirs_ls, download_week, final_dir) {
  if (!dir.exists(dir_input)) stop("❌ dir_input does not exist.")
  if (!dir.exists(final_dir)) dir.create(final_dir, recursive = TRUE)
  
  # Get all file paths from each subdirectory
  all_files <- unlist(lapply(sub_dirs_ls, function(subdir) {
    list.files(file.path(dir_input, subdir), full.names = TRUE)
  }))
  
  if (length(all_files) == 0) {
    message("⚠️ No files found in the specified directories.")
    return(NULL)
  }
  
  df_list <- list()
  for (f in all_files) {
    cat("📄 Processing:", basename(f), "\n")
    result <- tryCatch({
      read_and_clean_PAHO(f,download_week)
    }, error = function(e) {
      message("❌ Error in file ", basename(f), ": ", e$message)
      return(NULL)
    })
    if (!is.null(result)) df_list <- append(df_list, list(result))
  }
  
  if (length(df_list) == 0) {
    message("❗ No valid data could be read and cleaned.")
    return(NULL)
  }
  
  final_df <- bind_rows(df_list) %>%
    mutate(Reporting_EW = epiweek(ymd(ext_date)))
  final_df <- normalize_country(final_df)
  
  output_path <- file.path(final_dir, sprintf("PAHO_week_%s.csv", download_week))
  write_csv(final_df, output_path)
  message("✅ Compilation complete. File saved to: ", output_path)
}
