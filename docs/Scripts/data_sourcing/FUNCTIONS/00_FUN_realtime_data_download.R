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
#'  normalize_country()        = add English county name and iso3
#'  compile_PAHO()             = download, check and compile a week worth of download
#'  compile_PAHO_github()      = download from github, check and compile a week worth of download  


# Required Libraries
library(dplyr)        # Data manipulation (filter, mutate, bind_rows, etc.)
library(plyr)         # Counting, ordering (legacy support for encode_fix)
library(lubridate)    # Date parsing and epidemiological week handling
library(data.table)   # Efficient data.frame operations (used in rbindlist)
library(readr)        # Reading CSVs / TSVs (with encoding control)
library(stringr)      # String cleaning and regex helpers
library(tibble)       # Creating tidy tibbles
library(countrycode)  # Add ISO3 country codes
library(httr)         # GitHub API requests and downloads
library(jsonlite)     # Parse JSON responses from GitHub API



#' ───────────────────────────────────────────────────────────────
#' GitHub API Helper Function
#' ───────────────────────────────────────────────────────────────

#' **github_api_request()**
#' Makes authenticated GitHub API requests with proper error handling
#' 
#' @param url GitHub API URL to request
#' @return httr response object
#' @details Automatically uses GITHUB_TOKEN from environment if available.
#'          Provides better error messages for rate limit issues.

github_api_request <- function(url) {
  # Get GitHub token from environment variable (optional but recommended)
  token <- Sys.getenv("GITHUB_TOKEN", unset = "")
  
  # Build headers - User-Agent is required by GitHub
  headers <- c("User-Agent" = "DENV-Observatory/1.0")
  if (nchar(token) > 0) {
    headers <- c(headers, "Authorization" = paste("token", token))
  }
  
  res <- GET(url, add_headers(.headers = headers))
  
  # Check for rate limit errors and provide helpful message
  if (status_code(res) == 403) {
    rate_limit_remaining <- headers(res)$`x-ratelimit-remaining`
    if (!is.null(rate_limit_remaining) && as.numeric(rate_limit_remaining) == 0) {
      reset_timestamp <- headers(res)$`x-ratelimit-reset`
      if (!is.null(reset_timestamp)) {
        reset_time <- as.POSIXct(
          as.numeric(reset_timestamp), 
          origin = "1970-01-01"
        )
        stop(sprintf(
          "GitHub API rate limit exceeded. Reset time: %s. Consider setting GITHUB_TOKEN environment variable for higher limits.",
          format(reset_time, "%Y-%m-%d %H:%M:%S")
        ))
      }
    }
  }
  
  stop_for_status(res)
  return(res)
}


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
    # open file (tab separated, UTF-16LE)
    df <- suppressMessages(
      read_tsv(file_path, locale = locale(encoding = "UTF-16LE")))
    
    # 🧩 ADD THIS FOR DEBUGGING (safe logging)
    # cat("[", Sys.time(), "] INFO | File:", basename(file_path), 
    #     "| Raw cols:", paste(names(df), collapse = ", "), "\n")
    
    # remove sum row
    df <- remove_total(df)
  # remove extra year col
  if ("Año...5" %in% names(df)) {
    df <- df %>% select(!Año...5)
  }
  
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
  if (any(is.na(df$iso3))) {
    warn_countries <- unique(df$country[is.na(df$iso3)])
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


#' **compile_PAHO_github()**
#'
#' This function retrieves the most recent PAHO dengue data directly from the
#' `PAHO-crawler` GitHub repository. It identifies the latest weekly download
#' folder (`DL_YYYYMMDD`), ingests all `.csv` files inside (which are UTF-16LE
#' tab-separated), cleans and normalizes them with `read_and_clean_PAHO()`,
#' and compiles into a single dataframe. 
#'
#' Unlike the original `compile_PAHO()`, this version does not require local
#' folders or saving intermediate files — it works entirely via the GitHub API.
#'
#' @param repo_owner GitHub repository owner (default = "DengueGlobalObservatory")
#' @param repo_name GitHub repository name (default = "PAHO-crawler")
#'
#' @return A tibble containing compiled and cleaned PAHO dengue data for the most recent week.
#' @examples
#' df <- compile_PAHO_github()
#' head(df)
#'
#' @details
#' Steps:
#' 1. Query GitHub API for the `data/` folder contents.
#' 2. Identify the latest `DL_YYYYMMDD` subfolder by date.
#' 3. List all `.csv` files inside the subfolder.
#' 4. Download and process each file with `read_and_clean_PAHO()`.
#' 5. Combine all cleaned files into a single dataframe.
#' 6. Normalize country names and ISO3 codes with `normalize_country()`.
#' 7. Add epidemiological week (`Reporting_EW`) based on download date.


compile_PAHO_github <- function(repo_owner = "DengueGlobalObservatory",
                                repo_name = "PAHO-crawler") {
  start_time <- Sys.time()
  log_message("Starting compile_PAHO_github()...")
  
  # 1️⃣ Get list of DL_* subfolders ------------------------------------------
  url <- sprintf("https://api.github.com/repos/%s/%s/contents/data", repo_owner, repo_name)
  log_message(paste("Querying GitHub API for data folders:", url))
  
  dirs <- tryCatch({
    res <- github_api_request(url)  # Changed from GET(url)
    fromJSON(content(res, "text"))
  }, error = function(e) {
    log_message(paste("GitHub API request failed:", conditionMessage(e)), level = "ERROR")
    return(NULL)
  })
  
  if (is.null(dirs)) stop("Failed to retrieve PAHO data directories from GitHub.")
  
  dl_dirs <- grep("^DL_[0-9]{8}$", dirs$name, value = TRUE)
  if (length(dl_dirs) == 0) {
    log_message("No DL_YYYYMMDD folders found in GitHub repo.", level = "ERROR")
    stop("No PAHO data folders found.")
  }
  
  latest_dir <- dl_dirs[which.max(as.Date(gsub("DL_", "", dl_dirs), "%Y%m%d"))]
  download_week <- gsub("DL_", "", latest_dir)
  log_message(paste("Latest available folder:", latest_dir))
  
  # 2️⃣ List files inside that folder ----------------------------------------
  url_sub <- sprintf("https://api.github.com/repos/%s/%s/contents/data/%s",
                     repo_owner, repo_name, latest_dir)

  files <- tryCatch({
    res_sub <- github_api_request(url_sub)  # Changed from GET(url_sub)
    fromJSON(content(res_sub, "text"))
  }, error = function(e) {
    log_message(paste("Error retrieving file list:", conditionMessage(e)), level = "ERROR")
    return(NULL)
  })
  
  if (is.null(files)) stop("Unable to list files for PAHO data folder.")
  
  csv_files <- grep("\\.csv$", files$name, value = TRUE)
  log_message(paste("Found", length(csv_files), "CSV files in latest folder."))
  
  if (length(csv_files) == 0) {
    log_message("No CSV files found in latest folder.", level = "ERROR")
    stop("No CSV files found in PAHO GitHub directory.")
  }
  
  # 3️⃣ Read, clean, and normalize each file ---------------------------------
  df_list <- list()
  for (f in csv_files) {
    file_url <- files[files$name == f, "download_url"]
    log_message(paste("Reading file:", f))
    tmp <- tempfile(fileext = ".csv")
    
    tryCatch({
      download.file(file_url, tmp, mode = "wb", quiet = TRUE)
      df <- read_and_clean_PAHO(tmp, download_week)
      
      if (!is.null(df) && is.data.frame(df)) {
        log_message(paste(f, "read successfully with", nrow(df), "rows."))
        df_list <- append(df_list, list(df))
      } else {
        log_message(paste(f, "returned NULL or invalid data."), level = "WARNING")
      }
    }, error = function(e) {
      log_message(paste("Error processing", f, ":", conditionMessage(e)), level = "ERROR")
    })
  }
  
  if (length(df_list) == 0) {
    log_message("No valid PAHO CSV files could be read and cleaned.", level = "ERROR")
    stop("No valid PAHO CSV files found or processed.")
  }
  
  # 4️⃣ Combine and normalize -------------------------------------------------
  final_df <- tryCatch({
    bind_rows(df_list) %>%
      mutate(Reporting_EW = epiweek(ymd(ext_date))) %>%
      normalize_country()
  }, error = function(e) {
    log_message(paste("Error during final data binding/normalization:", conditionMessage(e)), level = "ERROR")
    return(NULL)
  })
  
  if (is.null(final_df) || !is.data.frame(final_df)) {
    stop("Final PAHO dataset could not be created.")
  }
  
  # 5️⃣ Log summary and timing -----------------------------------------------
  end_time <- Sys.time()
  log_message(paste("✅ PAHO compilation complete for week", download_week))
  log_message(paste("Rows:", nrow(final_df), "Cols:", ncol(final_df)))
  log_message(paste("Total duration:", round(difftime(end_time, start_time, units = "secs"), 2), "seconds"))
  
  return(final_df)
}



# log helper function for country df
add_country_step <- function(log_df, df, step_name, country_col = "country") {
  
  # Extract unique countries
  new_countries <- df[[country_col]] |> unique()
  
  # Combine with existing countries
  all_countries <- union(log_df$country, new_countries)
  
  # Expand log to include all countries
  log_df <- log_df |> 
    complete(country = all_countries)
  
  # Add a new logical column showing membership
  log_df[[step_name]] <- log_df$country %in% new_countries
  
  return(log_df)
}
