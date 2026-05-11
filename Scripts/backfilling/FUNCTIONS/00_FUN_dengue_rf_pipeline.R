#' ---------------------------------------------------------------------------
#' 00_FUN_dengue_rf_pipeline.R
#' ---------------------------------------------------------------------------
#' Portable, self-contained R script for the Dengue Empirical Reporting Factor
#' Pipeline. Source this file in any project to obtain up-to-date empirical
#' reporting factors from PAHO, WHO Global, and SEARO crawler data.
#'
#' Public functions:
#'   1. download_crawler_data()             - Download raw data to disk (legacy)
#'   2. standardize_delay_df()              - Standardize from local file paths (legacy)
#'   3. download_and_standardize_delay_df() - Download to temp, standardize in-memory (recommended)
#'   3b. download_and_standaedise()         - Download to temp, unify key variables only
#'   4. calculate_empirical_rf()            - K-fold cross-validated empirical RF (optionally write CSV)
#'   5. run_monthly_rf_refresh()            - Build monthly RF artifacts and latest RF cache files
#' ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Section 1: Library declarations
# ---------------------------------------------------------------------------
library(httr)
library(jsonlite)
library(readxl)
library(readr)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)
library(countrycode)
library(caret)
library(ISOweek)

# Non-interactive runs (Rscript, CI) are not a TTY: disable cli progress bars to
# avoid thousands of "Cannot determine terminal size" warnings and extra overhead.
if (!interactive()) {
  Sys.setenv(VROOM_SHOW_PROGRESS = "false")
  options(
    cli.progress_bar = FALSE,
    cli.spinner = FALSE,
    dplyr.show_progress = FALSE,
    readr.show_progress = FALSE
  )
}

# ---------------------------------------------------------------------------
# Section 2: Internal helpers
# ---------------------------------------------------------------------------

.github_api_request <- function(url, max_attempts = 5L) {
  token <- Sys.getenv("GITHUB_TOKEN", unset = "")
  headers <- c("User-Agent" = "DENV-Observatory/1.0")
  if (nchar(token) > 0) {
    headers <- c(headers, "Authorization" = paste("token", token))
  }
  last_res <- NULL
  for (attempt in seq_len(max_attempts)) {
    res <- httr::GET(url, httr::add_headers(.headers = headers))
    last_res <- res
    if (res$status_code == 403) {
      h <- httr::headers(res)
      if (any(grepl("rate limit", tolower(h), fixed = TRUE))) {
        message("GitHub API rate limit may be exceeded. Set GITHUB_TOKEN for higher limits.")
      }
    }
    if (res$status_code == 200L) {
      return(res)
    }
    if (res$status_code %in% c(403L, 429L, 502L, 503L) && attempt < max_attempts) {
      wait_s <- min(60L, as.integer(2^attempt))
      message(sprintf(
        "GitHub API status %s; waiting %ds then retry %d/%d ...",
        res$status_code, wait_s, attempt + 1L, max_attempts
      ))
      Sys.sleep(wait_s)
      next
    }
    break
  }
  httr::stop_for_status(last_res)
  return(last_res)
}

#' List contents of a GitHub repo path (files and subdirs). Returns a data frame
#' with name, path, type, download_url (for files).
.github_list_contents <- function(repo, path, branch = "main") {
  base <- "https://api.github.com/repos"
  url <- sprintf("%s/%s/contents/%s?ref=%s", base, repo, path, branch)
  res <- .github_api_request(url)
  out <- httr::content(res, as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(out, simplifyDataFrame = TRUE)
  if (is.data.frame(parsed)) return(parsed)
  if (is.list(parsed) && length(parsed) == 0) return(data.frame(name = character(), path = character(), type = character(), download_url = character(), stringsAsFactors = FALSE))
  data.frame(name = parsed$name, path = parsed$path, type = parsed$type, download_url = if ("download_url" %in% names(parsed)) parsed$download_url else NA_character_, stringsAsFactors = FALSE)
}

#' Download a single file from a URL to dest_path.
.github_download_file <- function(url, dest_path) {
  token <- Sys.getenv("GITHUB_TOKEN", unset = "")
  headers <- c("User-Agent" = "DENV-Observatory/1.0")
  if (nchar(token) > 0) headers <- c(headers, "Authorization" = paste("token", token))
  r <- httr::GET(url, httr::add_headers(.headers = headers), httr::write_disk(dest_path, overwrite = TRUE))
  httr::stop_for_status(r)
  invisible(dest_path)
}

.calculate_weeks_diff <- function(year, EW, ext_date) {
  iso_date <- ISOweek::ISOweek2date(paste0(year, "-W", sprintf("%02d", EW), "-1"))
  as.numeric(difftime(ext_date, iso_date, units = "weeks"))
}

.calculate_months_diff <- function(year, month, ext_date) {
  data_date <- as.Date(sprintf("%04d-%02d-01", year, month))
  as.integer(round((ext_date - data_date) / 30.44))
}

#' PAHO Spanish -> English country names and ISO3.
.normalize_country <- function(df, col = "country") {
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
  df$country <- dplyr::recode(df[[col]], !!!country_map)
  df$iso3c <- countrycode::countrycode(df$country, origin = "country.name", destination = "iso3c")
  df
}

#' Return named vector mapping PAHO column names (English + Spanish) to standard names.
.paho_col_map <- function() {
  m <- list(
    ID = "ID",
    Country.or.Subregion = "country",
    "País.o.Subregion" = "country",
    Serotype = "serotype",
    Serotipo = "serotype",
    Year = "year",
    "Año...4" = "year",
    In...Out.of.Subregions = "in_out",
    "Epi..Week..a." = "EW",
    "Semana.Epidemiológica..a." = "EW",
    "Total.of.Dengue.Cases..b." = "total_den",
    "Total.de.Casos.de.Dengue..b." = "total_den",
    "Incidence.Rate..c." = "incidence",
    "Tasa.de.Incidencia..c." = "incidence",
    Laboratory.Confirmed = "lab_confirmed",
    Confirmados.Laboratorio = "lab_confirmed",
    "X..Lab.Conf..x100." = "percent_Lconf",
    "%.Lab.Conf..x100." = "percent_Lconf",
    "Severe.Dengue..d." = "sev_den",
    "Dengue.Grave..d." = "sev_den",
    "X.SD.D..x100..e." = "SD_D",
    "X..SD.D..x100..e." = "SD_D",
    "X.DG.D..x100..e." = "SD_D",
    Deaths = "deaths",
    Muertes = "deaths",
    "CFR..f." = "CFR",
    "Letalidad..f." = "CFR",
    "Population.X.1000." = "pop",
    "Población.X.1000" = "pop"
  )
  setNames(unname(unlist(m)), names(m))
}

.month_name_to_num <- function(x) {
  months <- c(Jan = 1L, Feb = 2L, Mar = 3L, Apr = 4L, May = 5L, June = 6L,
              Jul = 7L, July = 7L, Aug = 8L, Sep = 9L, Sept = 9L, Oct = 10L, Nov = 11L, Dec = 12L)
  unname(months[match(x, names(months))])
}

# PAHO subregion names to exclude from country-level RF (aggregates).
.paho_subregions <- function() {
  c("Andean Subregion", "The Americas", "Non-Latin Caribbean",
    "Central America Ithsmus and Mexico", "Latin Caribbean", "North America", "Southern Cone")
}

#' Parse a single PAHO crawler file into standardized delay rows.
#' @param file_path Path to a single TSV/CSV-like file from PAHO-crawler.
#' @param ext_date Extraction/download date (Date).
.parse_paho_delay_file <- function(file_path, ext_date) {
  col_map <- .paho_col_map()
  df <- readr::read_tsv(file_path, locale = readr::locale(encoding = "UTF-16LE"), show_col_types = FALSE)
  df <- df %>% filter(!.data$ID == "Grand Total")
  df <- df %>% select(!tidyselect::matches("Año\\.\\.\\.5"))

  names(df) <- make.names(names(df))
  matched <- intersect(names(df), names(col_map))
  if (length(matched) == 0) return(NULL)
  df <- df %>%
    rename_with(~ col_map[.x], .cols = matched) %>%
    filter(!is.na(.data$EW))

  if (!"country" %in% names(df)) return(NULL)
  df <- .normalize_country(df)

  if (!("year" %in% names(df) && "EW" %in% names(df))) return(NULL)
  df <- df %>% filter(!is.na(.data$year), !is.na(.data$EW))

  # Fill missing optional columns
  if (!"sev_den" %in% names(df)) df$sev_den <- NA_real_
  if (!"deaths" %in% names(df)) df$deaths <- NA_real_
  if (!"lab_confirmed" %in% names(df)) df$lab_confirmed <- NA_real_
  if (!"CFR" %in% names(df)) df$CFR <- NA_real_
  if (!"total_den" %in% names(df)) return(NULL)

  df <- df %>%
    mutate(
      s = as.integer(.data$year),
      t = as.integer(.data$EW),
      d = as.integer(round(.calculate_weeks_diff(.data$year, .data$EW, ext_date), 0)),
      Nts = as.numeric(.data$total_den),
      Nts_sev = as.numeric(.data$sev_den),
      Nts_death = as.numeric(.data$deaths),
      Nts_lab = as.numeric(.data$lab_confirmed),
      CFRts = as.numeric(.data$CFR),
      time_resolution = "weekly",
      ext_date = as.Date(ext_date),
      source = "PAHO"
    ) %>%
    group_by(.data$source, .data$country, .data$iso3c, .data$s, .data$t, .data$time_resolution, .data$ext_date, .data$d) %>%
    summarise(
      Nts = sum(.data$Nts, na.rm = TRUE),
      Nts_sev = sum(.data$Nts_sev, na.rm = TRUE),
      Nts_death = sum(.data$Nts_death, na.rm = TRUE),
      Nts_lab = sum(.data$Nts_lab, na.rm = TRUE),
      CFRts = mean(.data$CFRts, na.rm = TRUE),
      .groups = "drop"
    )

  df %>% select(.data$source, .data$country, .data$iso3c, .data$s, .data$t, .data$time_resolution, .data$ext_date, .data$d,
    .data$Nts, .data$Nts_sev, .data$Nts_death, .data$Nts_lab, .data$CFRts)
}

.parse_searo_delay_file <- function(file_path, ext_date) {
  df <- readr::read_csv(file_path, show_col_types = FALSE)
  df <- df %>% filter(.data$Chart_Type == "line")
  if (!all(c("Month", "Year", "Value", "Country") %in% names(df))) return(NULL)

  month_num <- .month_name_to_num(df$Month)
  df <- df %>%
    mutate(
      month_num = month_num,
      ext_date = as.Date(ext_date),
      s = as.integer(.data$Year),
      t = as.integer(.data$month_num),
      d = .calculate_months_diff(.data$Year, .data$month_num, as.Date(ext_date)),
      Nts = as.numeric(.data$Value),
      Nts_sev = NA_real_,
      Nts_death = NA_real_,
      Nts_lab = NA_real_,
      CFRts = NA_real_,
      time_resolution = "monthly",
      source = "SEARO",
      country = .data$Country,
      iso3c = countrycode::countrycode(.data$Country, "country.name", "iso3c")
    )

  df %>%
    select(.data$source, .data$country, .data$iso3c, .data$s, .data$t, .data$time_resolution, .data$ext_date, .data$d,
      .data$Nts, .data$Nts_sev, .data$Nts_death, .data$Nts_lab, .data$CFRts)
}

.parse_who_delay_file <- function(file_path, ext_date) {
  df <- readxl::read_excel(file_path, sheet = 1)
  nm <- tolower(names(df))
  country_col <- names(df)[which(nm %in% c("country", "countries", "country/area"))[1]]
  year_col <- names(df)[which(nm %in% c("year", "annee"))[1]]
  month_col <- names(df)[which(nm %in% c("month", "mois", "epi_month"))[1]]
  cases_col <- names(df)[which(nm %in% c("cases", "total cases", "dengue cases", "value"))[1]]
  if (is.na(country_col) || is.na(cases_col)) return(NULL)

  df <- df %>% rename(country = !!rlang::sym(country_col), Nts = !!rlang::sym(cases_col))
  if (!is.na(year_col)) df <- df %>% rename(year = !!rlang::sym(year_col)) else df$year <- lubridate::year(ext_date)

  if (!is.na(month_col)) {
    if (is.character(df[[month_col]])) df$month_num <- .month_name_to_num(df[[month_col]])
    else df$month_num <- as.integer(df[[month_col]])
  } else {
    df$month_num <- 1L
  }

  df <- df %>%
    mutate(
      s = as.integer(.data$year),
      t = as.integer(.data$month_num),
      ext_date = as.Date(ext_date),
      d = .calculate_months_diff(.data$year, .data$month_num, as.Date(ext_date)),
      Nts_sev = NA_real_,
      Nts_death = NA_real_,
      Nts_lab = NA_real_,
      CFRts = NA_real_,
      time_resolution = "monthly",
      source = "WHO_Global",
      iso3c = countrycode::countrycode(.data$country, "country.name", "iso3c")
    )

  df %>% select(.data$source, .data$country, .data$iso3c, .data$s, .data$t, .data$time_resolution, .data$ext_date, .data$d,
    .data$Nts, .data$Nts_sev, .data$Nts_death, .data$Nts_lab, .data$CFRts)
}

#' Apply the 1-year (delay) cutoff and compute Nts_V/rc/rf via latest eligible snapshot.
.apply_validation_merge <- function(raw_df, source, resolution_cutoff_weeks = 52L, resolution_cutoff_months = 12L) {
  if (nrow(raw_df) == 0) return(raw_df)
  cutoff <- if (source == "PAHO") resolution_cutoff_weeks else resolution_cutoff_months

  latest <- raw_df %>%
    filter(.data$d >= cutoff) %>%
    group_by(.data$country, .data$s, .data$t) %>%
    slice_max(.data$ext_date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(.data$country, .data$s, .data$t, Nts_V = .data$Nts)

  raw_df <- raw_df %>% left_join(latest, by = c("country", "s", "t"))
  raw_df %>%
    mutate(
      rc = dplyr::if_else(!is.na(.data$Nts_V) & .data$Nts_V > 0, .data$Nts / .data$Nts_V, NA_real_),
      rf = dplyr::if_else(!is.na(.data$Nts_V) & .data$Nts_V > 0 & !is.na(.data$Nts) & .data$Nts > 0, .data$Nts_V / .data$Nts, NA_real_)
    )
}

# ---------------------------------------------------------------------------
# Section 3: download_crawler_data()
# ---------------------------------------------------------------------------

#' Download raw data files from a Dengue crawler GitHub repository.
#'
#' @param source One of "PAHO", "WHO_Global", "SEARO".
#' @param dest_dir Local directory to save files (created if needed).
#' @param date_start Optional; only include snapshots on or after this date (Date or "YYYYMMDD").
#' @param date_end Optional; only include snapshots on or before this date (Date or "YYYYMMDD").
#' @return Character vector of local file paths downloaded.
#' @export
download_crawler_data <- function(source,
                                 dest_dir,
                                 date_start = NULL,
                                 date_end = NULL) {
  source <- match.arg(source, c("PAHO", "WHO_Global", "SEARO"))
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  .parse_date_filter <- function(x) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "Date")) return(x)
    as.Date(as.character(x), format = "%Y%m%d")
  }
  d_start <- .parse_date_filter(date_start)
  d_end <- .parse_date_filter(date_end)

  repo <- switch(source,
    PAHO = "DengueGlobalObservatory/PAHO-crawler",
    WHO_Global = "DengueGlobalObservatory/WHOGlobal-crawler",
    SEARO = "DengueGlobalObservatory/SEARO-crawler"
  )

  downloaded <- character(0)

  if (source == "PAHO") {
    # List data/ then each data/DL_YYYYMMDD/ then each file inside
    top <- .github_list_contents(repo, "data")
    if (nrow(top) == 0) {
      message("No contents in data/ for PAHO repo.")
      return(downloaded)
    }
    dirs <- top[top$type == "dir" & grepl("^DL_\\d{8}$", top$name), , drop = FALSE]
    for (i in seq_len(nrow(dirs))) {
      folder <- dirs$name[i]
      date_str <- sub("^DL_", "", folder)
      ext_d <- tryCatch(as.Date(date_str, "%Y%m%d"), error = function(e) NA)
      if (!is.na(ext_d)) {
        if (!is.null(d_start) && ext_d < d_start) next
        if (!is.null(d_end) && ext_d > d_end) next
      }
      sub_path <- paste0("data/", folder)
      contents <- .github_list_contents(repo, sub_path)
      if (nrow(contents) == 0) next
      files <- contents[contents$type == "file" & grepl("\\.(tsv|csv|txt)$", contents$name, ignore.case = TRUE), , drop = FALSE]
      sub_dest <- file.path(dest_dir, folder)
      if (!dir.exists(sub_dest)) dir.create(sub_dest, recursive = TRUE)
      for (j in seq_len(nrow(files))) {
        url <- files$download_url[j]
        if (is.na(url)) next
        dest_path <- file.path(sub_dest, files$name[j])
        tryCatch({
          .github_download_file(url, dest_path)
          downloaded <- c(downloaded, dest_path)
        }, error = function(e) message("Failed to download ", files$name[j], ": ", e$message))
      }
    }
  } else if (source == "SEARO") {
    path <- "output"
    contents <- .github_list_contents(repo, path)
    if (nrow(contents) == 0) {
      message("No contents in output/ for SEARO repo.")
      return(downloaded)
    }
    # SEARO_National_data_YYYYMMDD_HHMM.csv only (exclude barchart, DEBUG)
    files <- contents[contents$type == "file" &
      grepl("^SEARO_National_data_\\d{8}_\\d{4}\\.csv$", contents$name), , drop = FALSE]
    for (i in seq_len(nrow(files))) {
      fname <- files$name[i]
      date_str <- regmatches(fname, regexpr("\\d{8}", fname))
      ext_d <- tryCatch(as.Date(date_str, "%Y%m%d"), error = function(e) NA)
      if (!is.na(ext_d)) {
        if (!is.null(d_start) && ext_d < d_start) next
        if (!is.null(d_end) && ext_d > d_end) next
      }
      url <- files$download_url[i]
      if (is.na(url)) next
      dest_path <- file.path(dest_dir, fname)
      tryCatch({
        .github_download_file(url, dest_path)
        downloaded <- c(downloaded, dest_path)
      }, error = function(e) message("Failed to download ", fname, ": ", e$message))
    }
  } else if (source == "WHO_Global") {
    path <- "Downloads"
    contents <- .github_list_contents(repo, path)
    if (nrow(contents) == 0) {
      message("No contents in Downloads/ for WHO Global repo.")
      return(downloaded)
    }
    files <- contents[contents$type == "file" &
      grepl("dengue-global.*\\.xlsx$", contents$name, ignore.case = TRUE), , drop = FALSE]
    for (i in seq_len(nrow(files))) {
      fname <- files$name[i]
      # dengue-global-data-YYYY-MM-DD.xlsx or dengue-globla-data-...
      m <- regmatches(fname, regexpr("\\d{4}-\\d{2}-\\d{2}", fname))
      if (length(m)) {
        ext_d <- tryCatch(as.Date(m, "%Y-%m-%d"), error = function(e) NA)
        if (!is.na(ext_d)) {
          if (!is.null(d_start) && ext_d < d_start) next
          if (!is.null(d_end) && ext_d > d_end) next
        }
      }
      url <- files$download_url[i]
      if (is.na(url)) next
      dest_path <- file.path(dest_dir, fname)
      tryCatch({
        .github_download_file(url, dest_path)
        downloaded <- c(downloaded, dest_path)
      }, error = function(e) message("Failed to download ", fname, ": ", e$message))
    }
  }

  message(sprintf("Downloaded %d file(s) for %s to %s", length(downloaded), source, dest_dir))
  invisible(downloaded)
}

# ---------------------------------------------------------------------------
# Section 4: download_and_standardize_delay_df() (in-memory, no raw persistence)
# ---------------------------------------------------------------------------

#' Download crawler snapshots and standardize in-memory (no persistent raw files).
#'
#' Raw crawler downloads are written only to temporary files and deleted immediately.
#' The returned data frame includes Nts_V/rc/rf using the 1-year (delay) cutoff.
#'
#' @param source One of "PAHO", "WHO_Global", "SEARO".
#' @param resolution_cutoff_weeks Cutoff in weeks (PAHO); default 52.
#' @param resolution_cutoff_months Cutoff in months (SEARO/WHO); default 12.
#' @param date_start Optional; only include snapshots on or after this date (Date or "YYYYMMDD").
#' @param date_end Optional; only include snapshots on or before this date (Date or "YYYYMMDD").
#' @return Standardized delay data frame (with Nts_V, rc, rf).
#' @export
download_and_standardize_delay_df <- function(source,
                                               resolution_cutoff_weeks = 52L,
                                               resolution_cutoff_months = 12L,
                                               date_start = NULL,
                                               date_end = NULL) {
  source <- match.arg(source, c("PAHO", "WHO_Global", "SEARO"))

  .parse_date_filter <- function(x) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "Date")) return(x)
    as.Date(as.character(x), format = "%Y%m%d")
  }
  d_start <- .parse_date_filter(date_start)
  d_end <- .parse_date_filter(date_end)

  repo <- switch(source,
    PAHO = "DengueGlobalObservatory/PAHO-crawler",
    WHO_Global = "DengueGlobalObservatory/WHOGlobal-crawler",
    SEARO = "DengueGlobalObservatory/SEARO-crawler"
  )

  parsed_list <- list()

  # Helper: download -> parse -> cleanup
  .download_and_parse <- function(url, tmp_path, parse_fun) {
    parsed <- NULL
    tryCatch({
      .github_download_file(url, tmp_path)
      parsed <- tryCatch(parse_fun(tmp_path), error = function(e) NULL)
    }, error = function(e) NULL)
    if (file.exists(tmp_path)) unlink(tmp_path)
    parsed
  }

  if (source == "PAHO") {
    top <- .github_list_contents(repo, "data")
    if (nrow(top) > 0) {
      dirs <- top[top$type == "dir" & grepl("^DL_\\d{8}$", top$name), , drop = FALSE]
      for (i in seq_len(nrow(dirs))) {
        folder <- dirs$name[i]
        ext_d <- as.Date(sub("^DL_", "", folder), "%Y%m%d")
        if (!is.na(ext_d)) {
          if (!is.null(d_start) && ext_d < d_start) next
          if (!is.null(d_end) && ext_d > d_end) next
        } else {
          next
        }

        sub_path <- paste0("data/", folder)
        contents <- .github_list_contents(repo, sub_path)
        if (nrow(contents) == 0) next
        files <- contents[contents$type == "file" & grepl("\\.(tsv|csv|txt)$", contents$name, ignore.case = TRUE), , drop = FALSE]
        sub_fun <- function(tmp_path) .parse_paho_delay_file(tmp_path, ext_d)

        for (j in seq_len(nrow(files))) {
          url <- files$download_url[j]
          fname <- files$name[j]
          if (is.na(url)) next
          tmp_path <- tempfile(pattern = "paho_", fileext = paste0(".", tools::file_ext(fname)))
          parsed <- .download_and_parse(url, tmp_path, parse_fun = sub_fun)
          if (!is.null(parsed) && nrow(parsed) > 0) parsed_list <- c(parsed_list, list(parsed))
        }
      }
    }
  } else if (source == "SEARO") {
    contents <- .github_list_contents(repo, "output")
    if (nrow(contents) > 0) {
      files <- contents[contents$type == "file" & grepl("^SEARO_National_data_\\d{8}_\\d{4}\\.csv$", contents$name), , drop = FALSE]
      for (i in seq_len(nrow(files))) {
        fname <- files$name[i]
        m <- regmatches(fname, regexpr("\\d{8}", fname))
        if (length(m) == 0) next
        ext_d <- as.Date(m, "%Y%m%d")
        if (!is.na(ext_d)) {
          if (!is.null(d_start) && ext_d < d_start) next
          if (!is.null(d_end) && ext_d > d_end) next
        } else {
          next
        }

        url <- files$download_url[i]
        if (is.na(url)) next
        tmp_path <- tempfile(pattern = "searo_", fileext = paste0(".", tools::file_ext(fname)))
        parsed <- .download_and_parse(url, tmp_path, parse_fun = function(tp) .parse_searo_delay_file(tp, ext_d))
        if (!is.null(parsed) && nrow(parsed) > 0) parsed_list <- c(parsed_list, list(parsed))
      }
    }
  } else {
    contents <- .github_list_contents(repo, "Downloads")
    if (nrow(contents) > 0) {
      files <- contents[contents$type == "file" & grepl("dengue-global.*\\.xlsx$", contents$name, ignore.case = TRUE), , drop = FALSE]
      for (i in seq_len(nrow(files))) {
        fname <- files$name[i]
        m <- regmatches(fname, regexpr("\\d{4}-\\d{2}-\\d{2}", fname))
        if (length(m) == 0) next
        ext_d <- as.Date(m, "%Y-%m-%d")
        if (!is.na(ext_d)) {
          if (!is.null(d_start) && ext_d < d_start) next
          if (!is.null(d_end) && ext_d > d_end) next
        } else {
          next
        }

        url <- files$download_url[i]
        if (is.na(url)) next
        tmp_path <- tempfile(pattern = "who_", fileext = paste0(".", tools::file_ext(fname)))
        parsed <- .download_and_parse(url, tmp_path, parse_fun = function(tp) .parse_who_delay_file(tp, ext_d))
        if (!is.null(parsed) && nrow(parsed) > 0) parsed_list <- c(parsed_list, list(parsed))
      }
    }
  }

  if (length(parsed_list) == 0) return(.empty_delay_df())

  raw <- bind_rows(parsed_list)
  raw <- .apply_validation_merge(raw, source,
    resolution_cutoff_weeks = resolution_cutoff_weeks,
    resolution_cutoff_months = resolution_cutoff_months
  )
  raw
}

# ---------------------------------------------------------------------------
# Section 4b: download_and_standaedise() (in-memory, unify key variables only)
# ---------------------------------------------------------------------------

#' Download crawler snapshots and return a unified dataset (no delays/validation merge).
#'
#' This is a simplified variant of download_and_standardize_delay_df() intended for
#' step-by-step work on reporting-factor estimation. It stops after creating a
#' unified dataset from crawler downloads.
#'
#' Key variables are unified across sources:
#'   - t: time the cases are reported for (week number for PAHO; month number for SEARO/WHO)
#'   - R_t: reporting/extraction date (snapshot date)
#'   - total_den: total dengue cases
#'
#' Other dengue-related variables are preserved when present and are not aggressively
#' standardized beyond minimal harmonization needed to join sources.
#'
#' @param source One of "PAHO", "WHO_Global", "SEARO".
#' @param date_start Optional; only include snapshots on or after this date (Date or "YYYYMMDD").
#' @param date_end Optional; only include snapshots on or before this date (Date or "YYYYMMDD").
#' @return Unified data frame with at least source, country, iso3c, s, t, R_t, total_den, time_resolution.
#' @export
download_and_standaedise <- function(source,
                                     date_start = NULL,
                                     date_end = NULL) {
  source <- match.arg(source, c("PAHO", "WHO_Global", "SEARO"))

  .parse_date_filter <- function(x) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "Date")) return(x)
    as.Date(as.character(x), format = "%Y%m%d")
  }
  d_start <- .parse_date_filter(date_start)
  d_end <- .parse_date_filter(date_end)

  repo <- switch(source,
    PAHO = "DengueGlobalObservatory/PAHO-crawler",
    WHO_Global = "DengueGlobalObservatory/WHOGlobal-crawler",
    SEARO = "DengueGlobalObservatory/SEARO-crawler"
  )

  .empty_unified_df <- function() {
    data.frame(
      source = character(),
      country = character(),
      iso3c = character(),
      s = integer(),
      t = integer(),
      R_t = as.Date(character()),
      total_den = numeric(),
      time_resolution = character(),
      stringsAsFactors = FALSE
    )
  }

  .unify_paho <- function(file_path, ext_date) {
    col_map <- .paho_col_map()
    df <- readr::read_tsv(file_path, locale = readr::locale(encoding = "UTF-16LE"), show_col_types = FALSE)
    df <- df %>% dplyr::filter(!.data$ID == "Grand Total")
    df <- df %>% dplyr::select(!tidyselect::matches("Año\\.\\.\\.5"))

    names(df) <- make.names(names(df))
    matched <- intersect(names(df), names(col_map))
    if (length(matched) == 0) return(NULL)

    df <- df %>% dplyr::rename_with(~ col_map[.x], .cols = matched)
    if (!all(c("country", "year", "EW", "total_den") %in% names(df))) return(NULL)

    df <- df %>%
      dplyr::filter(!is.na(.data$EW), !is.na(.data$year)) %>%
      .normalize_country(col = "country") %>%
      dplyr::mutate(
        source = "PAHO",
        s = as.integer(.data$year),
        t = as.integer(.data$EW),
        R_t = as.Date(ext_date),
        time_resolution = "weekly",
        total_den = suppressWarnings(as.numeric(.data$total_den))
      )

    # Keep other dengue variables if present; but avoid forcing any schema beyond the key variables.
    df %>%
      dplyr::select(
        .data$source,
        .data$country,
        .data$iso3c,
        .data$s,
        .data$t,
        .data$R_t,
        .data$time_resolution,
        .data$total_den,
        dplyr::everything()
      )
  }

  .unify_searo <- function(file_path, ext_date) {
    df <- readr::read_csv(file_path, show_col_types = FALSE)
    df <- df %>% dplyr::filter(.data$Chart_Type == "line")
    if (!all(c("Month", "Year", "Value", "Country") %in% names(df))) return(NULL)

    month_num <- .month_name_to_num(df$Month)
    df <- df %>%
      dplyr::mutate(
        month_num = month_num,
        source = "SEARO",
        s = as.integer(.data$Year),
        t = as.integer(.data$month_num),
        R_t = as.Date(ext_date),
        time_resolution = "monthly",
        total_den = suppressWarnings(as.numeric(.data$Value)),
        country = .data$Country,
        iso3c = countrycode::countrycode(.data$Country, "country.name", "iso3c")
      )

    df %>%
      dplyr::select(
        .data$source,
        .data$country,
        .data$iso3c,
        .data$s,
        .data$t,
        .data$R_t,
        .data$time_resolution,
        .data$total_den,
        dplyr::everything()
      )
  }

  .unify_who <- function(file_path, ext_date) {
    df <- readxl::read_excel(file_path, sheet = 1)
    nm <- tolower(names(df))
    country_col <- names(df)[which(nm %in% c("country", "countries", "country/area"))[1]]
    year_col <- names(df)[which(nm %in% c("year", "annee"))[1]]
    month_col <- names(df)[which(nm %in% c("month", "mois", "epi_month"))[1]]
    cases_col <- names(df)[which(nm %in% c("cases", "total cases", "dengue cases", "value"))[1]]
    if (is.na(country_col) || is.na(cases_col)) return(NULL)

    df <- df %>% dplyr::rename(country = !!rlang::sym(country_col))
    df$total_den <- suppressWarnings(as.numeric(df[[cases_col]]))

    if (!is.na(year_col)) df$year <- suppressWarnings(as.integer(df[[year_col]])) else df$year <- lubridate::year(ext_date)

    if (!is.na(month_col)) {
      if (is.character(df[[month_col]])) df$month_num <- .month_name_to_num(df[[month_col]])
      else df$month_num <- suppressWarnings(as.integer(df[[month_col]]))
    } else {
      df$month_num <- 1L
    }

    df <- df %>%
      dplyr::mutate(
        source = "WHO_Global",
        s = as.integer(.data$year),
        t = as.integer(.data$month_num),
        R_t = as.Date(ext_date),
        time_resolution = "monthly",
        iso3c = countrycode::countrycode(.data$country, "country.name", "iso3c")
      )

    df %>%
      dplyr::select(
        .data$source,
        .data$country,
        .data$iso3c,
        .data$s,
        .data$t,
        .data$R_t,
        .data$time_resolution,
        .data$total_den,
        dplyr::everything()
      )
  }

  parsed_list <- list()

  .download_and_parse <- function(url, tmp_path, parse_fun) {
    parsed <- NULL
    tryCatch({
      .github_download_file(url, tmp_path)
      parsed <- tryCatch(parse_fun(tmp_path), error = function(e) NULL)
    }, error = function(e) NULL)
    if (file.exists(tmp_path)) unlink(tmp_path)
    parsed
  }

  if (source == "PAHO") {
    top <- .github_list_contents(repo, "data")
    if (nrow(top) > 0) {
      dirs <- top[top$type == "dir" & grepl("^DL_\\d{8}$", top$name), , drop = FALSE]
      for (i in seq_len(nrow(dirs))) {
        folder <- dirs$name[i]
        ext_d <- as.Date(sub("^DL_", "", folder), "%Y%m%d")
        if (!is.na(ext_d)) {
          if (!is.null(d_start) && ext_d < d_start) next
          if (!is.null(d_end) && ext_d > d_end) next
        } else {
          next
        }

        sub_path <- paste0("data/", folder)
        contents <- .github_list_contents(repo, sub_path)
        if (nrow(contents) == 0) next
        files <- contents[contents$type == "file" & grepl("\\.(tsv|csv|txt)$", contents$name, ignore.case = TRUE), , drop = FALSE]
        sub_fun <- function(tmp_path) .unify_paho(tmp_path, ext_d)

        for (j in seq_len(nrow(files))) {
          url <- files$download_url[j]
          fname <- files$name[j]
          if (is.na(url)) next
          tmp_path <- tempfile(pattern = "paho_", fileext = paste0(".", tools::file_ext(fname)))
          parsed <- .download_and_parse(url, tmp_path, parse_fun = sub_fun)
          if (!is.null(parsed) && nrow(parsed) > 0) parsed_list <- c(parsed_list, list(parsed))
        }
      }
    }
  } else if (source == "SEARO") {
    contents <- .github_list_contents(repo, "output")
    if (nrow(contents) > 0) {
      files <- contents[contents$type == "file" & grepl("^SEARO_National_data_\\d{8}_\\d{4}\\.csv$", contents$name), , drop = FALSE]
      for (i in seq_len(nrow(files))) {
        fname <- files$name[i]
        m <- regmatches(fname, regexpr("\\d{8}", fname))
        if (length(m) == 0) next
        ext_d <- as.Date(m, "%Y%m%d")
        if (!is.na(ext_d)) {
          if (!is.null(d_start) && ext_d < d_start) next
          if (!is.null(d_end) && ext_d > d_end) next
        } else {
          next
        }

        url <- files$download_url[i]
        if (is.na(url)) next
        tmp_path <- tempfile(pattern = "searo_", fileext = paste0(".", tools::file_ext(fname)))
        parsed <- .download_and_parse(url, tmp_path, parse_fun = function(tp) .unify_searo(tp, ext_d))
        if (!is.null(parsed) && nrow(parsed) > 0) parsed_list <- c(parsed_list, list(parsed))
      }
    }
  } else {
    contents <- .github_list_contents(repo, "Downloads")
    if (nrow(contents) > 0) {
      files <- contents[contents$type == "file" & grepl("dengue-global.*\\.xlsx$", contents$name, ignore.case = TRUE), , drop = FALSE]
      for (i in seq_len(nrow(files))) {
        fname <- files$name[i]
        m <- regmatches(fname, regexpr("\\d{4}-\\d{2}-\\d{2}", fname))
        if (length(m) == 0) next
        ext_d <- as.Date(m, "%Y-%m-%d")
        if (!is.na(ext_d)) {
          if (!is.null(d_start) && ext_d < d_start) next
          if (!is.null(d_end) && ext_d > d_end) next
        } else {
          next
        }

        url <- files$download_url[i]
        if (is.na(url)) next
        tmp_path <- tempfile(pattern = "who_", fileext = paste0(".", tools::file_ext(fname)))
        parsed <- .download_and_parse(url, tmp_path, parse_fun = function(tp) .unify_who(tp, ext_d))
        if (!is.null(parsed) && nrow(parsed) > 0) parsed_list <- c(parsed_list, list(parsed))
      }
    }
  }

  if (length(parsed_list) == 0) return(.empty_unified_df())

  dplyr::bind_rows(parsed_list)
}

# ---------------------------------------------------------------------------
# Section 4: standardize_delay_df()
# ---------------------------------------------------------------------------

#' Read and standardize downloaded crawler files into a common delay data frame with validation merge.
#'
#' @param source One of "PAHO", "WHO_Global", "SEARO".
#' @param file_paths Character vector of local file paths (e.g. from download_crawler_data()).
#' @param resolution_cutoff_weeks Cutoff in weeks for weekly data (PAHO); default 52.
#' @param resolution_cutoff_months Cutoff in months for monthly data (SEARO/WHO); default 12.
#' @return A data frame with standardized columns including Nts_V, rc, rf.
#' @export
standardize_delay_df <- function(source,
                                file_paths,
                                resolution_cutoff_weeks = 52L,
                                resolution_cutoff_months = 12L) {
  source <- match.arg(source, c("PAHO", "WHO_Global", "SEARO"))
  if (length(file_paths) == 0) return(.empty_delay_df())

  out_list <- list()
  for (f in file_paths) {
    ext_d <- NA
    if (source == "PAHO") {
      parent <- basename(dirname(f))
      if (grepl("^DL_\\d{8}$", parent)) {
        ext_d <- as.Date(sub("^DL_", "", parent), "%Y%m%d")
      }
      if (is.na(ext_d)) next
      x <- tryCatch(.parse_paho_delay_file(f, ext_d), error = function(e) NULL)
    } else if (source == "SEARO") {
      fname <- basename(f)
      m <- regmatches(fname, regexpr("\\d{8}", fname))
      if (length(m)) ext_d <- as.Date(m, "%Y%m%d")
      if (is.na(ext_d)) next
      x <- tryCatch(.parse_searo_delay_file(f, ext_d), error = function(e) NULL)
    } else {
      fname <- basename(f)
      m <- regmatches(fname, regexpr("\\d{4}-\\d{2}-\\d{2}", fname))
      if (length(m)) ext_d <- as.Date(m, "%Y-%m-%d")
      if (is.na(ext_d)) next
      x <- tryCatch(.parse_who_delay_file(f, ext_d), error = function(e) NULL)
    }

    if (!is.null(x) && nrow(x) > 0) out_list <- c(out_list, list(x))
  }

  if (length(out_list) == 0) return(.empty_delay_df())
  raw <- bind_rows(out_list)
  raw <- .apply_validation_merge(raw, source,
    resolution_cutoff_weeks = resolution_cutoff_weeks,
    resolution_cutoff_months = resolution_cutoff_months
  )
  raw
}

.empty_delay_df <- function() {
  data.frame(source = character(), country = character(), iso3c = character(),
    s = integer(), t = integer(), time_resolution = character(), ext_date = as.Date(character()),
    d = integer(), Nts = numeric(), Nts_sev = numeric(), Nts_death = numeric(),
    Nts_lab = numeric(), CFRts = numeric(), Nts_V = numeric(), rc = numeric(), rf = numeric(),
    stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Section 5: calculate_empirical_rf()
# ---------------------------------------------------------------------------

#' K-fold cross-validated empirical reporting factor estimation.
#'
#' @param delay_df Standardized delay data frame from standardize_delay_df() (must have Nts, Nts_V, rf, d, country, source, time_resolution).
#' @param k_folds Number of folds; default 5.
#' @param n_replicates Number of replicate fold assignments; default 3.
#' @param seed Random seed; default 123.
#' @param exclude_subregions For PAHO, exclude aggregate subregions; default TRUE.
#' @param output_csv Optional path to write the RF summary CSV (only this output is written to disk).
#' @return Summary data frame with country, d, mean_ratio, median_ratio, mean_rf, median_rf, mean_RMSE, mean_MAE (and source, time_resolution).
#' @export
calculate_empirical_rf <- function(delay_df,
                                  k_folds = 5L,
                                  n_replicates = 3L,
                                  seed = 123L,
                                  exclude_subregions = TRUE,
                                  output_csv = NULL) {
  df <- delay_df %>%
    filter(!is.na(.data$Nts), !is.na(.data$Nts_V), .data$Nts > 0, .data$Nts_V > 0, .data$d >= 0)
  if (exclude_subregions && "PAHO" %in% df$source) {
    df <- df %>% filter(!.data$country %in% .paho_subregions())
  }
  if (nrow(df) == 0) {
    return(data.frame(source = character(), country = character(), d = integer(), time_resolution = character(),
      mean_ratio = numeric(), median_ratio = numeric(), mean_rf = numeric(), median_rf = numeric(),
      mean_RMSE = numeric(), mean_MAE = numeric(), stringsAsFactors = FALSE))
  }

  # Add k-fold indices per (country, d), multiple replicates
  set.seed(seed)
  kfold_cols <- paste0("kfold_", seq_len(n_replicates))
  for (rep in seq_len(n_replicates)) {
    col <- kfold_cols[rep]
    grp <- df %>% group_by(.data$country, .data$d, .data$source, .data$time_resolution)
    idx <- grp %>% mutate(fold_idx = sample(rep(seq_len(k_folds), length.out = n()))) %>% pull("fold_idx")
    df[[col]] <- idx
  }

  results_list <- list()
  for (kcol in kfold_cols) {
    for (k in seq_len(k_folds)) {
      train <- df %>% filter(.data[[kcol]] != k)
      test <- df %>% filter(.data[[kcol]] == k)
      if (nrow(train) == 0 || nrow(test) == 0) next
      mean_ratio <- train %>%
        group_by(.data$country, .data$d, .data$source, .data$time_resolution) %>%
        summarise(mean_ratio = mean(.data$Nts / .data$Nts_V, na.rm = TRUE), .groups = "drop") %>%
        dplyr::filter(
          !is.na(.data$mean_ratio),
          is.finite(.data$mean_ratio),
          .data$mean_ratio > 0
        )
      test <- test %>%
        left_join(mean_ratio, by = c("country", "d", "source", "time_resolution")) %>%
        mutate(
          predicted_Nts_V = .data$Nts * (1 / .data$mean_ratio),
          RMSE = sqrt(mean((.data$predicted_Nts_V - .data$Nts_V)^2, na.rm = TRUE)),
          MAE = mean(abs(.data$predicted_Nts_V - .data$Nts_V), na.rm = TRUE)
        )
      results_list <- c(results_list, list(test %>% select(.data$source, .data$country, .data$d, .data$time_resolution, .data$Nts, .data$Nts_V, .data$predicted_Nts_V, .data$mean_ratio, .data$RMSE, .data$MAE)))
    }
  }

  all_results <- bind_rows(results_list)
  if (nrow(all_results) == 0) {
    return(data.frame(source = character(), country = character(), d = integer(), time_resolution = character(),
      mean_ratio = numeric(), median_ratio = numeric(), mean_rf = numeric(), median_rf = numeric(),
      mean_RMSE = numeric(), mean_MAE = numeric(), stringsAsFactors = FALSE))
  }

  summary_results <- all_results %>%
    group_by(.data$source, .data$country, .data$d, .data$time_resolution) %>%
    summarise(
      mean_ratio = mean(.data$mean_ratio, na.rm = TRUE),
      median_ratio = median(.data$mean_ratio, na.rm = TRUE),
      mean_RMSE = mean(.data$RMSE, na.rm = TRUE),
      mean_MAE = mean(.data$MAE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      mean_rf = dplyr::if_else(
        !is.na(.data$mean_ratio) & is.finite(.data$mean_ratio) & .data$mean_ratio > 0,
        1 / .data$mean_ratio,
        NA_real_
      ),
      median_rf = dplyr::if_else(
        !is.na(.data$median_ratio) & is.finite(.data$median_ratio) & .data$median_ratio > 0,
        1 / .data$median_ratio,
        NA_real_
      )
    )

  if (!is.null(output_csv)) {
    write.csv(summary_results, file = output_csv, row.names = FALSE)
  }

  summary_results
}

# ---------------------------------------------------------------------------
# Section 6: run_monthly_rf_refresh()
# ---------------------------------------------------------------------------

#' Run monthly RF refresh and write a single combined RF artifact.
#'
#' Generates RF summaries for PAHO, SEARO, and WHO_Global from crawler snapshots,
#' combines them into one data frame, and writes both a dated monthly file and a
#' stable `latest` file to `output_dir`.
#'
#' @param output_dir Directory where RF artifacts are written.
#' @param run_date Date used in output file naming; default Sys.Date().
#' @param date_start Optional snapshot lower bound passed to
#'   download_and_standardize_delay_df() (Date or "YYYYMMDD").
#' @param date_end Optional snapshot upper bound passed to
#'   download_and_standardize_delay_df() (Date or "YYYYMMDD").
#' @param k_folds Number of folds for calculate_empirical_rf(); default 5.
#' @param n_replicates Number of replicate fold assignments; default 3.
#' @param seed Random seed; default 123.
#' @return Named list with combined/source RF data frames and output file paths.
#' @export
run_monthly_rf_refresh <- function(output_dir = "Output",
                                   run_date = Sys.Date(),
                                   date_start = NULL,
                                   date_end = NULL,
                                   k_folds = 5L,
                                   n_replicates = 3L,
                                   seed = 123L) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  run_date <- as.Date(run_date)
  stamp <- format(run_date, "%Y_%m")

  .calculate_source_rf <- function(source) {
    delay_df <- download_and_standardize_delay_df(
      source = source,
      date_start = date_start,
      date_end = date_end
    )

    rf_df <- calculate_empirical_rf(
      delay_df = delay_df,
      k_folds = k_folds,
      n_replicates = n_replicates,
      seed = seed
    )
    rf_df
  }

  paho_rf <- .calculate_source_rf("PAHO")
  searo_rf <- .calculate_source_rf("SEARO")
  who_global_rf <- .calculate_source_rf("WHO_Global")

  combined_rf <- dplyr::bind_rows(paho_rf, searo_rf, who_global_rf) %>%
    dplyr::mutate(
      iso3 = countrycode::countrycode(
        .data$country,
        origin = "country.name",
        destination = "iso3c",
        custom_match = c(
          "Saint Martin (French part)" = "MAF",
          "Saint Martin" = "MAF"
        )
      )
    )

  combined_dated_path <- file.path(output_dir, sprintf("empirical_rf_%s.csv", stamp))
  combined_latest_path <- file.path(output_dir, "empirical_rf_latest.csv")

  write.csv(combined_rf, file = combined_dated_path, row.names = FALSE)
  file.copy(combined_dated_path, combined_latest_path, overwrite = TRUE)

  list(
    run_date = run_date,
    output_dir = normalizePath(output_dir, winslash = "/", mustWork = FALSE),
    combined_rf = combined_rf,
    combined_dated_path = combined_dated_path,
    combined_latest_path = combined_latest_path,
    paho_rf = paho_rf,
    searo_rf = searo_rf,
    who_global_rf = who_global_rf
  )
}

# ---------------------------------------------------------------------------
# Section 7: Example usage (commented)
# ---------------------------------------------------------------------------
#
# ## Example: Get up-to-date PAHO reporting factors
# paho_delay <- download_and_standardize_delay_df("PAHO")
# paho_rf    <- calculate_empirical_rf(paho_delay, output_csv = "paho_empirical_rf.csv")
#
# ## Example: Get SEARO reporting factors
# searo_delay <- download_and_standardize_delay_df("SEARO")
# searo_rf    <- calculate_empirical_rf(searo_delay, output_csv = "searo_empirical_rf.csv")
#
# ## Example: Get WHO Global reporting factors
# who_delay <- download_and_standardize_delay_df("WHO_Global")
# who_rf    <- calculate_empirical_rf(who_delay, output_csv = "who_global_empirical_rf.csv")
