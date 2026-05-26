#' ---------------------------------------------------------------------------
#' 00_FUN_whosearo_backfill.R
#' ---------------------------------------------------------------------------
#' Lightweight WHO Global and SEARO crawler download + standardisation for
#' backfilling workflows. Standalone: does not source 00_FUN_dengue_rf_pipeline.R.
#'
#' Public function:
#'   download_and_standardise(source)
#'
#' Unified columns (monthly sources):
#'   s  — calendar year of the case period
#'   t  — month number (1–12) of the case period
#'   tr — reporting / snapshot date (from crawler filename)
#'   total_den — dengue case count
#' ---------------------------------------------------------------------------

library(httr)
library(jsonlite)
library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(countrycode)

# ---------------------------------------------------------------------------
# GitHub API (minimal; shared by WHO and SEARO listing/download)
# ---------------------------------------------------------------------------

.github_get <- function(url, max_attempts = 5L) {
  token <- Sys.getenv("GITHUB_TOKEN", unset = "")
  headers <- c("User-Agent" = "DENV-Observatory/1.0")
  if (nchar(token) > 0) {
    headers <- c(headers, "Authorization" = paste("token", token))
  }
  last_res <- NULL
  for (attempt in seq_len(max_attempts)) {
    res <- httr::GET(url, httr::add_headers(.headers = headers))
    last_res <- res
    if (res$status_code == 200L) return(res)
    if (res$status_code %in% c(403L, 429L, 502L, 503L) && attempt < max_attempts) {
      Sys.sleep(min(60L, as.integer(2^attempt)))
      next
    }
    break
  }
  httr::stop_for_status(last_res)
  invisible(last_res)
}

.github_list <- function(repo, path, branch = "main") {
  url <- sprintf(
    "https://api.github.com/repos/%s/contents/%s?ref=%s",
    repo, path, branch
  )
  text <- httr::content(.github_get(url), as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(text, simplifyDataFrame = TRUE)
  if (is.data.frame(parsed)) return(parsed)
  data.frame(
    name = character(), path = character(), type = character(),
    download_url = character(), stringsAsFactors = FALSE
  )
}

.github_download <- function(url, dest_path) {
  token <- Sys.getenv("GITHUB_TOKEN", unset = "")
  headers <- c("User-Agent" = "DENV-Observatory/1.0")
  if (nchar(token) > 0) headers <- c(headers, "Authorization" = paste("token", token))
  r <- httr::GET(url, httr::add_headers(.headers = headers),
                 httr::write_disk(dest_path, overwrite = TRUE))
  httr::stop_for_status(r)
  invisible(dest_path)
}

.month_name_to_num <- function(x) {
  months <- c(
    Jan = 1L, Feb = 2L, Mar = 3L, Apr = 4L, May = 5L, June = 6L,
    Jul = 7L, July = 7L, Aug = 8L, Sep = 9L, Sept = 9L,
    Oct = 10L, Nov = 11L, Dec = 12L
  )
  unname(months[match(x, names(months))])
}

# ---------------------------------------------------------------------------
# download_and_standardise()
# ---------------------------------------------------------------------------

#' Download WHO Global or SEARO crawler snapshots and return a unified table.
#'
#' @param source \code{"WHO_Global"}, \code{"WHO"}, or \code{"SEARO"}.
#' @return Data frame with \code{source}, \code{country}, \code{iso3c},
#'   \code{s}, \code{t}, \code{tr}, \code{time_resolution}, \code{total_den},
#'   plus other columns from the raw file when present.
#' @examples
#' \dontrun{
#' who   <- download_and_standardise("WHO_Global")
#' searo <- download_and_standardise("SEARO")
#' }
download_and_standardise <- function(source) {
  source <- match.arg(source, c("WHO_Global", "WHO", "SEARO"))
  if (source == "WHO") source <- "WHO_Global"

  repo <- switch(source,
    WHO_Global = "DengueGlobalObservatory/WHOGlobal-crawler",
    SEARO      = "DengueGlobalObservatory/SEARO-crawler"
  )

  empty_df <- function() {
    data.frame(
      source = character(),
      country = character(),
      iso3c = character(),
      s = integer(),
      t = integer(),
      tr = as.Date(character()),
      time_resolution = character(),
      total_den = numeric(),
      stringsAsFactors = FALSE
    )
  }

  parse_searo_file <- function(file_path, tr) {
    df <- readr::read_csv(file_path, show_col_types = FALSE)
    df <- df %>% dplyr::filter(.data$Chart_Type == "line")
    if (!all(c("Month", "Year", "Value", "Country") %in% names(df))) {
      return(NULL)
    }

    df %>%
      dplyr::mutate(
        source = "SEARO",
        country = .data$Country,
        iso3c = countrycode::countrycode(.data$Country, "country.name", "iso3c"),
        s = as.integer(.data$Year),
        t = as.integer(.month_name_to_num(.data$Month)),
        tr = as.Date(tr),
        time_resolution = "monthly",
        total_den = suppressWarnings(as.numeric(.data$Value))
      ) %>%
      dplyr::select(
        dplyr::any_of(c(
          "source", "country", "iso3c", "s", "t", "tr",
          "time_resolution", "total_den"
        )),
        dplyr::everything()
      )
  }

  # WHO: read "data" sheet; s/t from monthly `date` column (not snapshot year)
  parse_who_file <- function(file_path, tr) {
    df <- readxl::read_excel(file_path, sheet = "data")
    nm <- tolower(names(df))

    country_col <- names(df)[which(nm %in% c("country", "countries", "country/area"))[1]]
    date_col    <- names(df)[which(nm %in% c("date", "period", "reporting_date", "time", "epi_date"))[1]]
    year_col    <- names(df)[which(nm %in% c("year", "annee"))[1]]
    month_col   <- names(df)[which(nm %in% c("month", "mois", "epi_month"))[1]]
    iso_col     <- names(df)[which(nm %in% c("iso3", "iso3c", "iso"))[1]]
    cases_col   <- names(df)[which(nm %in% c("cases", "total cases", "dengue cases", "value"))[1]]
    if (is.na(country_col) || is.na(cases_col)) return(NULL)

    df$country <- df[[country_col]]
    df$total_den <- suppressWarnings(as.numeric(df[[cases_col]]))

    if (!is.na(date_col)) {
      period_date <- as.Date(df[[date_col]])
      df$year <- lubridate::year(period_date)
      df$month_num <- lubridate::month(period_date)
    } else if (!is.na(year_col) || !is.na(month_col)) {
      if (!is.na(year_col)) {
        df$year <- suppressWarnings(as.integer(df[[year_col]]))
      } else {
        df$year <- lubridate::year(tr)
      }
      if (!is.na(month_col)) {
        if (is.character(df[[month_col]])) {
          df$month_num <- .month_name_to_num(df[[month_col]])
        } else {
          df$month_num <- suppressWarnings(as.integer(df[[month_col]]))
        }
      } else {
        df$month_num <- 1L
      }
    } else {
      df$year <- lubridate::year(tr)
      df$month_num <- 1L
    }

    if (!is.na(iso_col)) {
      df$iso3c <- as.character(df[[iso_col]])
    } else {
      df$iso3c <- countrycode::countrycode(df$country, "country.name", "iso3c")
    }

    df %>%
      dplyr::mutate(
        source = "WHO_Global",
        s = as.integer(.data$year),
        t = as.integer(.data$month_num),
        tr = as.Date(tr),
        time_resolution = "monthly"
      ) %>%
      dplyr::select(
        dplyr::any_of(c(
          "source", "country", "iso3c", "s", "t", "tr",
          "time_resolution", "total_den"
        )),
        dplyr::everything()
      )
  }

  download_parse <- function(url, tmp_path, parse_fun) {
    out <- NULL
    tryCatch({
      .github_download(url, tmp_path)
      out <- tryCatch(parse_fun(tmp_path), error = function(e) NULL)
    }, error = function(e) NULL)
    if (file.exists(tmp_path)) unlink(tmp_path)
    out
  }

  parsed_list <- list()

  if (source == "SEARO") {
    contents <- .github_list(repo, "output")
    if (nrow(contents) == 0) {
      message("No files found for SEARO")
      return(empty_df())
    }

    files <- contents[
      contents$type == "file" &
        grepl("^SEARO_National_data_\\d{8}_\\d{4}\\.csv$", contents$name),
      ,
      drop = FALSE
    ]
    for (i in seq_len(nrow(files))) {
      fname <- files$name[i]
      m <- regmatches(fname, regexpr("\\d{8}", fname))
      if (length(m) == 0) next
      tr <- as.Date(m, "%Y%m%d")

      url <- files$download_url[i]
      if (is.na(url)) next
      tmp <- tempfile(pattern = "searo_", fileext = ".csv")
      row <- download_parse(url, tmp, function(p) parse_searo_file(p, tr))
      if (!is.null(row) && nrow(row) > 0) parsed_list <- c(parsed_list, list(row))
    }
  } else {
    contents <- .github_list(repo, "Downloads")
    if (nrow(contents) == 0) {
      message("No files found for WHO_Global")
      return(empty_df())
    }

    files <- contents[
      contents$type == "file" &
        grepl("dengue-glob?al.*\\.xlsx$", contents$name, ignore.case = TRUE),
      ,
      drop = FALSE
    ]
    for (i in seq_len(nrow(files))) {
      fname <- files$name[i]
      m <- regmatches(fname, regexpr("\\d{4}-\\d{2}-\\d{2}", fname))
      if (length(m) == 0) next
      tr <- as.Date(m, "%Y-%m-%d")

      url <- files$download_url[i]
      if (is.na(url)) next
      tmp <- tempfile(pattern = "who_", fileext = ".xlsx")
      row <- download_parse(url, tmp, function(p) parse_who_file(p, tr))
      if (!is.null(row) && nrow(row) > 0) parsed_list <- c(parsed_list, list(row))
    }
  }

  if (length(parsed_list) == 0) return(empty_df())
  dplyr::bind_rows(parsed_list)
}

# ---------------------------------------------------------------------------
# source("Scripts/backfilling/FUNCTIONS/00_FUN_whosearo_backfill.R")
# who   <- download_and_standardise("WHO_Global")
# searo <- download_and_standardise("SEARO")
