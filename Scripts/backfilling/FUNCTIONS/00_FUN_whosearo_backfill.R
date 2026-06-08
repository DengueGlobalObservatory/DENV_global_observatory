#' ---------------------------------------------------------------------------
#' 00_FUN_whosearo_backfill.R
#' ---------------------------------------------------------------------------
#' Lightweight WHO Global, SEARO, and PAHO crawler download + standardisation
#' for backfilling workflows. Standalone: does not source 00_FUN_dengue_rf_pipeline.R.
#'
#' Public functions:
#'   download_and_standardise(source, ...) — PAHO supports cache + date window
#'   paho_assign_epiweek_to_month(df) — month assignment from EW end date (3-day rule)
#'
#' Unified columns:
#'   s  — calendar year (monthly) or epi year (PAHO weekly)
#'   t  — month (1–12) or epidemiological week (PAHO)
#'   tr — reporting / snapshot date (from crawler filename)
#'   total_den — dengue case count (cumulative for PAHO weekly)
#' ---------------------------------------------------------------------------

library(httr)
library(jsonlite)
library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(countrycode)
library(ISOweek)
library(stringr)

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

.paho_subregion_names <- function() {
  c(
    "Andean Subregion", "The Americas", "Non-Latin Caribbean",
    "Central America Ithsmus and Mexico", "Latin Caribbean",
    "North America", "Southern Cone"
  )
}

.paho_col_map <- function() {
  m <- list(
    ID = "ID",
    Country.or.Subregion = "country",
    "País.o.Subregion" = "country",
    Year = "year",
    "Año...4" = "year",
    "Epi..Week..a." = "EW",
    "Semana.Epidemiológica..a." = "EW",
    "Total.of.Dengue.Cases..b." = "total_den",
    "Total.de.Casos.de.Dengue..b." = "total_den"
  )
  setNames(unname(unlist(m)), names(m))
}

.paho_normalize_country <- function(df) {
  country_map <- c(
    "Canadá" = "Canada",
    "Estados Unidos de América" = "United States of America",
    "Belice" = "Belize",
    "México" = "Mexico",
    "Panamá" = "Panama",
    "Perú" = "Peru",
    "República Dominicana" = "Dominican Republic",
    "Anguila" = "Anguilla",
    "Antigua y Barbuda" = "Antigua and Barbuda",
    "Bonaire, San Eustaquio y Saba" = "Bonaire, Saint Eustatius and Saba",
    "Curazao" = "Curacao",
    "Granada" = "Grenada",
    "Guadalupe" = "Guadeloupe",
    "Islas Caimán" = "Cayman Islands",
    "Isla de San Martín (Francia)" = "Saint Martin",
    "Isla de San Martín (Holanda)" = "Sint Maarten",
    "Islas Turcas y Caicos" = "Turks and Caicos Islands",
    "Islas Vírgenes (EUA)" = "Virgin Islands (US)",
    "Islas Vírgenes (RU)" = "Virgin Islands (UK)",
    "Martinica" = "Martinique",
    "Saint Kitts y Nevis" = "Saint Kitts and Nevis",
    "San Bartolomé" = "Saint Barthelemy",
    "San Vicente y las Granadinas" = "Saint Vincent and the Grenadines",
    "Santa Lucía" = "Saint Lucia",
    "Trinidad y Tobago" = "Trinidad and Tobago",
    "Guayana Francesa" = "French Guiana"
  )
  df$country <- dplyr::recode(df$country, !!!country_map)
  df$iso3c <- countrycode::countrycode(df$country, "country.name", "iso3c")
  df
}

#' Assign PAHO epidemiological weeks to calendar months (pipeline rule).
#'
#' Week-ending Sunday defines the month: if the week ends in the first three
#' days of a month, it is assigned to the previous month. Matches the intent of
#' \code{compute_monthcumm_cases()} in \code{00_FUN_paho_data_process.R}.
#'
#' @param df Data frame with \code{s} (epi year) and \code{t} (EW).
#' @return Same data frame with \code{month_s}, \code{month_t}, \code{month_date}.
#' @export
paho_assign_epiweek_to_month <- function(df) {
  df %>%
    mutate(
      onset_date = ISOweek::ISOweek2date(
        paste0(s, "-W", stringr::str_pad(t, 2, pad = "0"), "-1")
      ),
      week_end = onset_date + 6L,
      month_date = if_else(
        day(week_end) <= 3L,
        week_end - days(day(week_end)),
        week_end
      ),
      month_s = year(month_date),
      month_t = month(month_date)
    )
}

# ---------------------------------------------------------------------------
# PAHO fast load (aligned with DENV_data_delay PAHO_crawler_dataPROC cache)
# ---------------------------------------------------------------------------

# Snapshot history to load: ~18 months delay analysis + ~12 months reporting lag
.paho_default_snapshot_months <- 30L

.paho_default_cache_dir <- function() {
  env_path <- Sys.getenv("PAHO_CACHE_DIR", unset = "")
  if (nzchar(env_path) && dir.exists(env_path)) return(env_path)

  delay_proc <- path.expand("~/Dropbox/DMMG/DENV_data_delay/PAHO_crawler_dataPROC")
  if (dir.exists(delay_proc)) return(delay_proc)

  file.path("Output", "cache", "paho_crawler")
}

.paho_parse_date_arg <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "Date")) return(x)
  as.Date(as.character(x), format = "%Y%m%d")
}

.paho_select_snapshot_file <- function(file_names) {
  preferred <- grep("^PAHO_.*\\.csv$", file_names, ignore.case = TRUE, value = TRUE)
  if (length(preferred) > 0) {
    return(preferred[which.max(nchar(preferred))])
  }
  hits <- grep("\\.(csv|tsv|txt)$", file_names, ignore.case = TRUE, value = TRUE)
  if (length(hits) == 0) NA_character_ else hits[1]
}

.paho_standardize_rows <- function(df, tr) {
  if (!"ID" %in% names(df)) {
    df$ID <- paste0(df$country, "-", df$year, "-", df$EW)
  }
  df %>%
    dplyr::filter(.data$ID != "Grand Total") %>%
    dplyr::filter(!.data$country %in% .paho_subregion_names()) %>%
    dplyr::filter(!is.na(.data$EW), !is.na(.data$year)) %>%
    .paho_normalize_country() %>%
    dplyr::mutate(
      source = "PAHO",
      s = as.integer(.data$year),
      t = as.integer(.data$EW),
      tr = as.Date(tr),
      time_resolution = "weekly",
      total_den = suppressWarnings(as.numeric(.data$total_den))
    ) %>%
    dplyr::select(
      dplyr::any_of(c(
        "source", "country", "iso3c", "s", "t", "tr",
        "time_resolution", "total_den"
      ))
    )
}

#' Read one pre-processed PAHO daily file (UTF-8 CSV, DENV_data_delay format).
.paho_parse_processed_csv <- function(file_path) {
  tr <- as.Date(
    sub("^PAHO([0-9]{8})\\.csv$", "\\1", basename(file_path)),
    format = "%Y%m%d"
  )
  if (is.na(tr)) return(NULL)

  df <- readr::read_csv(file_path, show_col_types = FALSE) %>%
    dplyr::select(dplyr::any_of(c(
      "ID", "country", "year", "EW", "total_den", "iso3c"
    )))
  .paho_standardize_rows(df, tr)
}

.paho_list_cached_snapshots <- function(cache_dir, date_start, date_end) {
  files <- list.files(
    cache_dir,
    pattern = "^PAHO[0-9]{8}\\.csv$",
    full.names = TRUE
  )
  if (length(files) == 0) return(character())

  tr <- as.Date(sub("^PAHO([0-9]{8})\\.csv$", "\\1", basename(files)), format = "%Y%m%d")
  keep <- !is.na(tr)
  if (!is.null(date_start)) keep <- keep & tr >= date_start
  if (!is.null(date_end)) keep <- keep & tr <= date_end
  files[keep]
}

.paho_load_from_cache <- function(cache_dir, date_start, date_end) {
  files <- .paho_list_cached_snapshots(cache_dir, date_start, date_end)
  if (length(files) == 0) return(NULL)

  parsed <- lapply(files, .paho_parse_processed_csv)
  parsed <- parsed[!vapply(parsed, is.null, logical(1))]
  if (length(parsed) == 0) return(NULL)
  dplyr::bind_rows(parsed)
}

.paho_write_processed_cache <- function(df, cache_dir, tr) {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  out_path <- file.path(cache_dir, sprintf("PAHO%s.csv", format(tr, "%Y%m%d")))
  df %>%
    dplyr::mutate(
      year = .data$s,
      EW = .data$t,
      ext_date = format(tr, "%Y%m%d"),
      ID = paste0(.data$country, "-", .data$s, "-", .data$t)
    ) %>%
    dplyr::select(
      ID, country, year, EW, total_den, iso3c, ext_date
    ) %>%
    readr::write_csv(out_path)
  invisible(out_path)
}

.paho_download_snapshots <- function(repo,
                                    date_start,
                                    date_end,
                                    cache_dir = NULL,
                                    write_cache = FALSE) {
  contents <- .github_list(repo, "data")
  if (nrow(contents) == 0) return(NULL)

  dirs <- contents[
    contents$type == "dir" & grepl("^DL_[0-9]{8}$", contents$name),
    ,
    drop = FALSE
  ]
  if (nrow(dirs) == 0) return(NULL)

  dirs$tr <- as.Date(sub("^DL_", "", dirs$name), "%Y%m%d")
  dirs <- dirs[!is.na(dirs$tr), , drop = FALSE]
  if (!is.null(date_start)) dirs <- dirs[dirs$tr >= date_start, , drop = FALSE]
  if (!is.null(date_end)) dirs <- dirs[dirs$tr <= date_end, , drop = FALSE]
  if (nrow(dirs) == 0) return(NULL)

  parse_paho_file <- function(file_path, tr) {
    col_map <- .paho_col_map()
    df <- readr::read_tsv(
      file_path,
      locale = readr::locale(encoding = "UTF-16LE"),
      show_col_types = FALSE
    )
    names(df) <- make.names(names(df))
    matched <- intersect(names(df), names(col_map))
    if (length(matched) == 0) return(NULL)

    df <- df %>%
      dplyr::rename_with(~ col_map[.x], .cols = matched)
    if (!all(c("country", "year", "EW", "total_den") %in% names(df))) return(NULL)
    .paho_standardize_rows(df, tr)
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

  parsed_list <- vector("list", nrow(dirs))

  for (i in seq_len(nrow(dirs))) {
    tr <- dirs$tr[i]
    sub_path <- paste0("data/", dirs$name[i])
    sub_contents <- .github_list(repo, sub_path)
    if (nrow(sub_contents) == 0) next

    pick <- .paho_select_snapshot_file(sub_contents$name)
    if (is.na(pick)) next

    row <- sub_contents[sub_contents$name == pick, , drop = FALSE]
    url <- row$download_url[1]
    if (is.na(url)) next

    ext <- tools::file_ext(pick)
    tmp <- tempfile(pattern = "paho_", fileext = paste0(".", ext))
    snap <- download_parse(url, tmp, function(p) parse_paho_file(p, tr))
    if (is.null(snap) || nrow(snap) == 0) next

    if (write_cache && !is.null(cache_dir)) {
      .paho_write_processed_cache(snap, cache_dir, tr)
    }
    parsed_list[[i]] <- snap
  }

  parsed_list <- parsed_list[!vapply(parsed_list, is.null, logical(1))]
  if (length(parsed_list) == 0) return(NULL)
  dplyr::bind_rows(parsed_list)
}

# ---------------------------------------------------------------------------
# download_and_standardise()
# ---------------------------------------------------------------------------

#' Download WHO Global, SEARO, or PAHO crawler snapshots and return a unified table.
#'
#' PAHO is optimised for speed: reads pre-processed daily CSVs when available
#' (as in DENV_data_delay \code{PAHO_crawler_dataPROC}), otherwise downloads one
#' consolidated file per snapshot day from GitHub and optionally writes cache files.
#'
#' @param source \code{"WHO_Global"}, \code{"WHO"}, \code{"SEARO"}, or \code{"PAHO"}.
#' @param date_start,date_end Optional Date (or \code{YYYYMMDD}) window for PAHO snapshots.
#'   Default start is \code{date_end - 30 months} (18-month delay span plus
#'   12-month reporting lag before validation).
#' @param cache_dir PAHO cache folder; defaults to \code{PAHO_CACHE_DIR}, then
#'   DENV_data_delay processed folder, then \code{Output/cache/paho_crawler}.
#' @param use_cache If TRUE (PAHO), load from cache when possible.
#' @param refresh_cache If TRUE (PAHO), ignore cache and re-download GitHub snapshots.
#' @return Data frame with \code{source}, \code{country}, \code{iso3c},
#'   \code{s}, \code{t}, \code{tr}, \code{time_resolution}, \code{total_den}.
#' @examples
#' \dontrun{
#' who   <- download_and_standardise("WHO_Global")
#' searo <- download_and_standardise("SEARO")
#' paho  <- download_and_standardise("PAHO")  # default: last 30 months of snapshots
#' }
download_and_standardise <- function(source,
                                     date_start = NULL,
                                     date_end = NULL,
                                     cache_dir = NULL,
                                     use_cache = TRUE,
                                     refresh_cache = FALSE) {
  source <- match.arg(source, c("WHO_Global", "WHO", "SEARO", "PAHO"))
  if (source == "WHO") source <- "WHO_Global"

  repo <- switch(source,
    WHO_Global = "DengueGlobalObservatory/WHOGlobal-crawler",
    SEARO      = "DengueGlobalObservatory/SEARO-crawler",
    PAHO       = "DengueGlobalObservatory/PAHO-crawler"
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

  if (source == "PAHO") {
    d_start <- .paho_parse_date_arg(date_start)
    d_end <- .paho_parse_date_arg(date_end)
    if (is.null(d_end)) d_end <- Sys.Date()
    if (is.null(d_start)) {
      d_start <- d_end %m-% months(.paho_default_snapshot_months)
    }

    if (is.null(cache_dir)) cache_dir <- .paho_default_cache_dir()

    out <- NULL
    if (use_cache && !refresh_cache) {
      out <- .paho_load_from_cache(cache_dir, d_start, d_end)
    }

    if (is.null(out) || nrow(out) == 0) {
      out <- .paho_download_snapshots(
        repo = repo,
        date_start = d_start,
        date_end = d_end,
        cache_dir = cache_dir,
        write_cache = TRUE
      )
    }

    if (is.null(out) || nrow(out) == 0) return(empty_df())
    return(out)
  } else if (source == "SEARO") {
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
# paho  <- download_and_standardise("PAHO")
