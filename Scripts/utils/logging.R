get_log_env <- function() {
  if (!exists(".v1_log_env", envir = .GlobalEnv, inherits = FALSE)) {
    assign(".v1_log_env", new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  get(".v1_log_env", envir = .GlobalEnv, inherits = FALSE)
}

`%+%` <- function(a, b) paste0(a, b)

log_message <- function(msg, level = "INFO") {
  env <- get_log_env()
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  entry <- sprintf("[%s] %s: %s", timestamp, level, msg)

  if (!is.null(env$log_con)) {
    tryCatch({
      writeLines(entry, env$log_con, sep = "\n")
      flush(env$log_con)
    }, error = function(e) {
      warning("Failed to write to log file: ", conditionMessage(e), call. = FALSE)
    })
  }

  if (isTRUE(env$console) || is.null(env$log_con)) {
    cat(entry, "\n")
  }

  invisible(entry)
}

logger_active <- function() {
  env <- get_log_env()
  isTRUE(env$initialized)
}

start_logging <- function(log_dir = NULL,
                          log_prefix = "pipeline_log",
                          console = TRUE) {
  env <- get_log_env()
  if (isTRUE(env$initialized)) {
    return(invisible(env$log_file))
  }

  env$console <- console
  env$initialized <- TRUE
  env$log_file <- NULL
  env$log_con <- NULL
  env$orig_warning <- getOption("warning.expression")
  env$orig_error <- getOption("error")

  if (!is.null(log_dir)) {
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    }
    log_path <- file.path(
      log_dir,
      paste0(log_prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    )
    env$log_file <- log_path
    env$log_con <- file(log_path, open = "wt")
  }

  options(
    warning.expression = quote(log_message(geterrmessage(), level = "WARNING")),
    error = function(e) {
      log_message(paste("ERROR:", conditionMessage(e)), level = "ERROR")
      stop(e)
    }
  )

  log_message("Logging initialized.")
  invisible(env$log_file)
}

ensure_logger <- function(log_dir = NULL,
                          log_prefix = "pipeline_log",
                          console = TRUE) {
  if (!logger_active()) {
    start_logging(log_dir = log_dir, log_prefix = log_prefix, console = console)
  }
  invisible(get_log_env()$log_file)
}

log_file_path <- function() {
  env <- get_log_env()
  env$log_file
}

close_logger <- function() {
  env <- get_log_env()
  if (!isTRUE(env$initialized)) {
    return(invisible(NULL))
  }

  log_message("Closing logger.")

  if (!is.null(env$orig_warning)) {
    options(warning.expression = env$orig_warning)
  } else {
    options(warning.expression = NULL)
  }

  if (!is.null(env$orig_error)) {
    options(error = env$orig_error)
  } else {
    options(error = NULL)
  }

  if (!is.null(env$log_con)) {
    try(close(env$log_con), silent = TRUE)
  }

  env$initialized <- FALSE
  env$log_con <- NULL
  env$log_file <- NULL
  invisible(NULL)
}

