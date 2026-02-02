#' Country Tracking Utility Functions
#' 
#' Functions to track countries through the pipeline steps

get_tracking_env <- function() {
  if (!exists(".country_tracking_env", envir = .GlobalEnv, inherits = FALSE)) {
    assign(".country_tracking_env", new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  get(".country_tracking_env", envir = .GlobalEnv, inherits = FALSE)
}

#' Initialize country tracking
#' 
#' @param countries_df Data frame with country and iso3 columns
#' @param step_name Name of the step (e.g., "Step_2_Initial_Data")
initialize_country_tracking <- function(countries_df, step_name = "Step_2_Initial_Data") {
  env <- get_tracking_env()
  
  # Validate input
  if (is.null(countries_df) || !is.data.frame(countries_df) || nrow(countries_df) == 0) {
    warning("Cannot initialize country tracking: invalid or empty data frame")
    return(invisible(NULL))
  }
  
  # Extract unique countries with iso3
  if ("iso3" %in% names(countries_df) && "country" %in% names(countries_df)) {
    initial_countries <- tryCatch({
      countries_df %>%
        dplyr::select(country, iso3) %>%
        dplyr::filter(!is.na(iso3)) %>%
        dplyr::distinct()
    }, error = function(e) {
      warning("Error extracting countries from data frame: ", conditionMessage(e))
      return(data.frame(country = character(), iso3 = character()))
    })
  } else {
    # Try to find country identifier columns
    country_col <- NULL
    iso3_col <- NULL
    
    if ("country" %in% names(countries_df)) country_col <- "country"
    if ("iso3" %in% names(countries_df)) iso3_col <- "iso3"
    if ("Country" %in% names(countries_df)) country_col <- "Country"
    if ("ISO_A0" %in% names(countries_df)) iso3_col <- "ISO_A0"
    if ("adm_0_name" %in% names(countries_df) && is.null(country_col)) country_col <- "adm_0_name"
    
    if (is.null(country_col) || is.null(iso3_col)) {
      warning("Cannot find country and iso3 columns in data frame")
      return(invisible(NULL))
    }
    
    initial_countries <- tryCatch({
      result <- countries_df %>%
        dplyr::select(dplyr::all_of(c(country_col, iso3_col))) %>%
        dplyr::filter(!is.na(.data[[iso3_col]])) %>%
        dplyr::distinct()
      names(result)[names(result) == country_col] <- "country"
      names(result)[names(result) == iso3_col] <- "iso3"
      result
    }, error = function(e) {
      warning("Error extracting countries from data frame: ", conditionMessage(e))
      return(data.frame(country = character(), iso3 = character()))
    })
  }
  
  # Check if we got any countries
  if (nrow(initial_countries) == 0) {
    warning("No valid countries found for tracking initialization")
    return(invisible(NULL))
  }
  
  # Group by iso3 and standardize country name (use sentence case, prefer non-ALL-CAPS)
  # This ensures one row per iso3 code
  tracking_df <- tryCatch({
    # First, add a flag to identify ALL CAPS vs sentence case
    initial_countries_flagged <- initial_countries %>%
      dplyr::mutate(
        is_all_caps = !is.na(country) & 
          country == toupper(country) & 
          country != tolower(country) &
          nchar(gsub("[^A-Za-z]", "", country)) > 0
      )
    
    # Group by iso3, prefer sentence case names
    initial_countries_flagged %>%
      dplyr::group_by(iso3) %>%
      dplyr::summarise(
        # Prefer non-ALL-CAPS version if available, otherwise take first
        country = dplyr::case_when(
          any(!is_all_caps, na.rm = TRUE) ~ 
            dplyr::first(country[!is_all_caps], na_rm = TRUE),
          TRUE ~ dplyr::first(country, na_rm = TRUE)
        ),
        .groups = "drop"
      ) %>%
      # Convert remaining ALL CAPS to sentence case
      dplyr::mutate(
        country = dplyr::if_else(
          !is.na(country) & 
            country == toupper(country) & 
            country != tolower(country) &
            nchar(gsub("[^A-Za-z]", "", country)) > 0,
          # Convert to sentence case: first letter uppercase, rest lowercase
          paste0(toupper(substring(country, 1, 1)), tolower(substring(country, 2))),
          country
        ),
        step_2_initial_data = TRUE,
        step_3a_who_od_clean = FALSE,
        step_3a_who_od_selection = FALSE,
        step_3b_combined_before_filter = FALSE,
        step_3b_combined_after_filter = FALSE,
        step_3c_seasonal_before_filter = FALSE,
        step_3c_seasonal_after_filter = FALSE,
        step_4_current_data = FALSE,
        step_5_nowcast_merge = FALSE,
        step_6_final = FALSE,
        dropped_at_step = NA_character_,
        drop_reason = NA_character_
      )
  }, error = function(e) {
    # Fallback: simple group by iso3, take first country name and convert to sentence case
    tryCatch({
      initial_countries %>%
        dplyr::group_by(iso3) %>%
        dplyr::summarise(
          country = dplyr::first(country, na_rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          # Convert ALL CAPS to sentence case
          country = dplyr::if_else(
            !is.na(country) & country == toupper(country) & country != tolower(country),
            paste0(toupper(substring(country, 1, 1)), tolower(substring(country, 2))),
            country
          ),
          step_2_initial_data = TRUE,
          step_3a_who_od_clean = FALSE,
          step_3a_who_od_selection = FALSE,
          step_3b_combined_before_filter = FALSE,
          step_3b_combined_after_filter = FALSE,
          step_3c_seasonal_before_filter = FALSE,
          step_3c_seasonal_after_filter = FALSE,
          step_4_current_data = FALSE,
          step_5_nowcast_merge = FALSE,
          step_6_final = FALSE,
          dropped_at_step = NA_character_,
          drop_reason = NA_character_
        )
    }, error = function(e2) {
      warning("Error initializing tracking data frame: ", conditionMessage(e2))
      return(NULL)
    })
  })
  
  if (is.null(tracking_df) || nrow(tracking_df) == 0) {
    return(invisible(NULL))
  }
  
  env$tracking_df <- tracking_df
  env$initialized <- TRUE
  
  tryCatch({
    if (exists("log_message")) {
      log_message(sprintf("Country tracking initialized with %d unique countries (by iso3)", nrow(tracking_df)))
    }
  }, error = function(e) {
    # Silently fail - logging shouldn't stop initialization
  })
  
  invisible(tracking_df)
}

#' Record country presence at a step
#' 
#' @param countries_df Data frame with country and iso3 columns
#' @param step_name Name of the step (must match column name pattern)
#' @param drop_reason Optional reason if countries are being dropped (single value for all, or data frame with iso3 and drop_reason columns for country-specific reasons)
record_countries_at_step <- function(countries_df, step_name, drop_reason = NA_character_) {
  # Silently return if tracking not initialized - don't stop pipeline
  env <- get_tracking_env()
  
  if (!isTRUE(env$initialized)) {
    return(invisible(NULL))
  }
  
  # Validate input
  if (is.null(countries_df) || !is.data.frame(countries_df) || nrow(countries_df) == 0) {
    return(invisible(NULL))
  }
  
  # Extract countries from data frame - only need iso3 for matching
  # Country names are just for reference, we match on iso3 only
  if ("iso3" %in% names(countries_df)) {
    step_countries <- tryCatch({
      countries_df %>%
        dplyr::select(iso3) %>%
        dplyr::distinct() %>%
        dplyr::filter(!is.na(iso3))
    }, error = function(e) {
      return(data.frame(iso3 = character()))
    })
  } else if ("iso3" %in% names(countries_df) && "country" %in% names(countries_df)) {
    step_countries <- tryCatch({
      countries_df %>%
        dplyr::select(country, iso3) %>%
        dplyr::distinct() %>%
        dplyr::filter(!is.na(iso3))
    }, error = function(e) {
      return(data.frame(country = character(), iso3 = character()))
    })
  } else {
    # Try alternative column names
    country_col <- NULL
    iso3_col <- NULL
    
    if ("country" %in% names(countries_df)) country_col <- "country"
    if ("iso3" %in% names(countries_df)) iso3_col <- "iso3"
    if ("Country" %in% names(countries_df)) country_col <- "Country"
    if ("ISO_A0" %in% names(countries_df)) iso3_col <- "ISO_A0"
    if ("adm_0_name" %in% names(countries_df) && is.null(country_col)) country_col <- "adm_0_name"
    
    if (is.null(country_col) || is.null(iso3_col)) {
      return(invisible(NULL))
    }
    
    step_countries <- tryCatch({
      result <- countries_df %>%
        dplyr::select(dplyr::all_of(iso3_col)) %>%
        dplyr::distinct() %>%
        dplyr::filter(!is.na(.data[[iso3_col]]))
      names(result)[names(result) == iso3_col] <- "iso3"
      result
    }, error = function(e) {
      return(data.frame(iso3 = character()))
    })
  }
  
  # If no countries extracted, silently return
  if (nrow(step_countries) == 0 || !"iso3" %in% names(step_countries)) {
    return(invisible(NULL))
  }
  
  # Map step names to column names
  step_col_map <- list(
    "Step_2_Initial_Data" = "step_2_initial_data",
    "Step_3a_WHO_OD_Clean" = "step_3a_who_od_clean",
    "Step_3a_WHO_OD_Selection" = "step_3a_who_od_selection",
    "Step_3b_Combined_Before_Filter" = "step_3b_combined_before_filter",
    "Step_3b_Combined_After_Filter" = "step_3b_combined_after_filter",
    "Step_3c_Seasonal_Before_Filter" = "step_3c_seasonal_before_filter",
    "Step_3c_Seasonal_After_Filter" = "step_3c_seasonal_after_filter",
    "Step_4_Current_Data" = "step_4_current_data",
    "Step_5_Nowcast_Merge" = "step_5_nowcast_merge",
    "Step_6_Final" = "step_6_final"
  )
  
  # Find matching column name
  step_col_final <- step_col_map[[step_name]]
  if (is.null(step_col_final)) {
    # Try normalized version
    step_col <- tolower(step_name)
    step_col <- gsub("[^a-z0-9]", "_", step_col)
    step_col <- gsub("_+", "_", step_col)
    step_col_final <- step_col_map[[step_col]]
    if (is.null(step_col_final)) {
      # If still can't map, silently return - don't stop pipeline
      return(invisible(NULL))
    }
  }
  
  # Ensure column exists and tracking_df is valid
  if (!is.data.frame(env$tracking_df) || nrow(env$tracking_df) == 0) {
    return(invisible(NULL))
  }
  
  if (!step_col_final %in% names(env$tracking_df)) {
    env$tracking_df[[step_col_final]] <- FALSE
  }
  
  # Update tracking data frame safely
  tryCatch({
    env$tracking_df[[step_col_final]] <- env$tracking_df$iso3 %in% step_countries$iso3
  }, error = function(e) {
    # Silently fail - don't stop pipeline
    return(invisible(NULL))
  })
  
  # Identify dropped countries (countries that were in previous steps but not in current step)
  # Only update if drop_reason is provided
  has_drop_reason <- if (is.data.frame(drop_reason)) {
    nrow(drop_reason) > 0
  } else {
    !is.na(drop_reason)
  }
  
  if (has_drop_reason) {
    tryCatch({
      # Find countries that were present in at least one previous step but not in current step
      previous_steps <- c("step_2_initial_data", "step_3a_who_od_clean", "step_3a_who_od_selection",
                         "step_3b_combined_before_filter", "step_3b_combined_after_filter",
                         "step_3c_seasonal_before_filter", "step_3c_seasonal_after_filter",
                         "step_4_current_data", "step_5_nowcast_merge")
      previous_steps <- previous_steps[previous_steps != step_col_final]
      previous_steps <- previous_steps[previous_steps %in% names(env$tracking_df)]
      
      if (length(previous_steps) > 0) {
        # For seasonal filtering, specifically check if they were in the before_filter step
        # For other steps, check if they were in any previous step
        if (step_col_final == "step_3c_seasonal_after_filter" && "step_3c_seasonal_before_filter" %in% names(env$tracking_df)) {
          # Countries that were in before_filter but not in after_filter
          dropped_iso3s <- env$tracking_df %>%
            dplyr::filter(
              # Was in before_filter step
              step_3c_seasonal_before_filter == TRUE &
              # But not in current step
              !(iso3 %in% step_countries$iso3) &
              # Haven't been marked as dropped yet
              is.na(dropped_at_step)
            ) %>%
            dplyr::pull(iso3)
        } else {
          # Countries that were in previous steps but not in current step
          dropped_iso3s <- env$tracking_df %>%
            dplyr::filter(
              # Was in at least one previous step
              (rowSums(dplyr::select(., dplyr::all_of(previous_steps)), na.rm = TRUE) > 0) &
              # But not in current step
              !(iso3 %in% step_countries$iso3) &
              # Haven't been marked as dropped yet
              is.na(dropped_at_step)
            ) %>%
            dplyr::pull(iso3)
        }
        
        if (length(dropped_iso3s) > 0) {
          # Check if drop_reason is a data frame with country-specific reasons
          if (is.data.frame(drop_reason) && "iso3" %in% names(drop_reason) && "drop_reason" %in% names(drop_reason)) {
            # Use country-specific drop reasons
            drop_reasons_map <- drop_reason %>%
              dplyr::select(iso3, drop_reason) %>%
              dplyr::filter(iso3 %in% dropped_iso3s)
            
            env$tracking_df <- env$tracking_df %>%
              dplyr::left_join(drop_reasons_map, by = "iso3", suffix = c("", "_new")) %>%
              dplyr::mutate(
                dropped_at_step = dplyr::if_else(
                  iso3 %in% dropped_iso3s & is.na(dropped_at_step),
                  step_name,
                  dropped_at_step
                ),
                drop_reason = dplyr::if_else(
                  iso3 %in% dropped_iso3s & is.na(drop_reason) & !is.na(drop_reason_new),
                  drop_reason_new,
                  drop_reason
                )
              ) %>%
              dplyr::select(-drop_reason_new)
          } else {
            # Use single drop_reason for all dropped countries
            env$tracking_df <- env$tracking_df %>%
              dplyr::mutate(
                dropped_at_step = dplyr::if_else(
                  iso3 %in% dropped_iso3s & is.na(dropped_at_step),
                  step_name,
                  dropped_at_step
                ),
                drop_reason = dplyr::if_else(
                  iso3 %in% dropped_iso3s & is.na(drop_reason),
                  drop_reason,
                  drop_reason
                )
              )
          }
        }
      }
    }, error = function(e) {
      # Silently fail - don't stop pipeline
    })
  }
  
  # Log result if possible, but don't fail if logging fails
  tryCatch({
    n_present <- sum(env$tracking_df[[step_col_final]], na.rm = TRUE)
    if (exists("log_message")) {
      log_message(sprintf("Step %s: %d countries present", step_name, n_present))
    }
  }, error = function(e) {
    # Silently fail
  })
  
  invisible(env$tracking_df)
}

#' Get current tracking data frame
get_tracking_df <- function() {
  env <- get_tracking_env()
  if (!isTRUE(env$initialized)) {
    return(NULL)
  }
  return(env$tracking_df)
}

#' Export tracking CSV
#' 
#' @param output_path Path to save the CSV file
export_country_tracking <- function(output_path) {
  env <- get_tracking_env()
  
  if (!isTRUE(env$initialized)) {
    warning("Country tracking not initialized. Nothing to export.")
    return(invisible(NULL))
  }
  
  # Add final status
  tracking_export <- env$tracking_df %>%
    dplyr::mutate(
      final_status = dplyr::case_when(
        step_6_final ~ "Included",
        step_5_nowcast_merge ~ "Dropped after nowcast merge",
        step_4_current_data ~ "Dropped after current data selection",
        step_3c_seasonal_after_filter ~ "Dropped after seasonal filtering",
        step_3c_seasonal_before_filter ~ "Dropped during seasonal filtering",
        step_3b_combined_after_filter ~ "Dropped after combined filtering",
        step_3b_combined_before_filter ~ "Dropped during combined filtering",
        step_3a_who_od_selection ~ "Dropped during WHO/OD selection",
        step_3a_who_od_clean ~ "Dropped during WHO/OD cleaning",
        step_2_initial_data ~ "Dropped after initial data load",
        TRUE ~ "Unknown"
      )
    ) %>%
    dplyr::arrange(country)
  
  # Create summary statistics
  summary_stats <- dplyr::tibble(
    step = c(
      "Step_2_Initial_Data",
      "Step_3a_WHO_OD_Clean",
      "Step_3a_WHO_OD_Selection",
      "Step_3b_Combined_Before_Filter",
      "Step_3b_Combined_After_Filter",
      "Step_3c_Seasonal_Before_Filter",
      "Step_3c_Seasonal_After_Filter",
      "Step_4_Current_Data",
      "Step_5_Nowcast_Merge",
      "Step_6_Final"
    ),
    n_countries = c(
      sum(tracking_export$step_2_initial_data, na.rm = TRUE),
      sum(tracking_export$step_3a_who_od_clean, na.rm = TRUE),
      sum(tracking_export$step_3a_who_od_selection, na.rm = TRUE),
      sum(tracking_export$step_3b_combined_before_filter, na.rm = TRUE),
      sum(tracking_export$step_3b_combined_after_filter, na.rm = TRUE),
      sum(tracking_export$step_3c_seasonal_before_filter, na.rm = TRUE),
      sum(tracking_export$step_3c_seasonal_after_filter, na.rm = TRUE),
      sum(tracking_export$step_4_current_data, na.rm = TRUE),
      sum(tracking_export$step_5_nowcast_merge, na.rm = TRUE),
      sum(tracking_export$step_6_final, na.rm = TRUE)
    )
  )
  
  # Write main tracking file
  readr::write_csv(tracking_export, file = output_path)
  
  # Write summary file
  summary_path <- sub("\\.csv$", "_summary.csv", output_path)
  readr::write_csv(summary_stats, file = summary_path)
  
  if (exists("log_message")) {
    log_message(sprintf("Country tracking exported to: %s", output_path))
    log_message(sprintf("Summary statistics exported to: %s", summary_path))
  }
  
  invisible(list(tracking = tracking_export, summary = summary_stats))
}


