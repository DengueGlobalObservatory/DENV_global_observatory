# --- Utility: Build data-driven story points for the global scrollytelling block ---
build_global_story_points <- function(summary_df) {
  empty_story <- dplyr::tibble(
    step = integer(),
    text = character()
  )
  
  if (is.null(summary_df) || nrow(summary_df) == 0) {
    return(empty_story)
  }
  
  summary_df <- summary_df %>%
    dplyr::arrange(date) %>%
    dplyr::filter(!is.na(cases))
  
  if (nrow(summary_df) == 0) {
    return(empty_story)
  }
  
  latest <- summary_df %>% dplyr::slice_tail(n = 1)
  previous <- summary_df %>%
    dplyr::filter(date < latest$date) %>%
    dplyr::slice_tail(n = 1)
  
  format_big <- function(value, digits = 0) {
    if (is.null(value) || is.na(value)) return(NA_character_)
    format(round(value, digits), big.mark = ",", trim = TRUE, scientific = FALSE)
  }
  
  # Get current year
  current_year <- as.integer(format(latest$date, "%Y"))
  last_month_label <- format(latest$date, "%B")
  
  # Calculate monthly ratio (cases in last month vs expected for that month)
  monthly_ratio_value <- if (!is.na(latest$Ave_season_monthly_cases) && latest$Ave_season_monthly_cases > 0) {
    latest$cases / latest$Ave_season_monthly_cases
  } else {
    NA_real_
  }
  monthly_ratio_label <- if (!is.na(monthly_ratio_value)) sprintf("%.1f", monthly_ratio_value) else NA_character_
  
  # Calculate YTD values
  ytd_df <- summary_df %>% dplyr::filter(date <= latest$date)
  ytd_cases <- sum(ytd_df$cases, na.rm = TRUE)
  ytd_baseline <- sum(ytd_df$Ave_season_monthly_cases, na.rm = TRUE)
  ytd_cases_label <- format_big(ytd_cases)
  ytd_ratio_value <- if (ytd_baseline > 0) ytd_cases / ytd_baseline else NA_real_
  ytd_ratio_label <- if (!is.na(ytd_ratio_value)) sprintf("%.1f", ytd_ratio_value) else NA_character_
  
  # Helper function to classify modifier
  classify_modifier <- function(ratio) {
    if (is.na(ratio)) return(list(text = "near", class = "near"))
    if (ratio < 0.9) return(list(text = "below", class = "below"))
    if (ratio > 1.1) return(list(text = "above", class = "above"))
    return(list(text = "near", class = "near"))
  }
  
  # Build the single unified story point with all 5 pieces of information
  latest_cases_label <- format_big(latest$cases)
  prev_month_label <- if (!is.null(previous) && nrow(previous) == 1) format(previous$date, "%B") else NA_character_
  
  # Determine increase/decrease
  if (!is.null(previous) && nrow(previous) == 1 && !is.na(previous$cases) && previous$cases > 0 && !is.na(latest$cases)) {
    delta_pct_value <- (latest$cases - previous$cases) / previous$cases
    if (delta_pct_value < -0.05) {
      direction_text <- "decrease"
      direction_class <- "decrease"
    } else if (delta_pct_value > 0.05) {
      direction_text <- "increase"
      direction_class <- "increase"
    } else {
      direction_text <- "similar level"
      direction_class <- ""
    }
  } else {
    direction_text <- "similar level"
    direction_class <- ""
  }
  
  modifier_info <- classify_modifier(ytd_ratio_value)
  
  # Build the complete story text
  story_parts <- character(0)
  
  # Part 1: Monthly cases
  if (!is.na(latest_cases_label)) {
    story_parts <- c(story_parts, 
      glue::glue('Globally we estimate <span class="scrolly-step-number">{latest_cases_label}</span> cases in {last_month_label}.')
    )
  }
  
  # Part 2: Monthly ratio and month-over-month change
  if (!is.na(monthly_ratio_label)) {
    if (!is.na(prev_month_label) && direction_class != "") {
      story_parts <- c(story_parts,
        glue::glue('This represents <span class="scrolly-step-number">{monthly_ratio_label}×</span> the expected number of cases for this time of the year. This is an <span class="scrolly-modifier {direction_class}">{direction_text}</span> over the number of cases reported in {prev_month_label}.')
      )
    } else if (!is.na(prev_month_label)) {
      story_parts <- c(story_parts,
        glue::glue('This represents <span class="scrolly-step-number">{monthly_ratio_label}×</span> the expected number of cases for this time of the year. This is a {direction_text} over the number of cases reported in {prev_month_label}.')
      )
    } else {
      story_parts <- c(story_parts,
        glue::glue('This represents <span class="scrolly-step-number">{monthly_ratio_label}×</span> the expected number of cases for this time of the year.')
      )
    }
  }
  
  # Part 3: YTD cases
  if (!is.na(ytd_cases_label)) {
    story_parts <- c(story_parts,
      glue::glue('Globally we estimate <span class="scrolly-step-number">{ytd_cases_label}</span> cases have been reported between January {current_year} and {last_month_label} {current_year}.')
    )
  }
  
  # Part 4: YTD ratio
  if (!is.na(ytd_ratio_label)) {
    story_parts <- c(story_parts,
      glue::glue('This represents <span class="scrolly-step-number">{ytd_ratio_label}×</span> the expected number of cases by this time in the year.')
    )
  }
  
  # Part 5: Season status
  if (!is.na(ytd_ratio_label)) {
    story_parts <- c(story_parts,
      glue::glue('The {current_year} dengue season is <span class="scrolly-modifier {modifier_info$class}">{modifier_info$text}</span> average globally so far.')
    )
  }
  
  # Combine all parts into a single text block with spacing between each part
  if (length(story_parts) > 0) {
    # Wrap each part in a paragraph tag and join with spacing
    story_text <- paste(
      paste0('<p class="scrolly-step-body">', story_parts, '</p>'),
      collapse = ""
    )
  } else {
    story_text <- '<p class="scrolly-step-body">Global case data is still being compiled.</p>'
  }
  
  # Return single-row tibble
  dplyr::tibble(
    step = 1L,
    text = story_text
  )
}

# --- Utility: Render a region-level summary plot with title ---
render_region_summary_plot <- function(plot_list, region_name, show_title = TRUE) {
  # Check that the plot exists
  if (is.null(plot_list[[region_name]])) {
    return(htmltools::div(
      class = "alert alert-warning text-center",
      paste("No region summary available for", region_name)
    ))
  }
  
  # Build the layout
  title_el <- if (show_title) {
    htmltools::div(
      class = "region-summary-title",
      region_name
    )
  } else {
    NULL
  }
  
  htmltools::div(
    class = "region-summary-container",
    title_el,
    htmltools::div(
      class = "region-summary-plot",
      htmltools::plotTag(plot_list[[region_name]]+ ggtitle(" "), 
                         alt = paste("Summary plot for", region_name))
    )
  )
}


render_region_overview <- function(
    region_name,
    region_plot_list,
    region_callouts,
    map_lookup = NULL,
    latest_update_label = NULL
) {
  # Pull supporting assets
  callout_row <- region_callouts %>%
    dplyr::filter(Region == region_name) %>%
    dplyr::slice_head(n = 1)
  
  has_callout <- nrow(callout_row) == 1
  latest_sentence <- if (has_callout) callout_row$latest_sentence else glue::glue("{region_name} summary coming soon.")
  ytd_sentence <- if (has_callout) callout_row$ytd_sentence else "Season-to-date totals are being compiled."
  badge_label <- if (has_callout && !is.na(callout_row$season_badge_label)) callout_row$season_badge_label else "Season overview"
  badge_state <- if (has_callout && !is.na(callout_row$season_badge_state)) callout_row$season_badge_state else "is-neutral"
  
  if (is.null(map_lookup)) {
    map_lookup <- get0("region_map_lookup", ifnotfound = NULL)
  }
  map_src <- if (!is.null(map_lookup) && region_name %in% names(map_lookup)) {
    map_lookup[[region_name]]
  } else {
    NULL
  }
  
  map_element <- if (!is.null(map_src)) {
    htmltools::tags$img(
      src = map_src,
      alt = paste(region_name, "map"),
      class = "region-hero-map-image"
    )
  } else {
    htmltools::div(
      class = "region-hero-map-placeholder",
      "Regional map coming soon"
    )
  }
  
  plot_element <- render_region_summary_plot(
    plot_list = region_plot_list,
    region_name = region_name,
    show_title = FALSE
  )
  
  updated_label <- if (!is.null(latest_update_label)) {
    glue::glue("Data refreshed: {latest_update_label}")
  } else {
    NULL
  }
  
  heading_block <- htmltools::div(
    class = "region-hero-heading-block",
    htmltools::tags$h1(
      region_name,
      class = "region-hero-heading"
    ),
    htmltools::div(
      class = paste("season-badge", badge_state),
      badge_label
    )
  )
  
  text_block <- htmltools::div(
    class = "region-hero-text-block",
    htmltools::tags$p(latest_sentence, class = "region-hero-text-body"),
    htmltools::tags$p(ytd_sentence, class = "region-hero-text-body"),
    if (!is.null(updated_label)) {
      htmltools::tags$small(class = "region-hero-updated", updated_label)
    }
  )
  
  htmltools::div(
    class = "region-hero-grid",
    htmltools::div(
      class = "region-hero-panel region-hero-map",
      map_element
    ),
    htmltools::div(
      class = "region-hero-panel region-hero-plot",
      heading_block,
      plot_element,
      text_block
    )
  )
}


# --- Utility: Render a plot + text row for each country dynamically ---
render_country_plot_text_grid <- function(
    plot_list,
    summary_df,
    title = NULL,
    scrollable = TRUE,
    container_class = "snapshot-grid severity-grid",
    row_class = "snapshot-card severity-card",
    plot_class = "severity-plot",
    text_class = "severity-blurb",
    data_status_df = NULL
) {
  if (is.null(plot_list) || length(plot_list) == 0) {
    return(htmltools::div(
      class = "alert alert-warning",
      "No plots available for this selection."
    ))
  }
  
  section_title <- if (!is.null(title)) {
    htmltools::tags$h4(
      title,
      class = "region-title text-center fw-bold mt-3 mb-3"
    )
  } else {
    htmltools::tags$h4(
      "Countries",
      class = "region-title text-center fw-bold mt-3 mb-3"
    )
  }
  
  # Get country name column from summary_df
  country_name_col <- dplyr::case_when(
    "country" %in% names(summary_df) ~ "country",
    "Country" %in% names(summary_df) ~ "Country",
    TRUE ~ NA_character_
  )
  
  rows <- lapply(names(plot_list), function(country) {
    info <- summary_df %>%
      dplyr::filter(.data[[country_name_col]] == !!country) %>%
      dplyr::slice_head(n = 1)
    
    # Get data status if available
    data_status_msg <- if (!is.null(data_status_df) && nrow(data_status_df) > 0) {
      status_row <- data_status_df %>%
        dplyr::filter(country_name == !!country) %>%
        dplyr::slice_head(n = 1)
      if (nrow(status_row) > 0) {
        status_row$data_status_message[1]
      } else {
        NULL
      }
    } else {
      NULL
    }
    
    if (nrow(info) > 0) {
      # Extract cumulative values for unified text format
      cum_ratio <- info$cum_ratio[1]
      cum_high <- info$cum_high[1]
      
      # Use unified function to generate text
      desc <- severity_country_blurb(
        country = country,
        ratio = cum_ratio,
        cum_cases = cum_high,
        region = NULL,
        region_href = NULL
      )
    } else {
      desc <- glue::glue("No current summary available for {country}.")
    }
    
    # Build card with new style matching all countries page
    card_elements <- list(
      htmltools::tags$h4(country, class = "text-center fw-semibold"),
      htmltools::div(
        class = plot_class,
        htmltools::plotTag(plot_list[[country]], alt = paste("Plot for", country))
      ),
      htmltools::tags$p(
        class = text_class,
        htmltools::HTML(desc)
      )
    )
    
    # Add data status message if available
    if (!is.null(data_status_msg)) {
      card_elements <- append(
        card_elements,
        list(
          htmltools::tags$small(
            class = "severity-footnote text-muted",
            data_status_msg
          )
        )
      )
    }
    
    htmltools::div(
      class = row_class,
      card_elements
    )
  })
  
  container <- htmltools::div(class = container_class, rows)
  
  if (scrollable) {
    container <- htmltools::div(class = "snapshot-region-scroll", container)
  }
  
  htmltools::tagList(section_title, container)
}


# --- Utility: Build severity blurbs for high severity countries ---
severity_country_blurb <- function(country, ratio, cum_cases, region = NULL, region_href = NULL) {
  n <- max(length(country), length(ratio), length(cum_cases))
  if (n == 0) {
    return(character())
  }
  
  country <- rep_len(country, n)
  ratio <- rep_len(ratio, n)
  cum_cases <- rep_len(cum_cases, n)
  region <- if (is.null(region)) rep(NA_character_, n) else rep_len(region, n)
  region_href <- if (is.null(region_href)) rep(NA_character_, n) else rep_len(region_href, n)
  
  safe_country <- ifelse(
    is.na(country) | country == "",
    "This country",
    country
  )
  
  ratio_valid <- !is.na(ratio) & is.finite(ratio)
  ratio_text <- ifelse(ratio_valid, sprintf("%.1f", ratio), NA_character_)
  
  cases_valid <- !is.na(cum_cases) & is.finite(cum_cases)
  cases_text <- ifelse(
    cases_valid,
    format(round(cum_cases), big.mark = ",", trim = TRUE, scientific = FALSE),
    "cases are still being compiled"
  )
  
  # Calculate status (below/near/above) from cum_ratio
  # Thresholds: < 0.9 = "below", > 1.1 = "above", else = "near"
  status_text <- character(n)
  for (i in seq_len(n)) {
    if (!ratio_valid[i]) {
      status_text[i] <- "near"
    } else if (ratio[i] < 0.9) {
      status_text[i] <- "below"
    } else if (ratio[i] > 1.1) {
      status_text[i] <- "above"
    } else {
      status_text[i] <- "near"
    }
  }
  status_class <- tolower(status_text)
  
  # Generate unified text format
  base_sentence <- character(n)
  for (i in seq_len(n)) {
    if (ratio_valid[i] && cases_valid[i]) {
      base_sentence[i] <- glue::glue(
        "{safe_country[i]} is estimated to have experienced <span class=\"scrolly-step-number\">{cases_text[i]}</span> cases this year to date. This is <span class=\"scrolly-step-number\">{ratio_text[i]}×</span> the number of cases reported in an average year. This year the dengue situation in {safe_country[i]} is <span class=\"scrolly-modifier {status_class[i]}\">{status_text[i]}</span> average."
      )
    } else if (!ratio_valid[i]) {
      base_sentence[i] <- glue::glue(
        "{safe_country[i]} data is still loading this year; totals are being compiled."
      )
    } else {
      # cases_valid is FALSE but ratio_valid is TRUE
      base_sentence[i] <- glue::glue(
        "{safe_country[i]} is estimated to have experienced cases this year to date. This year the dengue situation in {safe_country[i]} is <span class=\"scrolly-modifier {status_class[i]}\">{status_text[i]}</span> average."
      )
    }
  }
  
  # Return the unified sentence
  output <- base_sentence
  
  output
}


# --- Utility: Render list of plots as responsive HTML grid using saved PNGs ---
render_plot_grid_static <- function(
    plot_list,
    title = NULL,
    save_dir = "V1/Assets",
    scrollable = TRUE,
    grid_class = "snapshot-grid",
    card_class = "snapshot-card"
) {
  # Ensure output folder exists
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Handle empty lists
  if (is.null(plot_list) || length(plot_list) == 0) {
    return(htmltools::div(
      class = "alert alert-warning",
      "No plots available for this selection."
    ))
  }
  
  # Save each ggplot as PNG
  message("Saving ", length(plot_list), " plots to: ", save_dir)
  png_paths <- purrr::imap(plot_list, function(p, name) {
    # Clean region/country name for safe filename
    file_name <- paste0(gsub("[^A-Za-z0-9_]", "_", name), ".png")
    file_path <- file.path(save_dir, file_name)
    ggsave(file_path, plot = p, width = 6, height = 6, dpi = 150, bg = "white")
    return(file_path)
  })
  
  
  # Compute relative paths for Quarto
  # (remove "V1/" prefix if your .qmd is in the project root)
  # png_paths_rel <- gsub("^V1/", "", png_paths)
  png_paths_rel <-  png_paths
  
  
  # Optionally add title
  section_title <- if (!is.null(title)) {
    htmltools::tags$h4(title, class = "region-title text-center fw-bold mt-3 mb-3")
  } else {
    NULL
  }
  
  # Build grid of static image cards
  plot_cards <- lapply(names(png_paths_rel), function(name) {
    htmltools::div(
      class = card_class,
      htmltools::tags$h6(name, class = "text-center fw-semibold"),
      htmltools::tags$img(
        src = png_paths_rel[[name]],
        alt = paste("Plot for", name),
        style = "width:100%; border-radius:12px; box-shadow:0 2px 4px rgba(0,0,0,0.15);"
      )
    )
  })
  
  # Wrap grid container
  grid_container <- htmltools::div(
    class = grid_class,
    plot_cards
  )
  
  # Optionally make it scrollable
  if (scrollable) {
    grid_container <- htmltools::div(
      class = "snapshot-region-scroll",
      grid_container
    )
  }
  
  # Return title + grid
  htmltools::tagList(section_title, grid_container)
}


# --- Utility: Render a list of country plots as a responsive HTML grid ---
render_plot_grid <- function(
    plot_list,
    title = NULL,
    scrollable = TRUE,
    grid_class = "snapshot-grid",
    card_class = "snapshot-card"
) {
  # Handle empty lists gracefully
  if (is.null(plot_list) || length(plot_list) == 0) {
    return(htmltools::div(
      class = "alert alert-warning",
      "No plots available for this selection."
    ))
  }
  
  # Optionally add section title
  section_title <- if (!is.null(title)) {
    htmltools::tags$h4(title, class = "region-title text-center fw-bold mt-3 mb-3")
  } else {
    NULL
  }
  
  # Create list of country plot cards
  plot_cards <- lapply(names(plot_list), function(country) {
    htmltools::div(
      class = card_class,
      htmltools::tags$h6(country, class = "text-center fw-semibold"),
      htmltools::plotTag(plot_list[[country]], alt = paste("Plot for", country))
    )
  })
  
  # Wrap in grid
  grid_container <- htmltools::div(
    class = grid_class,
    plot_cards
  )
  
  # Wrap in scrollable container if requested
  if (scrollable) {
    grid_container <- htmltools::div(
      class = "snapshot-region-scroll",
      grid_container
    )
  }
  
  # Combine title + grid
  htmltools::tagList(section_title, grid_container)
}


