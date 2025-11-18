# --- Utility: Render a region-level summary plot with title ---
render_region_summary_plot <- function(plot_list, region_name) {
  # Check that the plot exists
  if (is.null(plot_list[[region_name]])) {
    return(htmltools::div(
      class = "alert alert-warning text-center",
      paste("No region summary available for", region_name)
    ))
  }
  
  # Build the layout
  htmltools::div(
    class = "region-summary-container",
    htmltools::div(
      class = "region-summary-title",
      region_name
    ),
    htmltools::div(
      class = "region-summary-plot",
      htmltools::plotTag(plot_list[[region_name]]+ ggtitle(" "), 
                         alt = paste("Summary plot for", region_name))
    )
  )
}


# --- Utility: Render a plot + text row for each country dynamically ---
render_country_plot_text_grid <- function(
    plot_list,
    summary_df,
    title = NULL,
    scrollable = TRUE,
    container_class = "country-plot-text-container",
    row_class = "country-row",
    plot_class = "country-plot",
    text_class = "country-text"
) {
  if (is.null(plot_list) || length(plot_list) == 0) {
    return(htmltools::div(
      class = "alert alert-warning",
      "No plots available for this selection."
    ))
  }
  
  section_title <- htmltools::tags$h4(
    "Countries",
    class = "region-title text-center fw-bold mt-3 mb-3"
  )
  
  
  rows <- lapply(names(plot_list), function(country) {
    info <- summary_df %>%
      dplyr::filter(country == !!country) %>%
      dplyr::slice_head(n = 1)
    
    if (nrow(info) > 0) {
      # Extract single values as plain variables
      season_status <- info$SeasonStatus[1]
      recent_ratio  <- info$RecentRatio[1]
      last_month    <- info$CasesLastMonth[1]
      
      desc <- glue::glue(
        "For {country} to date, the dengue season has been {season_status} average. 
      The most recent month has been {recent_ratio}× the average season.  
      There were {last_month} cases last month."
      )
    } else {
      desc <- glue::glue("No current summary available for {country}.")
    }
    
    htmltools::div(
      class = row_class,
      htmltools::div(
        class = plot_class,
        htmltools::plotTag(plot_list[[country]], alt = paste("Plot for", country))
      ),
      htmltools::div(
        class = text_class,
        htmltools::tags$h6(country, class = "fw-semibold"),
        htmltools::HTML(desc)
      )
    )
  })
  
  container <- htmltools::div(class = container_class, rows)
  
  if (scrollable) {
    container <- htmltools::div(class = "snapshot-region-scroll", container)
  }
  
  htmltools::tagList(section_title, container)
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


