library(tidyverse)
library(geomtextpath)
library(RColorBrewer)
library(patchwork)

# df_start <- split_by_country$Brazil
# year <- 2026
# month <- 1

# Regional Summary radial plot:
make_radial_plot <- function(df_start, year = NULL, month = NULL ) {

  
  # Validate input
  if (is.null(df_start) || !is.data.frame(df_start) || nrow(df_start) == 0) {
    return(NULL)
  }
  
  
    if (is.null(month)) {
    month <- as.integer(format(Sys.Date(), "%m"))
  }

  # check for year variable

  if ("Year" %in% names(df_start)) {
    # If year is NULL, use current year
    if (is.null(year)) {
      year <- as.integer(format(Sys.Date(), "%Y"))
    }
  }

cat("Plotting:", unique(df_start$country))
    # Filter to needed months

    # Option 1: there are more that 6 months of data in the current year (ie month > 6)
    if (month > 6) {
      df_region <- df_start %>% filter(Year == year)
      cat("Option 1 - Rows after filter:", nrow(df_region), "\n")


    # Option 2: It is January, so all of the prior year is shown (i.e. month = 1)
    } else if (month == 1) {
      df_region <- df_start %>% filter(Year == year-1)
      cat("Option 2 - Rows after filter:", nrow(df_region), "\n")
      
      ### baseline is not needed in this version could that be why it is breaking?

    # Option 3: There are not more that 6 months of data in the year (i.e. month >1, month < 7)
    } else  {
      # Calculate the last 6 months (going back from month - 1)
      # This may span two years
      months_to_include <- numeric()
      years_to_include <- numeric()

      # Start from (month - 1) and go back 6 months
      for (i in 1:6) {
        target_month <- month - i
        if (target_month <= 0) {
          # Need to go to previous year
          target_year <- year - 1
          target_month <- 12 + target_month  # e.g., if target_month is -1, we want month 11
        } else {
          target_year <- year
        }
        months_to_include <- c(months_to_include, target_month)
        years_to_include <- c(years_to_include, target_year)
      }

      # Filter to include these specific year-month combinations
      df_region <- df_start %>%
        filter(
          (Year == years_to_include[1] & Month == months_to_include[1]) |
            (Year == years_to_include[2] & Month == months_to_include[2]) |
            (Year == years_to_include[3] & Month == months_to_include[3]) |
            (Year == years_to_include[4] & Month == months_to_include[4]) |
            (Year == years_to_include[5] & Month == months_to_include[5]) |
            (Year == years_to_include[6] & Month == months_to_include[6])
        )
 
      # ---- Restore baseline values
      
      # First, identify which columns we want to KEEP for baseline
      # These are the columns that contain baseline/seasonal information
      baseline_cols_to_keep <- c("iso3", "Country", "country", "Region", "Month",
                                 "season_nMonth", "nb_size", "nb_mean",
                                 "Ave_season_monthly_cases", "Ave_season_monthly_cum_cases",
                                 "Ave_cum_monthly_proportion", "Ave_monthly_proportion")
      
      # Only keep columns that actually exist in df_start
      baseline_cols_to_keep <- intersect(baseline_cols_to_keep, names(df_start))
      
      # Create baseline with only the columns we want
      baseline <- df_start %>%
        dplyr::select(dplyr::all_of(baseline_cols_to_keep))
      
      # Identify the key columns for grouping (Month + identifier columns)
      grouping_cols <- colnames(baseline)
      
      # Deduplicate baseline: keep only one row per Month (and per identifier)
      # If multiple rows exist, take the first non-NA value for Ave_season_monthly_cases
      baseline <- baseline %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) %>%
        dplyr::arrange(dplyr::desc(!is.na(Ave_season_monthly_cases))) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup()
      
      # Merge baseline back with filtered df_region
      # Match only on the grouping columns to avoid creating duplicates
      df_region <- dplyr::left_join(baseline,df_region, by = grouping_cols)
      
      cat("Option 3 - Rows after filter:", nrow(df_region), "\n")
      cat("Looking for:", paste(years_to_include, months_to_include, sep="-"), "\n")
    }
  

  # Check for duplicate months in final df
  df_dups <- df_region %>%
    dplyr::group_by(Month) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    filter(n > 1)
  if (nrow(df_dups) > 0) {
    cat("WARNING: Duplicate Month values in df_region!\n")
    print(df_dups)
  }
  
  
  cat("After merge - df_region rows:", nrow(df_region), "\n")

    if ("Ave_season_monthly_cases" %in% names(df_region)) {
    cat("Non-NA baseline values:", sum(!is.na(df_region$Ave_season_monthly_cases)), "\n")
    cat("All baseline NA?", all(is.na(df_region$Ave_season_monthly_cases)), "\n")
  }
  
  # ---- Build plot DF
  
  df <- df_region %>%
    mutate(
      month = Month,
      low_speed_raw = Ave_season_monthly_cases,
      high_speed_raw = cases,
      ratio = high_speed_raw / low_speed_raw,
      ratio_capped = pmin(pmax(ratio, 0.5), 2)
    )
  
  # Calculate max values with validation
  max_ref <- max(c(df$low_speed_raw, df$high_speed_raw), na.rm = TRUE)
  max_low  <- max(df$low_speed_raw, na.rm = TRUE)
  
  # Validate max_low - if invalid, set to 1 (minimum for plot structure)
  if (is.na(max_low) || is.infinite(max_low) || max_low <= 0) {
    # If no seasonal baseline data, this is invalid - return NULL
    if (all(is.na(df$low_speed_raw))) {
      return(NULL)
    }
    max_low <- 1
  }
  
  df <- df %>%
    mutate(
      over_cap = high_speed_raw > (1.8 * max_low),
      capped_height = pmin(high_speed_raw, 1.8 * max_low),
      small_both = (high_speed_raw < 0.15 * (2 * max_low))  ,
      dot_y = max_low * 1.7  #  visibility
    )
  
  # Check if all current year data (high_speed_raw) is NA
  has_current_year_data <- !all(is.na(df$high_speed_raw))
  
  # Calculate last_month with validation
  if (has_current_year_data) {
    valid_months <- df$month[!is.na(df$high_speed_raw)]
    if (length(valid_months) > 0) {
      last_month <- max(valid_months)
    } else {
      last_month <- 0
    }
  } else {
    last_month <- 0
  }
  
  # Calculate cumulative values up to current month (for ring fill)
  # Use current month parameter, not last_month from data
  if (month > 0 && month <= 12) {
    # Sum data for months 1 through current month
    current_month <- month  # Store parameter value to avoid confusion
    months_to_sum <- df %>% filter(.data$month >= 1 & .data$month <= current_month)
    cum_low <- sum(months_to_sum$low_speed_raw, na.rm=TRUE)
    cum_high <- sum(months_to_sum$high_speed_raw, na.rm=TRUE)
    if (cum_low > 0) {
      cum_ratio <- cum_high / cum_low
      cum_ratio_capped <- min(max(cum_ratio, 0.5), 2)
    } else {
      cum_ratio <- 1
      cum_ratio_capped <- 1
    }
  } else {
    cum_low <- 0
    cum_high <- 0
    cum_ratio <- 1
    cum_ratio_capped <- 1
  }
  
  # Create ring_df using current month (not last_month from data)
  # Ring extends from January (0.5) to current month (month + 0.5)
  if (has_current_year_data && month > 0 && month <= 12) {
    ring_df <- tibble(
      xmin = 0.5,
      xmax = month-1 + 0.5,
      ymin = max_low * 1.8,
      ymax = max_low * 2,
      fill_val = cum_ratio_capped
    )
  } else {
    # Create empty ring_df to avoid errors
    ring_df <- tibble(
      xmin = numeric(),
      xmax = numeric(),
      ymin = numeric(),
      ymax = numeric(),
      fill_val = numeric()
    )
  }
  
  # Build plot base
  p <- ggplot(df, aes(x = factor(month))) +
    geom_col(aes(y = low_speed_raw), fill = NA, color = "black", width = 0.8, size = 0.5)
  
  # Add colored bars if we have current year data
  if (has_current_year_data) {
    p <- p + geom_col(aes(y = capped_height, fill = ratio_capped), width = 0.6, alpha = 0.8, na.rm = TRUE)
  }
  
  # Add outer ring if we have current year data
  # Use current month instead of last_month
  if (has_current_year_data && month > 0 && month <= 12 && nrow(ring_df) > 0) {
    p <- p + geom_rect(data = ring_df,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill_val),
              inherit.aes = FALSE)
  }
  
  # Add month labels
  p <- p + geomtextpath::geom_textsegment(
      data = data.frame(x = 1:12 - 0.4, xend = 1:12 + 0.4, label = toupper(month.abb)),
      inherit.aes = FALSE, color = "grey20", vjust = -0.4, size = 3,
      aes(x = x, xend = xend, label = label,
          y = max_low * 2, yend = max_low * 2)
    )
  
  # Add points if we have current year data
  if (has_current_year_data) {
    p <- p + geom_point(
      data = df %>% filter(small_both),
      aes(
        x = factor(month),
        y = dot_y,
        fill = ratio_capped,
        color = ratio_capped
      ),
      shape = 21,
      size = 4,
      stroke = 0.3
    )
  }
  
  my_colours <- brewer.pal(3, "RdYlBu")
  # Add polar coordinates and scales
  p <- p + coord_polar() +
    # scale_fill_brewer("RdYlBu")
    # scale_fill_gradientn( colours = my_colours)
    scale_fill_gradient2(
      low = "green",
      mid = "yellow",
      high = "red",

      # low = "#006164",
      # mid = "#e6e1bc",
      # high = "#b3589a",
# 
#       low = "#05f7ff",
#       mid = "#fff9cf",
#       high = "#ff5432",
#       
#       low = "#6b9dc1",
#       mid = "#c9ccd2",
#       high = "#bf511f",
      
      
      midpoint = 1,
      limits = c(0.5, 2),
      name = "Relative cases",
      breaks = c(0.5, 1, 2),
      labels = c("<0.5x expected", "expected", ">2x expected"))
  
  # Add arrow segments if we have current year data
  if (has_current_year_data) {
    p <- p + geom_segment(
      data = df %>% filter(over_cap),
      aes(
        x = factor(month),
        xend = factor(month),
        y = 1.8 * (max_low - ( max_low * 0.1)),
        yend = 1.8 * max_low
      ),
      arrow = arrow(type = "closed", length = unit(0.15, "cm")),
      lineend = "round",
      linewidth = 0.6,
      inherit.aes = FALSE
    )
  }
  
  # Add text annotation if no current year data
  if (!has_current_year_data) {
    p <- p + annotate("text", 
      x = 6.5, 
      y = max_low * 1.5, 
      label = "No current year data available", 
      size = 4, 
      color = "gray50",
      hjust = 0.5)
  }
  
  # Add remaining plot elements
  p <- p +
    
    scale_y_continuous(limits = c(0, max_low * 2))+
    
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = "none",
          # legend.position = "bottom",
          plot.title = element_text(
            hjust = 0.5,          # center align horizontally
            vjust = -0.05,        # slightly lift above plot content
            face = "bold",        # bold text
            size = 16             # increase font size
          )) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),  # white inside circle
      plot.background = element_blank(),  # transparent outside
    )
  
  # # Create extras data frame (variables are always defined now)
  # extras <- data_frame(max_ref, max_low, last_month, cum_low, cum_high, cum_ratio, cum_ratio_capped)
  # if (nrow(ring_df) > 0) {
  #   extras <- cbind(ring_df, extras)
  # }
  return(p)
  # return(list(p, df, extras))
  
}


