library(tidyverse)
library(geomtextpath)
library(RColorBrewer)
library(patchwork)


# Regional Summary radial plot:
make_radial_plot <- function(df_region) {
  
  # Validate input
  if (is.null(df_region) || !is.data.frame(df_region) || nrow(df_region) == 0) {
    return(NULL)
  }
  
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
  
  # Calculate cumulative values only if we have valid data
  if (last_month > 0 && last_month <= nrow(df)) {
    cum_low <- sum(df$low_speed_raw[1:last_month], na.rm=TRUE)
    cum_high <- sum(df$high_speed_raw[1:last_month], na.rm=TRUE)
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
  
  # Create ring_df only if we have current year data
  if (has_current_year_data && last_month > 0) {
    ring_df <- tibble(
      xmin = 0.5,
      xmax = last_month + 0.5,
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
  if (has_current_year_data && last_month > 0 && nrow(ring_df) > 0) {
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
  
  # Create extras data frame (variables are always defined now)
  extras <- data_frame(max_ref, max_low, last_month, cum_low, cum_high, cum_ratio, cum_ratio_capped)
  if (nrow(ring_df) > 0) {
    extras <- cbind(ring_df, extras)
  }
  return(p)
  # return(list(p, df, extras))
  
}



