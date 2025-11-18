library(tidyverse)
library(geomtextpath)
library(RColorBrewer)
library(patchwork)


# Main radial plotting function
make_country_plot <- function(df_country) {
  
  df <- df_country %>%
    mutate(
      # month = Calendar_year_month,
      month = Month,
      low_speed_raw = Ave_season_monthly_cases,
      high_speed_raw = cases,
      ratio = high_speed_raw / low_speed_raw,
      ratio_capped = pmin(pmax(ratio, 0.5), 2)
    )
  
  # define reference scale as the bigger of the two maxima
  max_ref <- max(c(df$low_speed_raw, df$high_speed_raw), na.rm = TRUE)
  max_low  <- max(df$low_speed_raw, na.rm = TRUE)
  
  # scale low_speed to fit half the reference radius
  df <- df %>%
    mutate(
      low_speed = (low_speed_raw / max_low) * (0.5 * max_ref),
      high_speed = (high_speed_raw / max_ref) * max_ref  # unchanged but explicit
    )
  
  # cumulative ring based on *raw* values
  last_month <- max(df$month[!is.na(df$high_speed_raw)])
  cum_low <- sum(df$low_speed_raw[1:last_month], na.rm=TRUE)
  cum_high <- sum(df$high_speed_raw[1:last_month], na.rm=TRUE)
  cum_ratio <- cum_high / cum_low
  cum_ratio_capped <- min(max(cum_ratio, 0.5), 2)
  
  ring_df <- tibble(
    xmin = 0.5,
    xmax = last_month + 0.5,
    ymin = max_ref * 1.05,
    ymax = max_ref * 1.15,
    fill_val = cum_ratio_capped
  )
  
  p <- ggplot(df, aes(x = factor(month))) +
    # baseline (rescaled to half radius)
    geom_col(aes(y = low_speed), fill = NA, color = "black", width = 0.8, size = 0.5) +
    # observed (raw values, scaled to reference)
    geom_col(aes(y = high_speed, fill = ratio_capped), width = 0.6, alpha = 0.7, na.rm = TRUE) +
    # cumulative ring
    geom_rect(data = ring_df,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill_val),
              inherit.aes = FALSE) +
    # month labels
    geomtextpath::geom_textsegment(
      data = data.frame(x = 1:12 - 0.4, xend = 1:12 + 0.4, label = toupper(month.abb)),
      inherit.aes = FALSE, color = "grey20", vjust = -0.4, size = 3,
      aes(x = x, xend = xend, label = label,
          y = max_ref * 1.18, yend = max_ref * 1.18)
    ) +
    coord_polar() +
    scale_fill_distiller(palette = "Spectral", type = "div",
                         limits = c(0.5, 2),
                         name = "Relative cases",
                         breaks = c(0.5, 2),
                         labels = c("<0.5x expected", ">2x expected")) +
    scale_y_continuous(limits = c(0, max_ref * 1.25)) +
    ggtitle(unique(df$Country),
            subtitle = paste0(" | cum ratio = ", cum_ratio, " | cum capped ratio = ", cum_ratio_capped)) +
    
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = "none") +
    theme(
      panel.background = element_rect(fill = "white", color = NA),  # white inside circle
      plot.background = element_blank(),  # transparent outside
    )
  return(p)
}


# Regional Summary radial plot:
make_region_plot <- function(df_region) {
  
  df <- df_region %>%
    mutate(
      month = Month,
      low_speed_raw = Ave_season_monthly_cases,
      high_speed_raw = cases,
      ratio = high_speed_raw / low_speed_raw,
      ratio_capped = pmin(pmax(ratio, 0.5), 2)
    )
  
  max_ref <- max(c(df$low_speed_raw, df$high_speed_raw), na.rm = TRUE)
  max_low  <- max(df$low_speed_raw, na.rm = TRUE)
  
  df <- df %>%
    mutate(
      low_speed = (low_speed_raw / max_low) * (0.5 * max_ref),
      high_speed = (high_speed_raw / max_ref) * max_ref
    )
  
  last_month <- max(df$month[!is.na(df$high_speed_raw)])
  cum_low <- sum(df$low_speed_raw[1:last_month], na.rm=TRUE)
  cum_high <- sum(df$high_speed_raw[1:last_month], na.rm=TRUE)
  cum_ratio <- cum_high / cum_low
  cum_ratio_capped <- min(max(cum_ratio, 0.5), 2)
  
  ring_df <- tibble(
    xmin = 0.5,
    xmax = last_month + 0.5,
    ymin = max_ref * 1.05,
    ymax = max_ref * 1.15,
    fill_val = cum_ratio_capped
  )
  
  p <- ggplot(df, aes(x = factor(month))) +
    geom_col(aes(y = low_speed), fill = NA, color = "black", width = 0.8, size = 0.5) +
    geom_col(aes(y = high_speed, fill = ratio_capped), width = 0.6, alpha = 0.7, na.rm = TRUE) +
    geom_rect(data = ring_df,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill_val),
              inherit.aes = FALSE) +
    geomtextpath::geom_textsegment(
      data = data.frame(x = 1:12 - 0.4, xend = 1:12 + 0.4, label = toupper(month.abb)),
      inherit.aes = FALSE, color = "grey20", vjust = -0.4, size = 3,
      aes(x = x, xend = xend, label = label,
          y = max_ref * 1.18, yend = max_ref * 1.18)
    ) +
    coord_polar() +
    scale_fill_distiller(palette = "Spectral", type = "div",
                         limits = c(0.5, 2),
                         name = "Relative cases",
                         breaks = c(0.5, 2),
                         labels = c("<0.5x expected", ">2x expected")) +
    scale_y_continuous(limits = c(0, max_ref * 1.25)) +
    ggtitle(unique(df$Region)) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = "none",
          plot.title = element_text(
            hjust = 0.5,          # center align horizontally
            vjust = -0.05,         # slightly lift above plot content
            face = "bold",        # bold text
            size = 16             # increase font size
          )) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),  # white inside circle
      plot.background = element_blank(),  # transparent outside
    )
  
  return(p)
}



