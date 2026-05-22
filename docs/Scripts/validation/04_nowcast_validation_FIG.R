# =============================================================================
# 04_nowcast_validation_FIG.R
# Publication-style diagnostic figures for retrospective validation
# =============================================================================
# Prerequisites: run 03_nowcast_validation_ind.R, 03_nowcast_validation_summary.R,
# and (for snapshot figure) 03_nowcast_validation_snapshots.R.
# Writes six PNGs to Output/validation/ at 220 dpi.
# =============================================================================

library(tidyverse)
library(sf)
library(rnaturalearth)
library(cowplot)

out_dir <- "Output/validation"
dpi <- 300

# --- Shared inputs -----------------------------------------------------------
detail_path <- file.path(out_dir, "validation_detail.csv")
pair_path <- file.path(out_dir, "summary_pair.csv")
country_path <- file.path(out_dir, "summary_country.csv")
coverage_path <- file.path(out_dir, "coverage_summary.csv")
calib_path <- file.path(out_dir, "calibrated_prediction_intervals.csv")
snap_detail_path <- file.path(out_dir, "snapshot_convergence_detail.csv")

stopifnot(file.exists(detail_path), file.exists(pair_path), file.exists(country_path))
stopifnot(file.exists(coverage_path))

detail <- read_csv(detail_path, show_col_types = FALSE)
summary_pair <- read_csv(pair_path, show_col_types = FALSE)
summary_country <- read_csv(country_path, show_col_types = FALSE)
coverage_summary <- read_csv(coverage_path, show_col_types = FALSE)
calib <- if (file.exists(calib_path)) read_csv(calib_path, show_col_types = FALSE) else tibble()

# --- Fig 1: Heatmap — median |relative error| by (cutoff, prediction month) --
# Cell-level medians summarise spread at each operational pair (not horizon).
heat_df <- detail %>%
  filter(is.finite(relative_error)) %>%
  group_by(cutoff_month, prediction_month) %>%
  dplyr::summarise(med_abs_rel = median(abs(relative_error), na.rm = TRUE), .groups = "drop")

p1 <- heat_df %>%
  ggplot(aes(x = as.factor(cutoff_month), y = as.factor(prediction_month), fill = med_abs_rel)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90", limits = c(0, 2)) +
  coord_fixed(ratio = 1) +
  labs(
    x = "Last observation month (season month)",
    y = "Prediction month (season month)",
    fill = "Median\n|rel. err.|"
  ) +
  theme_cowplot()

ggsave(file.path(out_dir, "fig_error_heatmap_cutoff_pred.png"), p1, width = 8, height = 6.5, dpi = dpi)

# For each included country (pick_tiers), generate a heatmap for relative error by cutoff month and prediction month,
# then panel these plots in a single row using cowplot::plot_grid.

# Explicitly select Brazil (good), Afghanistan (moderate), and China (poor) from summary_country
pick_tiers <- summary_country %>%
  dplyr::filter(
    (iso3 == "BRA" )|
      (iso3 == "AFG" ) |
      (iso3 == "CHN" )
  ) %>%
  dplyr::select(iso3, country, performance_tier, composite_score)

# Generate heatmap per picked country, store in a list
p1_each_country <- purrr::map2(
  pick_tiers$iso3, pick_tiers$country,
  function(cur_iso3, cur_country) {
    heat_df_country <- detail %>%
      dplyr::filter(is.finite(relative_error), iso3 == cur_iso3) %>%
      dplyr::group_by(cutoff_month, prediction_month) %>%
      dplyr::summarise(med_abs_rel = median(abs(relative_error), na.rm = TRUE), .groups = "drop")
    plot_title <- paste0(
      cur_country, " (", 
      unique(pick_tiers$performance_tier[pick_tiers$iso3 == cur_iso3]), 
      ", score = ", signif(pick_tiers$composite_score[pick_tiers$iso3 == cur_iso3], 3), 
      ")"
    )
    ggplot(heat_df_country, aes(x = as.factor(cutoff_month), y = as.factor(prediction_month) , fill = med_abs_rel)) +
      geom_tile() +
      scale_fill_viridis_c(option = "plasma", na.value = "grey90", limits = c(0, 2)) +
 
      coord_fixed(ratio = 1) +
      labs(
        title = plot_title,
        x = "Last observation month (season month)",
        y = "Prediction month (season month)",
        fill = "Median\n|rel. err.|"
      ) +
      theme_cowplot()
  }
)

# Combine plots into a single row panel
p1_country_panel <- cowplot::plot_grid(p1_each_country[[2]],p1_each_country[[1]], p1_each_country[[3]], nrow = 1)

# Combine the main heatmap (p1) above the panel of country heatmaps
p1_country_summary <- cowplot::plot_grid(
  p1, 
  p1_country_panel,
  ncol = 1,
  align = "v",
  rel_heights = c(1, 1) # adjust as desired to change height proportions
)

# Save the combined panel plot
ggsave(file.path(out_dir, "fig_error_heatmap_cutoff_pred_country_panel.png"), p1_country_panel, width = 8 * length(p1_each_country), height = 6.5, dpi = dpi)

# --- Fig 2: Nominal vs empirical coverage ------------------------------------
p2 <- coverage_summary %>%
  ggplot(aes(x = nominal, y = empirical)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey45") +
  geom_point(size = 3, colour = "#2b8cbe") +
  geom_text(aes(label = paste0(interval, "%")), nudge_y = 0.02, size = 3.5) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_cowplot() +
  labs(
    title = "Calibrated interval coverage",
    x = "Nominal coverage",
    y = "Empirical coverage"
  )

ggsave(file.path(out_dir, "fig_coverage_calibration.png"), p2, width = 5.5, height = 5.5, dpi = dpi)

# --- Fig 3: Country performance tier map -------------------------------------
# Natural Earth polygons joined on ISO3; unmapped countries stay grey.
world_sf <- rnaturalearth::ne_countries(scale = 50, type = "countries", returnclass = "sf") %>%
  dplyr::select(iso_a3, geometry)

map_df <- summary_country %>%
  transmute(iso_a3 = iso3, performance_tier, RMSE_scaled)

tier_scale <- scale_fill_manual(
  values = c(Good = "#2ca25f", Moderate = "#fdae6b", Poor = "#de2d26"),
  na.value = "grey90",
  name = "Tier"
)

p3 <- world_sf %>%
  left_join(map_df, by = "iso_a3") %>%
  ggplot() +
  geom_sf(aes(fill = performance_tier), colour = "grey70", linewidth = 0.08) +
  tier_scale +
  theme_cowplot() +
  theme(legend.position = "bottom") +
  labs(title = "Country performance tier (composite z-score tertiles)")

ggsave(file.path(out_dir, "fig_country_tier_map.png"), p3, width = 11, height = 6, dpi = dpi)

# --- Fig 3b: Continuous composite-score map ---------------------------------
# Same geometry/join as Fig 3, but fill on the raw composite z-score so the
# magnitude of poorer / better performance is visible (Fig 3 only shows tier).
# Diverging palette centred at 0 so "average" countries are neutral; lower
# scores are better (green), higher scores are worse (red).
score_df <- summary_country %>%
  transmute(iso_a3 = iso3, composite_score)

score_limit <- max(abs(score_df$composite_score), na.rm = TRUE)
if (!is.finite(score_limit) || score_limit == 0) score_limit <- 1

p3b <- world_sf %>%
  left_join(score_df, by = "iso_a3") %>%
  ggplot() +
  geom_sf(aes(fill = composite_score), colour = "grey70", linewidth = 0.08) +
  scale_fill_gradient2(
    low = "#2ca25f", mid = "#f7f7f7", high = "#de2d26",
    midpoint = 0, limits = c(-score_limit, score_limit),
    na.value = "grey90",
    name = "Composite\nz-score"
  ) +
  theme_cowplot() +
  theme(legend.position = "bottom") +
  labs(title = "Country composite performance score (lower = better)")

ggsave(file.path(out_dir, "fig_country_score_map.png"), p3b, width = 11, height = 6, dpi = dpi)

# --- Fig 4: RMSE_scaled distribution by Region ------------------------------
p4 <- summary_country %>%
  filter(is.finite(RMSE_scaled)) %>%
  ggplot(aes(x = Region, y = RMSE_scaled, fill = Region)) +
  geom_boxplot(alpha = 0.85, outlier.size = 0.6, coef = 1.5) +
  coord_flip() +
  guides(fill = "none") +
  theme_cowplot() +
  labs(
    title = "Burden-scaled RMSE (RMSE / mean monthly cases) by region",
    x = NULL,
    y = "RMSE_scaled"
  )

ggsave(file.path(out_dir, "fig_rmse_by_region_boxplot.png"), p4, width = 9, height = 6, dpi = dpi)

# --- Fig 5: Example nowcast “fans” (Good / Moderate / Poor if available) -----
# One country per tier (first row when sorted by composite_score within tier),
# cutoffs 3 / 6 / 9, ribbons from calibrated lookup on validation_detail rows.
pick_tiers <- summary_country %>%
  group_by(performance_tier) %>%
  dplyr::arrange(composite_score, .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup()

if (nrow(pick_tiers) == 0) {
  message("Skipping fan plot: no countries in summary_country.")
} else if (nrow(calib) == 0) {
  message("Skipping fan plot: calibrated_prediction_intervals.csv is empty (no cells met MIN_OBS).")
} else {
  fan_df <- detail %>%
    filter(iso3 %in% pick_tiers$iso3, cutoff_month %in% c(3, 6, 9)) %>%
    left_join(
      calib %>%
        dplyr::select(iso3, cutoff_month, prediction_month, q025, q25, q75, q975),
      by = c("iso3", "cutoff_month", "prediction_month")
    ) %>%
    mutate(
      lower_95 = pmax(0, predicted_cases * (1 + q025)),
      upper_95 = pmax(0, predicted_cases * (1 + q975)),
      lower_50 = pmax(0, predicted_cases * (1 + q25)),
      upper_50 = pmax(0, predicted_cases * (1 + q75)),
      cutoff_label = paste0("Cutoff ", cutoff_month)
    ) %>%
    left_join(pick_tiers %>% 
                dplyr::select(iso3, performance_tier), by = "iso3") %>%
    mutate(facet_label = paste0(country, " (", performance_tier, ")"))

  p5 <- fan_df %>%
    ggplot(aes(x = prediction_month, group = interaction(season, cutoff_label))) +
    geom_ribbon(aes(ymin = lower_95, ymax = upper_95, fill = cutoff_label), alpha = 0.12, na.rm = TRUE) +
    geom_ribbon(aes(ymin = lower_50, ymax = upper_50, fill = cutoff_label), alpha = 0.22, na.rm = TRUE) +
    geom_line(aes(y = predicted_cases, colour = cutoff_label), linewidth = 0.55, na.rm = TRUE) +
    geom_point(aes(y = actual_cases), shape = 1, size = 1.1, alpha = 0.65, na.rm = TRUE) +
    facet_wrap(vars(facet_label), scales = "free_y", ncol = 3) +
    theme_cowplot() +
    labs(
      title = "Retrospective nowcasts with calibrated 50% / 95% intervals",
      x = "Season month (prediction month)",
      y = "Cases",
      colour = "Cutoff",
      fill = "Cutoff"
    )

  ggsave(file.path(out_dir, "fig_nowcast_fans.png"), p5, width = 12, height = 8, dpi = dpi)
}

# --- Fig 6: Snapshot convergence (largest initial revision examples) -----------
if (!file.exists(snap_detail_path)) {
  message("Skipping snapshot figure: missing snapshot_convergence_detail.csv.")
} else {
  snap_detail <- read_csv(snap_detail_path, show_col_types = FALSE)
  if (nrow(snap_detail) == 0) {
    message("Skipping snapshot figure: no rows in snapshot detail.")
  } else {
    # Rank country–months by how far the first snapshot was from the final value
    snap_examples <- snap_detail %>%
      group_by(iso3, country, Year, Month) %>%
      dplyr::summarise(start_diff = dplyr::first(abs_diff_to_final), .groups = "drop") %>%
      dplyr::arrange(desc(start_diff)) %>%
      slice_head(n = 6)

    p6 <- snap_detail %>%
      inner_join(snap_examples, by = c("iso3", "country", "Year", "Month")) %>%
      ggplot(aes(x = snapshot_date, y = cases_nowcast, colour = country,
                 group = interaction(iso3, Year, Month))) +
      geom_line(linewidth = 0.6, alpha = 0.85) +
      geom_point(size = 1) +
      theme_cowplot() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = "Snapshot convergence (selected country–months with largest initial |error|)",
        x = "Pipeline snapshot (run date)",
        y = "Nowcast estimate (unobserved months)",
        colour = "Country"
      )

    ggsave(file.path(out_dir, "fig_snapshot_convergence.png"), p6, width = 11, height = 5.5, dpi = dpi)
  }
}

message("Figure generation complete: ", out_dir)
