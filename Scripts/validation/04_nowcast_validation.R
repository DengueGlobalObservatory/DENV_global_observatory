library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(purrr)
library(sf)

source("Scripts/validation/FUNCTIONS/00_FUN_validation_metrics.R")

if (!exists("log_message")) {
  source("Scripts/utils/logging.R")
  ensure_logger(console = TRUE)
}

out_dir <- file.path("Output", "validation")
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

log_message("Validation run: preparing data.")
prep <- prepare_validation_dataset()
validation_data <- prep$validation_data


log_message("Validation run: moving-window nowcast simulation.")
validation_results <- run_moving_window_validation(validation_data)
if (nrow(validation_results) == 0) {
  stop("Validation produced zero rows; cannot compute metrics.")
}
write_csv(validation_results, file.path(out_dir, "validation_results_detail.csv"))

log_message("Validation run: computing error metrics.")
metrics <- compute_error_metrics(validation_results)
country_metrics <- classify_performance(metrics$by_country, validation_data)
cutoff_analysis <- season_month_accuracy_analysis(metrics$by_cutoff, threshold = 0.25)

write_csv(country_metrics, file.path(out_dir, "table1_country_validation_summary.csv"))
write_csv(metrics$by_cutoff, file.path(out_dir, "table2_cutoff_accuracy_summary.csv"))
write_csv(metrics$by_country_cutoff, file.path(out_dir, "table3_country_cutoff_detail.csv"))

log_message("Validation run: calibrated prediction intervals.")
lookup <- build_prediction_interval_lookup(validation_results, min_obs = 5)
if (!dir.exists("Assets/Stable")) {
  dir.create("Assets/Stable", recursive = TRUE, showWarnings = FALSE)
}
write_csv(lookup, "Assets/Stable/calibrated_prediction_intervals.csv")
write_csv(lookup, file.path(out_dir, "table5_calibrated_prediction_intervals.csv"))

calibration <- coverage_calibration(validation_results, lookup)
write_csv(calibration, file.path(out_dir, "calibration_summary.csv"))

log_message("Validation run: snapshot convergence and backfilling assessment.")
snap <- compute_snapshot_convergence(output_root = "Output")
write_csv(snap$detail, file.path(out_dir, "snapshot_convergence_detail.csv"))
write_csv(snap$summary, file.path(out_dir, "table4_snapshot_convergence_summary.csv"))

backfill_assessment <- compute_backfill_assessment(output_root = "Output")
write_csv(backfill_assessment, file.path(out_dir, "backfill_assessment_paho.csv"))

log_message("Validation run: building figures.")

# Figure 1: Heatmap country x cutoff RMSPE
log_message("Figure 1: heatmap (country x cutoff RMSPE).")
p_heat <- metrics$by_country_cutoff %>%
  ggplot(aes(x = cutoff_month, y = reorder(country, RMSPE, FUN = median), fill = RMSPE)) +
  geom_tile() +
  scale_fill_gradient(low = "#2ca25f", high = "#de2d26", na.value = "grey90") +
  labs(x = "Cutoff month (season_nMonth)", y = "Country", fill = "RMSPE") +
  facet_grid(Region ~ ., scales = "free_y", space = "free_y") +
  theme_minimal(base_size = 11)
ggsave(file.path(out_dir, "fig1_heatmap_country_cutoff_rmspe.png"), p_heat, width = 10, height = 14, dpi = 220)
log_message("Figure 1 saved.")

# Figure 2: Error curve over cutoff months
log_message("Figure 2: error curve by cutoff month.")
curve_df <- validation_results %>%
  dplyr::group_by(Region, cutoff_month) %>%
  dplyr::summarise(
    med = median(ape, na.rm = TRUE),
    q25 = quantile(ape, 0.25, na.rm = TRUE),
    q75 = quantile(ape, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
p_curve <- curve_df %>%
  ggplot(aes(x = cutoff_month, y = med, colour = Region, fill = Region)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  labs(x = "Cutoff month (season_nMonth)", y = "Median APE", colour = "Region", fill = "Region") +
  theme_minimal(base_size = 11)
ggsave(file.path(out_dir, "fig2_error_curve_cutoff_month.png"), p_curve, width = 10, height = 5.5, dpi = 220)
log_message("Figure 2 saved.")

# Figure 3: Selected countries with uncertainty ribbons (cutoffs 3, 6, 9)
log_message("Figure 3: nowcast fans (selected countries).")
selected <- country_metrics %>%
  dplyr::group_by(performance_tier) %>%
  dplyr::arrange(RMSPE) %>%
  dplyr::slice(c(1, 2)) %>%
  dplyr::ungroup() %>%
  dplyr::select(iso3, country, performance_tier)

fan_df <- validation_results %>%
  dplyr::filter(cutoff_month %in% c(3, 6, 9), iso3 %in% selected$iso3) %>%
  dplyr::left_join(selected, by = c("iso3", "country")) %>%
  apply_prediction_intervals(lookup = lookup) %>%
  dplyr::mutate(cutoff_label = paste0("Cutoff ", cutoff_month))

p_fan <- fan_df %>%
  ggplot(aes(x = season_nMonth, y = predicted_cases, group = interaction(season, cutoff_label))) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95, fill = cutoff_label), alpha = 0.12) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50, fill = cutoff_label), alpha = 0.25) +
  geom_line(aes(colour = cutoff_label), linewidth = 0.7) +
  geom_point(aes(y = actual_cases), shape = 1, size = 1.3, alpha = 0.65) +
  facet_wrap(~country, scales = "free_y", ncol = 3) +
  labs(x = "Season month", y = "Cases", colour = "Nowcast run", fill = "Nowcast run") +
  theme_minimal(base_size = 10)
ggsave(file.path(out_dir, "fig3_selected_country_nowcast_fans.png"), p_fan, width = 12, height = 8.5, dpi = 220)
log_message("Figure 3 saved.")

# Figure 4: Country performance map
log_message("Figure 4: country performance map.")
world_sf <- build_world_sf()
map_df <- country_metrics %>%
  dplyr::transmute(iso_a3 = iso3, performance_tier, RMSPE)
p_map <- world_sf %>%
  dplyr::left_join(map_df, by = "iso_a3") %>%
  ggplot() +
  geom_sf(aes(fill = performance_tier), colour = "grey70", linewidth = 0.1) +
  scale_fill_manual(values = c(Good = "#2ca25f", Moderate = "#fdae6b", Poor = "#de2d26"), na.value = "grey90") +
  theme_void() +
  theme(legend.position = "bottom")
ggsave(file.path(out_dir, "fig4_country_performance_map.png"), p_map, width = 12, height = 6.5, dpi = 220)
log_message("Figure 4 saved.")

# Figure 5: RMSPE by region
log_message("Figure 5: boxplot RMSPE by region.")
p_box <- country_metrics %>%
  ggplot(aes(x = Region, y = RMSPE, fill = Region)) +
  geom_boxplot(alpha = 0.85, outlier.size = 0.7) +
  coord_flip() +
  guides(fill = "none") +
  theme_minimal(base_size = 11) +
  labs(x = NULL, y = "Country-level RMSPE")
ggsave(file.path(out_dir, "fig5_rmspe_by_region_boxplot.png"), p_box, width = 8, height = 5.5, dpi = 220)
log_message("Figure 5 saved.")

# Figure 6: Snapshot convergence example
log_message("Figure 6: snapshot convergence examples.")
snap_examples <- snap$detail %>%
  dplyr::group_by(iso3, country, Year, Month) %>%
  dplyr::summarise(start_diff = dplyr::first(abs_diff_to_final), .groups = "drop") %>%
  dplyr::arrange(desc(start_diff)) %>%
  dplyr::slice_head(n = 6)

if (nrow(snap_examples) > 0) {
  p_conv <- snap$detail %>%
    dplyr::inner_join(snap_examples, by = c("iso3", "country", "Year", "Month")) %>%
    ggplot(aes(x = snapshot_date, y = cases_nowcast, group = interaction(iso3, Year, Month), colour = country)) +
    geom_line(alpha = 0.8) +
    geom_point(size = 1) +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Snapshot date", y = "Nowcast estimate", colour = "Country")
  ggsave(file.path(out_dir, "fig6_snapshot_convergence.png"), p_conv, width = 11, height = 5.5, dpi = 220)
  log_message("Figure 6 saved.")
}

# Figure 7: Calibration diagnostic
log_message("Figure 7: calibration diagnostic.")
p_cal <- calibration %>%
  ggplot(aes(x = nominal, y = empirical)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
  geom_point(size = 3, colour = "#2b8cbe") +
  geom_text(aes(label = paste0(interval, "%")), nudge_y = 0.02) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal(base_size = 11) +
  labs(x = "Nominal coverage", y = "Empirical coverage")
ggsave(file.path(out_dir, "fig7_calibration_diagnostic.png"), p_cal, width = 6, height = 6, dpi = 220)
log_message("Figure 7 saved.")

reliable_horizon <- cutoff_analysis$reliable_horizon
writeLines(
  paste0("Reliable horizon (median RMSPE < 0.25): ", ifelse(is.na(reliable_horizon), "Not reached", reliable_horizon)),
  con = file.path(out_dir, "reliable_horizon.txt")
)

log_message("Validation run complete.")
