#' Overview
#' ========
#' 
#' V2 PAHO and WHO backfilling.
#' 
#' Note: sourced by 02_V2_monthly_source_selection.R. Expects `paho` and `who`
#'  from 01_dengue_data.R (both carry reporting delay `d`) and uses the stable
#'  RF lookups in Assets/Stable via apply_delay_correction().
#'
#' 

# delay correction function (PAHO + WHO)
source("Scripts/backfilling/FUNCTIONS/00_FUN_apply_delay_correction.R")

# ----- apply correction - PAHO -------# 
# weekly delay (d) joined to paho_rf_lookup.csv; returns raw/corrected/applied + audit
paho_correction <- apply_delay_correction(df = paho, source = "PAHO")


# Record countries after PAHO correction (Step 4a: PAHO After Correction)
if (exists("record_countries_at_step")) {
  tryCatch({
    paho_correction_countries <- paho_correction %>%
      dplyr::select(country) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        iso3 = countrycode::countrycode(country, "country.name", "iso3c")
      ) %>%
      dplyr::select(country, iso3) %>%
      dplyr::filter(!is.na(iso3))
    
    record_countries_at_step(paho_correction_countries, "Step_4a_PAHO_After_Correction")
    log_message("Recorded " %+% nrow(paho_correction_countries) %+% " countries after PAHO correction")
  }, error = function(e) {
    if (exists("log_message")) {
      log_message("Warning: Country tracking failed at Step 4a PAHO Correction: " %+% conditionMessage(e), level = "WARNING")
    }
  })
}

# -------- Apply Corrections - WHO -------- #

# ----- apply correction - WHO -------# 
# monthly delay (d) joined to who_rf_lookup.csv; defaults use the `cases` column
who_correction <- apply_delay_correction(df = who, source = "WHO")


# Record countries after WHO correction (Step 4b: WHO After Correction)
if (exists("record_countries_at_step")) {
  tryCatch({
   who_correction_countries <- who_correction %>%
      dplyr::select(country) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        iso3 = countrycode::countrycode(country, "country.name", "iso3c")
      ) %>%
      dplyr::select(country, iso3) %>%
      dplyr::filter(!is.na(iso3))
    
    record_countries_at_step(who_correction_countries, "Step_4b_WHO_After_Correction")
    log_message("Recorded " %+% nrow(who_correction_countries) %+% " countries after WHO correction")
  }, error = function(e) {
    if (exists("log_message")) {
      log_message("Warning: Country tracking failed at Step 4b WHO Correction: " %+% conditionMessage(e), level = "WARNING")
    }
  })
}

# ----- save correction audit (raw vs corrected vs applied + reason) ----- #
# Used by 04_correction_impact_summary.R. Writes to the run dir in-pipeline,
# else a standalone audit dir.
audit_dir <- if (exists("run_dir")) paste0(run_dir,"/inital_rf_correction") else file.path("Output", "inital_rf_correction")
dir.create(audit_dir, showWarnings = FALSE, recursive = TRUE)

write.csv(paho_correction, file.path(audit_dir, "correction_paho_weekly.csv"), row.names = FALSE)
write.csv(who_correction, file.path(audit_dir, "correction_who_monthly.csv"), row.names = FALSE)
log_message("Saved PAHO/WHO correction audit CSVs to " %+% audit_dir)

