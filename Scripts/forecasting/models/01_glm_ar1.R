#' ---
#' title: "01_glm_ar1"
#' author: "K M Susong"
#' ---
#'
#' Overview:
#' ========
#' The "linear temporal autoregressive" comparator from the scoring plan: a
#' pooled negative-binomial GLM of this month's cases on last month's cases,
#' log1p-transformed. Deliberately the simplest case-data-only model - no
#' country term, no seasonality term, no climate. Multi-step forecasts are
#' recursive (the h-1 point forecast feeds in as `cases_lag1` for h >= 2).
#'
#' DEFINITION ONLY - builds `glm_ar1_model` via make_glm_model(); no side
#' effects on source.
#'
#' Timeline:
#' ========
#' 10-09-2026: Created (Step 4).

if (!exists("make_glm_model")) {
  source("Scripts/forecasting/models/utils/glm_factory.R")
}

glm_ar1_model <- make_glm_model(
  name      = "glm_ar1",
  formula   = cases ~ log1p(cases_lag1),
  family    = "nb",
  recursive = TRUE
)
