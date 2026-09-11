#' ---
#' title: "glm_factory"
#' author: "K M Susong"
#' ---
#'
#' Overview:
#' ========
#' `make_glm_model()` - builds a forecast_model whose fit / predict / diagnose
#' are generic GLM machinery, parametrised by a formula. Every
#' `models/NN_glm_*.R` file is then a one-liner: give it a name and a formula,
#' get back a contract-compliant model object. Nothing here runs on source.
#'
#' Supported so far (all case-data only - no climate covariates yet):
#'   - response is always `cases`
#'   - predictors: any `cases_lagN` (built here from the panel if absent),
#'     the calendar-month harmonics `sin1/cos1/sin2/cos2`, `Month`,
#'     `time_index`, and `iso3` as a fixed effect
#'   - family "nb" (MASS::glm.nb, Poisson fallback if it will not fit) or any
#'     base family name, e.g. "poisson"
#'   - `recursive = TRUE`: for horizons >= 2 the model's own previous-horizon
#'     point forecast is fed back in as `cases_lag1` (a genuine 1-step AR run);
#'     `cases_lag12` and longer always come from observed history (they are
#'     inside the training window for horizons 1..6)
#'
#' Intervals: negative-binomial (or Poisson) quantiles of the fitted mean at
#' the 0.05 / 0.25 / 0.5 / 0.75 / 0.95 probabilities -> `forecast_output_cols`
#' 50% / 90% bands. `.pred` is the fitted mean, clamped inside its own 50% band
#' so the contract's monotonicity check always passes.
#'
#' Timeline:
#' ========
#' 10-09-2026: Created. Tested Stage 0 - 01_glm_ar1.R.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
  library(lubridate)
})

if (!exists("interval_probs")) {
  source("Scripts/forecasting/00_config.R")
}
if (!exists("new_forecast_model")) {
  source("Scripts/forecasting/models/utils/forecast_helpers.R")
}

# Default quantile_probs for make_glm_model() - 00_config.R's interval_probs is
# the single source of truth (also used by models/00b_baseline_nowcast.R), so
# the two model families can't drift apart on quantile levels. A model file can
# still override quantile_probs explicitly if it ever needs to.
glm_default_probs <- interval_probs

#' Build a GLM forecast model.
#'
#' @param name Model slug (also the `model` column value).
#' @param formula A two-sided formula, response `cases`.
#' @param family "nb" (default) or a base family name like "poisson".
#' @param recursive Feed the model's own h-1 forecast in as `cases_lag1` for
#'   horizons >= 2.
#' @param quantile_probs Named 5-vector (q05,q25,q50,q75,q95).
#' @return An object of class `forecast_model`.
make_glm_model <- function(name, formula, family = "nb", recursive = FALSE,
                           quantile_probs = glm_default_probs) {
  spec <- list(
    name           = name,
    formula        = formula,
    family         = family,
    recursive      = isTRUE(recursive),
    quantile_probs = quantile_probs,
    vars           = all.vars(formula)
  )

  # ---- fit -----------------------------------------------------
  glm_fit <- function(train_df, spec = spec) {
    if (!all(c("iso3", "date", "cases") %in% names(train_df))) {
      cli::cli_abort("`train_df` needs {.field iso3}, {.field date}, {.field cases}.")
    }
    vars      <- spec$vars
    resp      <- vars[1]
    lag_vars  <- grep("^cases_lag[0-9]+$", vars, value = TRUE)
    lag_ns    <- sort(as.integer(sub("^cases_lag", "", lag_vars)))
    uses_iso3 <- "iso3" %in% vars

    d <- dplyr::arrange(train_df, iso3, date)
    for (n in lag_ns) {
      col <- paste0("cases_lag", n)
      if (!col %in% names(d)) {
        d <- d %>%
          dplyr::group_by(iso3) %>%
          dplyr::mutate(!!col := dplyr::lag(cases, n)) %>%
          dplyr::ungroup()
      }
    }
    if (uses_iso3) d <- dplyr::mutate(d, iso3 = factor(iso3))

    model_vars <- intersect(vars, names(d))
    fit_df <- d %>%
      dplyr::filter(!is.na(cases)) %>%
      dplyr::filter(dplyr::if_all(dplyr::all_of(model_vars), ~ !is.na(.x)))

    if (nrow(fit_df) < 2 * length(vars) + 2) {
      cli::cli_abort("Model {.val {spec$name}}: only {nrow(fit_df)} usable training rows.")
    }

    fam_used  <- spec$family
    converged <- FALSE
    mod <- NULL
    if (spec$family == "nb") {
      mod <- tryCatch(
        suppressWarnings(MASS::glm.nb(spec$formula, data = fit_df)),
        error = function(e) NULL
      )
      converged <- !is.null(mod) && isTRUE(mod$converged)
      if (!converged) {
        warning(sprintf("glm_fit[%s]: glm.nb did not fit cleanly - falling back to Poisson",
                        spec$name))
        mod <- stats::glm(spec$formula, data = fit_df, family = stats::poisson())
        fam_used  <- "poisson"
        converged <- isTRUE(mod$converged)
      }
    } else {
      fam_fn <- get(spec$family, mode = "function")
      mod <- stats::glm(spec$formula, data = fit_df, family = fam_fn())
      converged <- isTRUE(mod$converged)
    }
    theta <- if (fam_used == "nb") as.numeric(mod$theta) else Inf

    structure(list(
      name        = spec$name,
      spec        = spec,
      model       = mod,
      family_used = fam_used,
      theta       = theta,
      converged   = converged,
      n_obs       = nrow(fit_df),
      lag_ns      = lag_ns,
      uses_iso3   = uses_iso3,
      iso3_levels = if (uses_iso3) levels(fit_df$iso3) else NULL,
      obs_history = d %>% dplyr::filter(!is.na(cases)) %>% dplyr::select(iso3, date, cases),
      trained_to  = max(d$date)
    ), class = "glm_model_fit")
  }

  # ---- predict -------------------------------------------------
  glm_predict <- function(fitted, targets, spec = fitted$spec) {
    need <- c("iso3", "origin_date", "horizon", "target_date")
    if (!all(need %in% names(targets))) {
      cli::cli_abort("`targets` needs columns {.field {need}}.")
    }
    probs   <- spec$quantile_probs
    lag_ns  <- fitted$lag_ns
    recurse <- isTRUE(spec$recursive)

    obs <- fitted$obs_history
    obs_vec <- stats::setNames(obs$cases, paste(obs$iso3, as.character(obs$date)))
    obs_at <- function(i, d) {
      v <- unname(obs_vec[paste(i, as.character(d))])  # NA if the key is absent
      if (length(v) != 1L || is.na(v)) NA_real_ else v
    }

    tg <- targets %>%
      dplyr::mutate(target_date = as.Date(target_date),
                    origin_date = as.Date(origin_date)) %>%
      dplyr::arrange(iso3, origin_date, horizon)

    predict_group <- function(g) {
      i <- g$iso3[[1]]
      known_iso3 <- !fitted$uses_iso3 || i %in% fitted$iso3_levels
      by_h <- list()  # horizon (chr) -> mu
      for (r in seq_len(nrow(g))) {
        h  <- g$horizon[[r]]
        td <- g$target_date[[r]]
        M  <- lubridate::month(td)
        # Calendar-month features for this target - only need `Month` itself,
        # so build these directly rather than looking the target month up in
        # the training panel. The two harmonic pairs (annual + semi-annual
        # cycle) mirror the sin1/cos1/sin2/cos2 columns 01_prepare_training_data.R
        # already puts on every panel row; a formula only picks these up if it
        # names them (glm_ar1's does not - a seasonal GLM's formula would).
        newd <- list(
          Month = M,
          sin1 = sin(2 * pi * M / 12), cos1 = cos(2 * pi * M / 12),
          sin2 = sin(4 * pi * M / 12), cos2 = cos(4 * pi * M / 12)
        )
        if (fitted$uses_iso3) {
          newd$iso3 <- factor(i, levels = fitted$iso3_levels)
        }
        ok <- known_iso3
        for (n in lag_ns) {
          val <- obs_at(i, td %m-% months(n))
          if (is.na(val) && recurse && n == 1L) {
            val <- by_h[[as.character(h - 1L)]] %||% NA_real_
          }
          if (is.na(val)) ok <- FALSE
          newd[[paste0("cases_lag", n)]] <- val
        }
        mu <- NA_real_
        if (isTRUE(ok)) {
          mu <- tryCatch(
            as.numeric(stats::predict(fitted$model,
              newdata = as.data.frame(newd), type = "response")),
            error = function(e) NA_real_
          )
        }
        by_h[[as.character(h)]] <- mu
        g$.mu[r] <- mu
      }
      g
    }

    out <- tg %>%
      dplyr::mutate(.mu = NA_real_) %>%
      dplyr::group_split(iso3, origin_date) %>%
      purrr::map_dfr(predict_group)

    qfun <- function(p) {
      if (fitted$family_used == "nb") {
        stats::qnbinom(p, mu = out$.mu, size = fitted$theta)
      } else {
        stats::qpois(p, lambda = out$.mu)
      }
    }
    out %>%
      dplyr::mutate(
        .pred         = .mu,
        .pred_lower50 = pmin(.mu, qfun(probs[["q25"]])),
        .pred_upper50 = pmax(.mu, qfun(probs[["q75"]])),
        .pred_lower90 = pmin(.mu, qfun(probs[["q05"]])),
        .pred_upper90 = pmax(.mu, qfun(probs[["q95"]])),
        dplyr::across(dplyr::starts_with(".pred"), ~ pmax(0, round(.x)))
      ) %>%
      dplyr::select(dplyr::all_of(forecast_output_cols))
  }

  # ---- diagnose -----------------------------------------------
  glm_diagnose <- function(fitted, train_df = NULL, spec = fitted$spec) {
    co   <- stats::coef(fitted$model)
    pr   <- tryCatch(stats::residuals(fitted$model, type = "pearson"),
                     error = function(e) NA_real_)
    disp <- if (length(pr) > 1 && stats::df.residual(fitted$model) > 0) {
      sum(pr^2, na.rm = TRUE) / stats::df.residual(fitted$model)
    } else {
      NA_real_
    }
    ar_terms <- grep("cases_lag", names(co), value = TRUE)
    theta_ok <- fitted$family_used != "nb" ||
      (is.finite(fitted$theta) && fitted$theta > 1e-3 && fitted$theta < 1e6)

    list(
      model            = fitted$name,
      family           = fitted$family_used,
      n_obs            = fitted$n_obs,
      converged        = isTRUE(fitted$converged),
      theta            = if (is.finite(fitted$theta)) round(fitted$theta, 3) else Inf,
      dispersion_ratio = if (is.na(disp)) NA_real_ else round(disp, 3),
      n_coef           = length(co),
      any_na_coef      = anyNA(co),
      ar_coef          = if (length(ar_terms) > 0) {
        paste(sprintf("%s=%.3f", ar_terms, co[ar_terms]), collapse = ", ")
      } else {
        NA_character_
      },
      pass = isTRUE(fitted$converged) && !anyNA(co) && isTRUE(theta_ok),
      notes = sprintf(
        "%s GLM (%s); %d obs; theta=%s; dispersion=%s; %s",
        fitted$name, fitted$family_used, fitted$n_obs,
        if (is.finite(fitted$theta)) sprintf("%.2f", fitted$theta) else "Inf",
        if (is.na(disp)) "NA" else sprintf("%.2f", disp),
        if (length(ar_terms) > 0) paste(sprintf("%s=%.3f", ar_terms, co[ar_terms]), collapse = ", ") else "no AR term"
      )
    )
  }

  new_forecast_model(
    name     = name,
    spec     = spec,
    fit      = glm_fit,
    predict  = glm_predict,
    diagnose = glm_diagnose
  )
}
