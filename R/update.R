# update.R

# ---- User-facing unified function ----
#' Update a model with new data
#' @param model_obj The original model object.
#' @param new_data The dataset to use for updating the model.
#' @param outcome_col The name of the outcome column in the dataset.
#' @param type The update method to use. Options are:
#'   - `"recalibrate_intercept"`: Recalibrate the intercept only (Recalibration-in-the-large)
#'   - `"recalibrate_full"` or `"recalibrate"`: Recalibrate both intercept and slope
#'   - `"refit"`: Refit the model with new data
#' @return The updated model object containing new coefficients and update parameters.
#' @export
update <- function(
  model_obj,
  new_data,
  outcome_col,
  type = c(
    "recalibrate_intercept",
    "recalibrate_full",
    "recalibrate",
    "refit"
  )
) {
  # Validate and match the selected method
  type <- match.arg(type)

  # Dispatch to the appropriate internal function
  if (type == "recalibrate_intercept") {
    return(recalibrate_intercept(model_obj, new_data, outcome_col))
  } else if (type %in% c("recalibrate_full", "recalibrate")) {
    return(recalibrate_full(model_obj, new_data, outcome_col))
  } else if (type == "refit") {
    return(refit(model_obj, new_data, outcome_col))
  }
}

# ---- Internal implementations - Recalibration ----

#' Recalibrate the intercept only (Recalibration-in-the-large)
#' @export
recalibrate_intercept <- function(model, new_data, outcome_col) {
  if (!outcome_col %in% colnames(new_data)) {
    stop(paste("Outcome column", outcome_col, "not found in the data."))
  }

  y <- new_data[[outcome_col]]
  lp <- calculate_lp(model, new_data)

  fam <- if (model$type == "logistic") binomial() else gaussian()

  # Fit a model to estimate the intercept adjustment
  fit <- glm(y ~ 1 + offset(lp), family = fam)

  # Extract the adjustment intercept.
  # Since the slope is fixed via offset, we manually define its estimate as 1 and SE as NA.
  update_estimate <- c(
    "(Intercept)" = unname(stats::coef(fit)["(Intercept)"]),
    "linear_predictor" = 1
  )

  update_se <- c(
    "(Intercept)" = unname(sqrt(diag(stats::vcov(fit)))["(Intercept)"]),
    "linear_predictor" = NA_real_
  )

  # Initialise new coefficients and update the intercept
  new_coefs <- model$coefs
  new_coefs["(Intercept)"] <- new_coefs["(Intercept)"] +
    update_estimate["(Intercept)"]

  # Build model and attach the separate update parameter vectors
  updated_model <- build_model(new_coefs, model$type)
  updated_model$update_estimate <- update_estimate
  updated_model$update_se <- update_se

  return(updated_model)
}

#' Recalibrate both intercept and slope
#' @export
recalibrate_full <- function(model, new_data, outcome_col) {
  if (!outcome_col %in% colnames(new_data)) {
    stop(paste("Outcome column", outcome_col, "not found in the data."))
  }

  y <- new_data[[outcome_col]]
  lp <- calculate_lp(model, new_data)

  fam <- if (model$type == "logistic") binomial() else gaussian()

  # Fit a model using the linear predictor
  fit <- glm(y ~ lp, family = fam)

  # Extract the estimates and standard errors into named vectors
  update_estimate <- stats::coef(fit)
  update_se <- sqrt(diag(stats::vcov(fit)))

  # Standardise the name for the linear predictor to match user requirements
  names(update_estimate)[names(update_estimate) == "lp"] <- "linear_predictor"
  names(update_se)[names(update_se) == "lp"] <- "linear_predictor"

  # Initialise new coefficients, scale by slope, and add adjustment intercept
  new_coefs <- model$coefs * update_estimate["linear_predictor"]
  new_coefs["(Intercept)"] <- new_coefs["(Intercept)"] +
    update_estimate["(Intercept)"]

  # Build model and attach the separate update parameter vectors
  updated_model <- build_model(new_coefs, model$type)
  updated_model$update_estimate <- update_estimate
  updated_model$update_se <- update_se

  return(updated_model)
}

# ---- Internal implementations - Refit ----

#' Refit a model with new data
#' @export
refit <- function(model, new_data, outcome_col) {
  if (!outcome_col %in% names(new_data)) {
    stop(paste("Outcome column", outcome_col, "not found in the data."))
  }

  coef_names <- names(model$coefs)
  preds <- coef_names[coef_names != "(Intercept)"]

  missing_cols <- setdiff(preds, names(new_data))
  if (length(missing_cols) > 0) {
    stop(
      "The following predictor variables are missing in `new_data`: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  formula_str <- paste(outcome_col, "~", paste(preds, collapse = " + "))
  model_formula <- as.formula(formula_str)

  fam <- if (model$type == "logistic") stats::binomial() else stats::gaussian()

  # Fit the entirely new model
  fit <- stats::glm(model_formula, data = new_data, family = fam)

  # Extract the refitted coefficients and their standard errors as named vectors
  update_estimate <- stats::coef(fit)
  update_se <- sqrt(diag(stats::vcov(fit)))

  # Standardise the Intercept name in the vectors just in case
  names(update_estimate)[
    names(update_estimate) == "(Intercept)"
  ] <- "(Intercept)"
  names(update_se)[names(update_se) == "(Intercept)"] <- "(Intercept)"

  # Replace the old coefficients with the fully refitted estimates
  new_coefs <- update_estimate

  # Build model and attach the separate update parameter vectors
  updated_model <- build_model(new_coefs, type = model$type)
  updated_model$update_estimate <- update_estimate
  updated_model$update_se <- update_se

  return(updated_model)
}
