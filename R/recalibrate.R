#' Recalibrate the intercept only (Recalibration-in-the-large)
#' This method adjusts the intercept of a model to better fit the observed data.
#' @param model The original model object.
#' @param new_data The dataset to use for recalibration.
#' @param outcome_col The name of the outcome column in the dataset.
#' @return The recalibrated model.
#'
#' @export

recalibrate.intercept <- function(model, new_data, outcome_col) {
  # Check if the outcome variable exists in the dataset
  if (!outcome_col %in% colnames(new_data)) {
    stop(paste("Outcome column", outcome_col, "not found in the data."))
  }

  y <- new_data[[outcome_col]]
  lp <- calculate_lp(model, new_data)

  # Select the appropriate exponential family for the GLM
  fam <- if (model$type == "logistic") binomial() else gaussian()

  # Fit a model to estimate the intercept adjustment.
  # Using offset(lp) fixes the slope of the linear predictor to exactly 1.
  fit <- glm(y ~ 1 + offset(lp), family = fam)

  # Extract the adjustment term for the intercept
  adj_intercept <- unname(coef(fit)["(Intercept)"])

  # Initialise new coefficients and update the intercept
  new_coefs <- model$coefs
  new_coefs["(Intercept)"] <- new_coefs["(Intercept)"] + adj_intercept

  # Build and return the updated model using the user's build_model function
  return(build_model(new_coefs, model$type))
}

#' Recalibrate both intercept and slope
#' This method adjusts both the intercept and slope of a model to better fit the observed data.
#' @param model The original model object.
#' @param new_data The dataset to use for recalibration.
#' @param outcome_col The name of the outcome column in the dataset.
#' @return The recalibrated model.
#'
#' @export
recalibrate.full <- function(model, new_data, outcome_col) {
  # Check if the outcome variable exists in the dataset
  if (!outcome_col %in% colnames(new_data)) {
    stop(paste("Outcome column", outcome_col, "not found in the data."))
  }

  y <- new_data[[outcome_col]]
  lp <- calculate_lp(model, new_data)

  # Select the appropriate exponential family for the GLM
  fam <- if (model$type == "logistic") binomial() else gaussian()

  # Fit a model using the original linear predictor as the sole independent variable
  fit <- glm(y ~ lp, family = fam)

  # Extract the recalibration intercept (gamma_0) and slope (gamma_1)
  gamma_0 <- unname(coef(fit)["(Intercept)"])
  gamma_1 <- unname(coef(fit)["lp"])

  # Update the coefficients mathematically:
  # New equation: gamma_0 + gamma_1 * (beta_0 + beta_1*X_1 + ...)
  # Therefore:
  # New slope_i = gamma_1 * beta_i
  # New intercept = gamma_0 + gamma_1 * beta_0

  new_coefs <- model$coefs * gamma_1
  new_coefs["(Intercept)"] <- new_coefs["(Intercept)"] + gamma_0

  # Build and return the updated model
  return(build_model(new_coefs, model$type))
}
