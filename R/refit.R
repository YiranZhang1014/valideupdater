#' Refit a model with new data
#' @param model The original model object.
#' @param new_data The dataset to use for refitting.
#' @param outcome_col The name of the outcome column in the dataset.
#' @return The refitted model.
#' @export
refit <- function(model, new_data, outcome_col) {
  # Check if the outcome variable exists in the new dataset
  if (!outcome_col %in% names(new_data)) {
    stop(paste("Outcome column", outcome_col, "not found in the data."))
  }

  # Extract predictor names by excluding the intercept
  coef_names <- names(model$coefs)
  preds <- coef_names[coef_names != "(Intercept)"]

  # Check for missing predictors in the new dataset
  missing_cols <- setdiff(preds, names(new_data))
  if (length(missing_cols) > 0) {
    stop(
      "The following predictor variables are missing in `new_data`: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Construct the formula (e.g., outcome ~ pred1 + pred2 + pred3)
  formula_str <- paste(outcome_col, "~", paste(preds, collapse = " + "))
  model_formula <- as.formula(formula_str)

  # Determine the appropriate exponential family based on the model type
  fam <- if (model$type == "logistic") {
    stats::binomial()
  } else {
    stats::gaussian()
  }

  # Fit the new model using the provided data
  fit <- stats::glm(model_formula, data = new_data, family = fam)

  # Extract the refitted coefficients
  new_coefs <- stats::coef(fit)

  # Standardise the Intercept name just in case glm outputs a different format
  names(new_coefs)[names(new_coefs) == "(Intercept)"] <- "(Intercept)"

  # Build and return the refitted model object using the original type
  return(build_model(new_coefs, type = model$type))
}
