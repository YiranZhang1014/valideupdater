#' Predict Method for Logistic Models
#'
#' @param object An object of class 'logistic' created by `build_model`.
#' @param new_data A data.frame or data.table containing the predictors.
#' @param type Output type: default "all" returns both probabilities and linear predictors.
#'   "prob" returns predicted probabilities,
#'   "lp" returns linear predictors (log-odds).
#' @param ... Additional arguments (currently unused).
#'
#' @return A list of predicted probabilities and/or log-odds.
#' @export
predict.logistic <- function(
  object,
  new_data,
  type = c("all", "prob", "lp"),
  ...
) {
  type <- match.arg(type)

  # Ensure new_data is converted or handled as data.frame / data.table
  new_data <- to_data_table(new_data)

  coefs <- object$coefs
  coef_names <- names(coefs)

  # Separate Intercept and predictor coefficients
  intercept_val <- coefs[["(Intercept)"]]
  predictor_coefs <- coefs[coef_names != "(Intercept)"]
  predictor_names <- names(predictor_coefs)

  # Check if all required predictor columns exist in new_data
  missing_cols <- setdiff(predictor_names, names(new_data))
  if (length(missing_cols) > 0) {
    stop(
      "The following predictor variables are missing in `new_data`: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Extract design matrix X for predictors
  X <- as.matrix(new_data[, predictor_names, drop = FALSE, with = FALSE])

  # Calculate linear predictor: X %*% beta + intercept
  linear_predictor <- as.vector(X %*% predictor_coefs) + intercept_val

  # Create a list to save required predicted results.
  res_lst <- list()

  # Linear predictor
  if (type == "lp") {
    res_lst$lp <- linear_predictor
  }

  # Probability
  probabilities <- 1 / (1 + exp(-linear_predictor))

  if (type == "prob") {
    res_lst$prob <- probabilities
  }

  # Both LP and Prob
  if (type == "all") {
    res_lst$lp <- linear_predictor
    res_lst$prob <- probabilities
  }

  # Reture
  return(res_lst)
}
