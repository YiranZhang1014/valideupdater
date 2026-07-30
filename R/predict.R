#' Calculate the linear predictor for linear-based models
#'
#' An internal helper function that ensures input data is handled correctly,
#' checks for missing predictor columns, extracts the design matrix, and
#' calculates the linear combination ($X\beta + \beta_0$).
#'
#' @param object An object of class 'logistic' created by `build_model`.
#' @param new_data A `data.frame` or `data.table` containing new predictor values.
#' @return A numeric vector representing the calculated linear predictor values.
#' @keywords internal
calculate_lp <- function(object, new_data) {
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

  # Calculate linear combination
  lp <- as.vector(X %*% predictor_coefs) + intercept_val
  return(lp)
}

#' Predict method for linear regression models
#'
#' Computes predicted response values for new data using a fitted linear regression model.
#'
#' @param object An object of class 'logistic' created by `build_model`.
#' @param new_data A `data.frame` or `data.table` containing the new predictor variables.
#' @param type A character string specifying the type of prediction required.
#'   Currently supports `"response"`.
#' @param ... Additional arguments passed to or from other methods.
#' @return A list containing the predicted response values (`$fit`).
#' @export
predict.linear <- function(
  object,
  new_data,
  type = c("response"),
  ...
) {
  type <- match.arg(type)

  # Utilize the core linear predictor function
  fit <- calculate_lp(object, new_data)

  res_lst <- list()
  if (type == "response") {
    res_lst$fit <- fit
  }

  return(res_lst)
}


#' @param object An object of class 'logistic' created by `build_model`.
#' @param new_data A `data.frame` or `data.table` containing the new predictor variables.
#' @param type A character string specifying the output type. Options are `"all"` (default),
#'   `"prob"` (predicted probabilities), or `"lp"` (linear predictors).
#' @param ... Additional arguments passed to or from other methods.
#' @return A list containing the requested prediction components (`$lp` and/or `$prob`).
#' @export
predict.logistic <- function(
  object,
  new_data,
  type = c("all", "prob", "lp"),
  ...
) {
  type <- match.arg(type)

  # Utilize the core linear predictor function
  linear_predictor <- calculate_lp(object, new_data)

  res_lst <- list()

  # Linear predictor
  if (type %in% c("lp", "all")) {
    res_lst$lp <- linear_predictor
  }

  # Probability
  if (type %in% c("prob", "all")) {
    res_lst$prob <- 1 / (1 + exp(-linear_predictor))
  }

  return(res_lst)
}
