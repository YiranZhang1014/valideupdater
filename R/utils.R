#' Check and convert data to data.table
#' @param data A data.frame or data.table
#' @return A data.table
#'
#' @export
to_data_table <- function(data) {
  if (data.table::is.data.table(data)) {
    return(data)
  }
  return(data.table::as.data.table(data))
}

#' Convert factor columns to dummy variables
#' Now can only handle factor columns, character columns will be ignored
#' @param data A data.table
#' @return A data.table with dummy variables for factor columns
#'
#' @export
dummy_vars <- function(data) {
  # Copy the data to avoid modifying the original data.table by reference
  new_data <- copy(data)
  # Check and convert to data.table if necessary
  dt <- to_data_table(new_data)

  factor_cols <- names(dt)[sapply(dt, is.factor)]
  if (length(factor_cols) == 0) {
    return(dt)
  }

  for (j in factor_cols) {
    dummy_mat <- stats::model.matrix.lm(
      ~ -1 + dt[[j]],
      na.action = "na.pass"
    )
    colnames(dummy_mat) <- paste(
      j,
      sub("dt\\[\\[j\\]\\]", "", colnames(dummy_mat)),
      sep = "_"
    )

    for (col_name in colnames(dummy_mat)) {
      set(dt, j = col_name, value = dummy_mat[, col_name])
    }
  }
  # Remove original factor columns
  dt[, (factor_cols) := NULL]
  return(dt)
}


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
