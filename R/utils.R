#' Check and convert data to data.table
#' @param data A data.frame or data.table
#' @return A data.table
#' 
#' @import data.table
#' @export
to_data_table <- function(data) {
  if (!data.table::is.data.table(data)) {
    dt <- data.table::as.data.table(data)
  }
  return(dt)
}

#' Convert factor columns to dummy variables
#' Now can only handle factor columns, character columns will be ignored
#' @param data A data.table
#' @return A data.table with dummy variables for factor columns
#' 
#' @import data.table
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
