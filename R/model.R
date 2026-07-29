#' Build model based on the coefficient information
#' @param coefs a named vector including each coeficicient and its beta. Must has `Intercept` term.
#' @param type the type of model to build, currently only support "logistic" and "survival"
#' 
#' @export
build_model <- function(coefs, type = c("logistic", "survival")) {
  # Check the type argument and ensure it's one of the allowed values
  type <- match.arg(type)

  # Check input types
  if (!is.numeric(coefs) || is.null(names(coefs))) {
    stop("`coefs` must be a named numeric vector.")
  }

  # Check Intercept term
  intercept_pattern <- "^\\(?[Ii]ntercept\\)?$"
  idx <- grep(intercept_pattern, names(coefs))

  # - Check if there is exactly one Intercept term, if not, throw an error to avoid ambiguity
  if (length(idx) == 0) {
    stop(
      "The coefs must include an Intercept term (e.g., '(Intercept)', 'intercept', etc.)."
    )
  } else if (length(idx) > 1) {
    stop(
      "Multiple terms match 'Intercept'. Please check your coefficient names."
    )
  }
  # Standardise the Intercept term name to "(Intercept)"
  names(coefs)[idx] <- "(Intercept)"

  # Build the model
  model <- list(
    coefs = coefs,
    type = type
  )

  # Assign class to the model
  class(model) <- c(type, class(model))
  return(model)
}
