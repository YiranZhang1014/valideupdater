#' Build a model based on the given equation information
#' @param coefs a named vector including each coeficicient and its beta. Must has `Intercept` term.
#' @param type the type of model to build, currently only support "linear", "logistic"
#' @param outcome the name of the outcome variable. This is optional and can be NULL if not needed.
#'
#' @export
build_model <- function(coefs, type = c("linear", "logistic"), outcome = NULL) {
  # -- check Inputs
  # Check the type argument and ensure it's one of the allowed values
  type <- match.arg(type)

  # Check input types
  if (!is.numeric(coefs) || is.null(names(coefs))) {
    stop("`coefs` must be a named numeric vector.")
  }

  if (!is.null(outcome) && !is.character(outcome)) {
    stop("`outcome` must be a character string or NULL.")
  }

  # Check Intercept term
  intercept_pattern <- "^\\(?[Ii]ntercept\\)?$"
  idx <- grep(intercept_pattern, names(coefs))

  # Check if there is exactly one Intercept term, if not, throw an error to avoid ambiguity
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
    type = type,
    outcome = outcome
  )

  # Assign class to the model
  class(model) <- c(type, class(model))

  # Return the model object
  return(model)
}
