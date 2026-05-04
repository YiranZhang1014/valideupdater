validate <- function() {
  UseMethod("validate")
}

validate.default <- function() {
  stop("No validate method for this class of object")
}

#' @export
# Functions for validating logistic regression models
validate.logistic <- function(
  obs,
  probs,
  lps,
  metrics = c("oe", "cal_int", "cal_slope", "auc", "r2", "brier"),
  level = 0.95,
  ...
) {
  # Validation Logic (CI, infinite lps, etc.) stays at the top
  metrics <- match.arg(metrics, several.ok = TRUE)
  # Check level is valid
  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {
    stop("Level must be a single numeric value between 0 and 1")
  }

  n_inf <- sum(is.infinite(lps))
  if (n_inf > 0) {
    id <- which(is.infinite(lps))
    obs <- obs[-id]
    lps <- lps[-id]
    probs <- probs[-id]
    warning(paste(
      n_inf,
      "observations deleted due to predicted risks being 0 and 1"
    ))
  }

  z_val <- stats::qnorm(1 - (1 - level) / 2)

  # Define a mapping of metric names to their functions
  # This replaces all those 'if' blocks
  metric_map <- list(
    oe = function() get_oe_ratio(obs, probs, z_val),
    cal_int = function() get_cal_int(obs, lps, z_val),
    cal_slope = function() get_cal_slope(obs, lps, z_val),
    auc = function() get_auc(obs, probs, z_val),
    r2 = function() get_r2(obs, lps),
    brier = function() get_brier(obs, probs, z_val)
  )

  # Use 'lapply' to execute only the requested ones
  valid_res <- lapply(metrics, function(m) metric_map[[m]]())
  names(valid_res) <- metrics

  # Add metadata and return
  valid_res$level <- level
  return(valid_res)
}
