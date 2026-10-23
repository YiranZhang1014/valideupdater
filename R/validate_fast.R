# update.R

# ---- User-facing unified function ----
#' Validate a model with new data (fast version)
#' @param obs A vector of observed outcomes.
#' @param probs A vector of predicted probabilities.
#' @param lps A vector of linear predictors.
#' @param metrics A character vector specifying which validation metrics to compute.
#'   Options include: "oe", "cal_int", "cal_slope", "auc", "r2", "brier".
#' @param level A numeric value specifying the confidence level for the confidence intervals of the metrics.
#' @param type The type of model being validated. Currently supports "binary" for logistic regression models.
#' @return A list containing the computed validation metrics and their confidence intervals.
#'
#' @export
validate_fast <- function(
  obs,
  probs,
  lps,
  metrics = c("oe", "cal_int", "cal_slope", "auc", "r2", "brier"),
  level = 0.95,
  type = c(
    "binary"
  )
) {
  # Validate and match the selected method
  type <- match.arg(type)
  if (type == "binary") {
    return(validate_logistic_fast(obs, probs, lps, metrics, level))
  }
}

# Functions for validating logistic regression models
validate_logistic_fast <- function(
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
    oe = function() get_oe_ratio_fast(obs, probs, z_val),
    cal_int = function() get_cal_int_fast(obs, lps, z_val),
    cal_slope = function() get_cal_slope_fast(obs, lps, z_val),
    auc = function() get_auc_fast(obs, probs, z_val),
    r2 = function() get_r2_fast(obs, lps),
    brier = function() get_brier_fast(obs, probs, z_val)
  )

  # Use 'lapply' to execute only the requested ones
  valid_res <- lapply(metrics, function(m) metric_map[[m]]())
  names(valid_res) <- metrics

  # Add metadata and return
  valid_res$level <- level
  return(valid_res)
}
