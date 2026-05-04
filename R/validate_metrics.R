#' Calculate the Observed-to-Expected (O:E) ratio
#' @param obs A numeric vector of observed binary outcomes (0 or 1)
#' @param probs A numeric vector of predicted probabilities (between 0 and 1)
#' @param z_val The z-value corresponding to the desired confidence level (e.g., 1.96 for 95% confidence)
#' @return A list containing the OE ratio and its confidence intervals
#'
#' @noRd
get_oe_ratio <- function(obs, probs, z_val) {
  log_OE_ratio <- log(sum(obs)) - log(mean(probs) * length(probs))
  log_OE_ratio_SE <- sqrt(((1 - mean(obs)) / sum(obs)))
  OE_ratio <- exp(log_OE_ratio)
  OE_ratio_lower <- exp(log_OE_ratio - (z_val * log_OE_ratio_SE))
  OE_ratio_upper <- exp(log_OE_ratio + (z_val * log_OE_ratio_SE))

  OE_ratio_info <- list(
    "value" = OE_ratio,
    "se" = log_OE_ratio_SE,
    "lower" = OE_ratio_lower,
    "upper" = OE_ratio_upper
  )

  OE_ratio_info
}

#' Calculate the calibration intercept (CITL) and its confidence interval
#' @noRd
get_cal_int <- function(obs, lps, z_val) {
  CITL_mod <- stats::glm(
    obs ~ 1,
    family = stats::binomial(link = "logit"),
    offset = lps
  )
  CalInt <- as.numeric(stats::coef(CITL_mod)[1])
  CalIntSE <- sqrt(stats::vcov(CITL_mod)[1, 1])
  CalInt_lower <- CalInt - (z_val * CalIntSE)
  CalInt_upper <- CalInt + (z_val * CalIntSE)

  cal_int_info <- list(
    "value" = CalInt,
    "se" = CalIntSE,
    "lower" = CalInt_lower,
    "upper" = CalInt_upper
  )

  cal_int_info
}

#' Calculate the calibration slope and its confidence interval
#' @noRd
get_cal_slope <- function(obs, lps, z_val) {
  CalSlope_mod <- stats::glm(
    obs ~ lps,
    family = stats::binomial(link = "logit")
  )
  CalSlope <- as.numeric(stats::coef(CalSlope_mod)[2])
  CalSlopeSE <- sqrt(stats::vcov(CalSlope_mod)[2, 2])
  CalSlope_lower <- CalSlope - (z_val * CalSlopeSE)
  CalSlope_upper <- CalSlope + (z_val * CalSlopeSE)

  cal_slope_info <- list(
    "value" = CalSlope,
    "se" = CalSlopeSE,
    "lower" = CalSlope_lower,
    "upper" = CalSlope_upper
  )

  cal_slope_info
}

#' Calculate the Area Under the Curve (AUC) and its confidence interval
#' @noRd
get_auc <- function(obs, probs, z_val) {
  roc_curve <- pROC::roc(
    response = obs,
    predictor = probs,
    direction = "<",
    levels = c(0, 1),
    ci = TRUE
  )
  AUC <- as.numeric(roc_curve$auc)
  AUCSE <- sqrt(pROC::var(roc_curve))
  AUC_lower <- AUC - (z_val * AUCSE)
  AUC_upper <- AUC + (z_val * AUCSE)

  auc_info <- list(
    "value" = AUC,
    "se" = AUCSE,
    "lower" = AUC_lower,
    "upper" = AUC_upper
  )

  auc_info
}

#' Calculate the R-squared values (Cox-Snell and Nagelkerke)
#' @noRd
get_r2 <- function(obs, lps) {
  R2_mod <- stats::glm(
    obs ~ -1,
    family = stats::binomial(link = "logit"),
    offset = lps
  )
  E <- sum(obs) # number of events in the validation data
  N <- length(obs) # number of observations in the validation data
  L_Null <- (E * log(E / N)) + ((N - E) * log(1 - (E / N)))
  LR <- -2 * (L_Null - as.numeric(stats::logLik(R2_mod)))
  MaxR2 <- 1 - exp((2 * L_Null) / length(obs))
  R2_coxsnell <- 1 - exp(-LR / length(obs))
  R2_Nagelkerke <- R2_coxsnell / MaxR2

  r2_info <- list(
    "r2_coxsnell" = R2_coxsnell,
    "r2_nagelkerke" = R2_Nagelkerke
  )

  r2_info
}

#' Calculate the Brier score and its confidence interval
#' @noRd
get_brier <- function(obs, probs, z_val) {
  N <- length(obs) # number of observations in the validation data
  BrierScore <- 1 / N * (sum((probs - obs)^2))
  Brier_var <- (N^(-2)) * (sum(((1 - (2 * probs))^2) * (probs * (1 - probs))))
  # Spiegelhalter, D. J. 1986 https://doi.org/10.1002/sim.4780050506
  BrierSE <- sqrt(Brier_var)
  Brier_lower <- BrierScore - (z_val * BrierSE)
  Brier_upper <- BrierScore + (z_val * BrierSE)

  brier_info <- list(
    "value" = BrierScore,
    "se" = BrierSE,
    "lower" = Brier_lower,
    "upper" = Brier_upper
  )

  brier_info
}
