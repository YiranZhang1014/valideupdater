#' Pool Validation Metrics using Rubin's Rules and Print Formatted Output
#'
#' @param valid_res_dt A data.frame or data.table containing metric values and standard errors per imputation.
#' @param metrics Character vector specifying the metrics to pool.
#' @param level Confidence level for calculating the lower and upper bounds (default is 0.95).
#' @param digits Number of decimal places for rounding the output (default is 3).
#' @param quiet Logical. If TRUE, suppresses console printing (default is FALSE).
#'
#' @return A named character vector with formatted strings: "Estimate (Lower - Upper)".
pool <- function(
  valid_res_dt,
  metrics = c("auc", "cal_int", "cal_slope", "oe", "brier"),
  level = 0.95,
  digits = 2,
  quiet = FALSE
) {
  dt <- data.table::as.data.table(valid_res_dt)
  formatted_results <- character()

  for (m in metrics) {
    val_col <- paste0(m, ".value")
    se_col <- paste0(m, ".se")

    if (!val_col %in% names(dt) || !se_col %in% names(dt)) {
      warning(sprintf(
        "Columns for metric '%s' (%s, %s) were not found in dt. Skipping.",
        m,
        val_col,
        se_col
      ))
      next
    }

    val <- dt[[val_col]]
    se <- dt[[se_col]]
    M <- length(val)

    if (M < 2) {
      stop(
        "Rubin's Rules require results from at least 2 imputed datasets (M >= 2)."
      )
    }

    # Point estimate and within-imputation variance
    pooled_est <- mean(val)
    W_bar <- mean(se^2)

    # Between-imputation variance
    B <- var(val)

    # Total variance and pooled standard error
    T_var <- W_bar + (1 + 1 / M) * B
    pooled_se <- sqrt(T_var)

    # Degrees of freedom calculation
    r <- (1 + 1 / M) * (B / W_bar)
    nu <- if (is.na(r) || r == 0) Inf else (M - 1) * (1 + 1 / r)^2

    # Critical value based on t-distribution
    alpha <- 1 - level
    t_crit <- if (is.infinite(nu)) {
      qnorm(1 - alpha / 2)
    } else {
      qt(1 - alpha / 2, df = nu)
    }

    # Calculate bounds
    pooled_lower <- pooled_est - t_crit * pooled_se
    pooled_upper <- pooled_est + t_crit * pooled_se

    # Format the numbers to fixed decimal places
    fmt <- sprintf("%%.%1$df (%%.%1$df - %%.%1$df)", digits)
    res_str <- sprintf(fmt, pooled_est, pooled_lower, pooled_upper)

    formatted_results[m] <- res_str
  }

  # Print directly to the console unless quiet = TRUE
  if (!quiet && length(formatted_results) > 0) {
    cat("\n=== Pooled Validation Results (Rubin's Rules) ===\n")
    for (name in names(formatted_results)) {
      cat(sprintf("%-12s: %s\n", name, formatted_results[name]))
    }
    cat("=================================================\n\n")
  }

  # Return the named character vector invisibly
  invisible(formatted_results)
}
