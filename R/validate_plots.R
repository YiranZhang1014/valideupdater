get_pred_dist <- function(obs, prob, xlab = "Predicted Probability") {
  plot_df <- data.table::data.table(
    "Prob" = prob,
    "Outcome" = factor(ifelse(obs == 1, "Event", "No Event"))
  )
  PR_dist <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$Outcome,
      y = .data$Prob
    )
  ) +
    ggplot2::geom_violin(
      position = ggplot2::position_dodge(width = .75),
      linewidth = 1
    ) +
    ggplot2::geom_boxplot(
      width = 0.1,
      outlier.shape = NA
    ) +
    ggplot2::ylab(xlab) +
    ggplot2::theme_bw(base_size = 12)

  PR_dist
}

cal_plot <- function(
  model_type,
  obs,
  prob,
  lps,
  xlim = c(0, 1),
  ylim = c(0, 1),
  xlab = "Predicted Probability",
  ylab = "Observed Probability",
  pred_rug = FALSE,
  cal_plot_n_sample = NULL
) {
  if (length(unique(prob)) <= 10) {
    # allows handling of intercept-only models
    stop(
      "Very low unique predicted risks - calplot not possible; call again with cal_plot = FALSE"
    )
  } else {
    cal_plot_out <- flex_calplot(
      model_type = "logistic",
      ObservedOutcome = obs,
      Prob = prob,
      lps = lps,
      xlim = xlim,
      ylim = ylim,
      xlab = xlab,
      ylab = ylab,
      pred_rug = pred_rug,
      cal_plot_n_sample = cal_plot_n_sample
    )
  }

  cal_plot_out
}
