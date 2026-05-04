# library(testthat)

mock_data <- function() {
  set.seed(123)
  n <- 100
  obs <- rbinom(n, 1, 0.5)
  probs <- runif(n)
  lps <- log(probs / (1 - probs))

  list(obs = obs, probs = probs, lps = lps)
}

test_that("get_oe_ratio calculates observed-to-expected ratio correctly", {
  data <- mock_data()

  # Execute the function
  res <- get_oe_ratio(obs = data$obs, probs = data$probs, z_val = 1.96)

  # Check the overall structure and behaviour
  expect_type(res, "list")
  expect_named(res, c("value", "se", "lower", "upper"))
  expect_type(res$value, "double")
})

test_that("get_cal_int calculates calibration-in-the-large correctly", {
  data <- mock_data()

  # Execute the function
  res <- get_cal_int(obs = data$obs, lps = data$lps, z_val = 1.96)

  # Check the overall structure and behaviour
  expect_type(res, "list")
  expect_named(res, c("value", "se", "lower", "upper"))
  expect_type(res$value, "double")
})

test_that("get_cal_slope calculates calibration slope correctly", {
  data <- mock_data()

  # Execute the function
  res <- get_cal_slope(obs = data$obs, lps = data$lps, z_val = 1.96)

  # Check the overall structure and behaviour
  expect_type(res, "list")
  expect_named(res, c("value", "se", "lower", "upper"))
  expect_type(res$value, "double")
})

test_that("get_auc calculates area under the curve correctly", {
  data <- mock_data()

  # Execute the function
  res <- get_auc(obs = data$obs, probs = data$probs, z_val = 1.96)

  # Check the overall structure and behaviour
  expect_type(res, "list")
  expect_named(res, c("value", "se", "lower", "upper"))
  expect_type(res$value, "double")
})

test_that("get_r2 calculates R-squared values correctly", {
  data <- mock_data()

  # Execute the function
  res <- get_r2(obs = data$obs, lps = data$lps)

  # Check the overall structure and behaviour
  expect_type(res, "list")
  expect_named(res, c("r2_coxsnell", "r2_nagelkerke"))
  expect_type(res$r2_coxsnell, "double")
  expect_type(res$r2_nagelkerke, "double")
})

test_that("get_brier calculates Brier score correctly", {
  data <- mock_data()

  # Execute the function
  res <- get_brier(obs = data$obs, probs = data$probs, z_val = 1.96)

  # Check the overall structure and behaviour
  expect_type(res, "list")
  expect_named(res, c("value", "se", "lower", "upper"))
  expect_type(res$value, "double")
})
