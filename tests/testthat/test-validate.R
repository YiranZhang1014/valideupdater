# library(testthat)

library(testthat)

mock_data <- function() {
  set.seed(123)
  n <- 100
  obs <- rbinom(n, 1, 0.5)
  probs <- runif(n)
  lps <- log(probs / (1 - probs))

  list(obs = obs, probs = probs, lps = lps)
}

test_that("validate.logistic handles invalid inputs correctly", {
  # Mock data
  mock_data <- mock_data()

  # Test 1: Level must be between 0 and 1
  expect_error(
    validate.logistic(
      mock_data$obs,
      mock_data$probs,
      mock_data$lps,
      level = 1.5
    ),
    "Level must be a single numeric value between 0 and 1"
  )
  expect_error(
    validate.logistic(
      mock_data$obs,
      mock_data$probs,
      mock_data$lps,
      level = -0.1
    ),
    "Level must be a single numeric value between 0 and 1"
  )

  # Test 2: Invalid metric name
  expect_error(validate.logistic(
    mock_data$obs,
    mock_data$probs,
    mock_data$lps,
    metrics = "invalid_metric"
  ))
})

test_that("validate.logistic handles infinite lps (0/1 probabilities)", {
  # Create data with an infinite log-prediction
  mock_data <- mock_data()
  mock_data$probs[1] <- 0 # This will create an infinite log-prediction
  mock_data$lps <- log(mock_data$probs / (1 - mock_data$probs))

  # We expect a warning when observations are deleted
  expect_warning(
    validate.logistic(
      mock_data$obs,
      mock_data$probs,
      mock_data$lps,
      metrics = "auc"
    )
  )
})

test_that("validate.logistic returns only requested metrics", {
  # Mock data
  obs <- c(0, 1, 0, 1)
  probs <- c(0.2, 0.8, 0.3, 0.7)
  lps <- log(probs / (1 - probs))

  # Request only 'auc' and 'brier'
  results <- validate.logistic(obs, probs, lps, metrics = c("auc", "brier"))

  # Check that requested metrics exist
  expect_true("auc" %in% names(results))
  expect_true("brier" %in% names(results))

  # Check that unrequested metrics do NOT exist
  expect_false("cal_slope" %in% names(results))

  # Check that level is preserved in output
  expect_equal(results$level, 0.95)
})

test_that("validate.logistic respects the 'level' argument", {
  # Mock data
  mock_data <- mock_data()

  res_95 <- validate.logistic(
    mock_data$obs,
    mock_data$probs,
    mock_data$lps,
    level = 0.95
  )
  res_90 <- validate.logistic(
    mock_data$obs,
    mock_data$probs,
    mock_data$lps,
    level = 0.90
  )

  expect_equal(res_95$level, 0.95)
  expect_equal(res_90$level, 0.90)
})
