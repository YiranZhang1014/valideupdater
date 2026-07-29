library(testthat)
library(data.table)

test_that("predict.logistic Correct log-odds", {
  # 1. Build model
  coefs <- c("(Intercept)" = -2.0, age = 0.05, bmi = 0.1)
  mod <- build_model(coefs, type = "logistic")

  # 2. Create test data
  test_dt <- data.table(
    age = c(20, 40),
    bmi = c(20, 25)
  )

  # 3. Calculate the results
  # Row 1: -2.0 + 20*0.05 + 20*0.1 = -2.0 + 1.0 + 2.0 = 1.0
  # Row 2: -2.0 + 40*0.05 + 25*0.1 = -2.0 + 2.0 + 2.5 = 2.5
  expected_lp <- c(1.0, 2.5)
  expected_prob <- 1 / (1 + exp(-expected_lp))

  # Test type = "probs"
  pred_res <- predict(mod, test_dt)
  expect_equal(pred_res$prob, expected_prob)

  message("------------------------------------")

  # Test type = "lp"
  pred_res <- predict(mod, test_dt, type = "lp")
  expect_equal(pred_res$lp, expected_lp)
})

test_that("predict.logistic Lack variables in new_data", {
  coefs <- c("(Intercept)" = -1.0, age = 0.05, bmi = 0.1)
  mod <- build_model(coefs, type = "logistic")

  # Lack of BMI
  bad_dt <- data.table(age = c(20, 30))

  expect_error(
    predict(mod, bad_dt),
    regexp = "The following predictor variables are missing"
  )
})
