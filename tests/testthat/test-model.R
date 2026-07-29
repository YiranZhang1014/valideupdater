library(testthat)

test_that("build_model Create correct model", {
    # Correct input:
    coefs1 <- c("(Intercept)" = -1.5, age = 0.05, bmi = 0.1)
    mod1 <- build_model(coefs1)

    expect_s3_class(mod1, "logistic")
    expect_equal(mod1$type, "logistic")
    expect_equal(names(mod1$coefs)[1], "(Intercept)")
    expect_equal(mod1$coefs[["(Intercept)"]], -1.5)

    # Alternative correted intecept:
    coefs2 <- c("intercept" = -2.0, age = 0.03)
    mod2 <- build_model(coefs2)

    # 验证名称是否成功标准化为 "(Intercept)"
    expect_named(mod2$coefs, c("(Intercept)", "age"))
    expect_equal(mod2$coefs[["(Intercept)"]], -2.0)
})


test_that("build_model Catch and throw errors", {
    # Lack of intercept
    no_intercept <- c(age = 0.05, bmi = 0.1)
    expect_error(
        build_model(no_intercept),
        regexp = "The coefs must include an Intercept term"
    )

    # Multiple intercepts
    multi_intercept <- c("(Intercept)" = -1.0, "intercept" = -2.0, age = 0.05)
    expect_error(
        build_model(multi_intercept),
        regexp = "Multiple terms match 'Intercept'"
    )

    # Wrong type
    valid_coefs <- c("(Intercept)" = -1.0, age = 0.05)
    expect_error(
        build_model(valid_coefs, type = "cox"),
        regexp = "'arg' should be one of"
    )
})
