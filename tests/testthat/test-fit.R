

library(testthat)


#
# Classes are recognized as S3
#
test_that("blblm recognized as S3", {
  fit <- blblm(Sepal.Length ~ Petal.Width, data = iris, m = 10, B = 15)
  expect_s3_class(fit, "blblm")

})

test_that("blbglm recognized as S3", {
  fit <- blbglm(Sepal.Length ~ Species, data = iris, m = 10, B = 15)
  expect_s3_class(fit, "blbglm")

})


test_that("Features are expected size", {
  fit <- blblm(Sepal.Length ~ Petal.Width + Petal.Length, data = iris, m = 10, B = 15)

  expect_equivalent(length(sigma(fit)), 1) # Return only one sigma
  expect_equivalent(length(coef(fit)), 3)  # Return two features and an intercept
  expect_equivalent(nrow(confint(fit)), 2) # Conf int returns all features

})
