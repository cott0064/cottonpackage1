library(testthat)

test_that("myncurve return correct mu", {
  result = myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(result$mu, 10)
})

test_that("myncurve return correct sigma", {
  result = myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(result$sigma, 5)
})

test_that("myncurve return correct a", {
  result = myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(result$a, 6)
})
