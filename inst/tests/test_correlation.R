context("correlation")

test_that("pearson", {
  n = 200L
  p = 100L
  x = matrix(rnorm(n * p), n, p)

  v1 = aggr.cor(x, method = "cpp")
  v2 = aggr.cor(x, method = "R")
  expect_equal(v1, v2)
})
