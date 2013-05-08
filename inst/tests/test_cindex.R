context("cindex")

test_that("cindex", {
  n = 200L
  p = 100L
  beta = c(rep(1, 10L), rep(0, p - 10L))
  x = matrix(rnorm(n * p), n, p)
  real.time = -(log(runif(n))) / (10*exp(drop(x %*% beta)))
  cens.time = rexp(n, rate = 1/10)
  status = real.time <= cens.time
  time = pmin(real.time, cens.time)

  v1 = cindex(time, status, x, method = "cpp")
  v2 = cindex(time, status, x, method = "Hmisc")
  expect_equal(v1, v2)
})
