context("fmrmr")

test_that("fmrmr", {
  library(survival)
  library(Hmisc)
  cindex_r = function(time, status, x) 2 * abs(apply(x, 2L, function(x) rcorr.cens(x, S = Surv(time, status))[[1L]]) - 0.5)
  mean_pearson_r = function(x) colMeans(abs(cor(x)) - diag(ncol(x)))

  n = 200L
  p = 100L
  beta = c(rep(1, 10L), rep(0, p - 10L))
  x = matrix(rnorm(n * p), n, p)
  real.time = -(log(runif(n))) / (10*exp(drop(x %*% beta)))
  cens.time = rexp(n, rate = 1/10)
  status = real.time <= cens.time
  time = pmin(real.time, cens.time)

  expect_equal(fmrmr:::calcRedundance(x, method = "pearson"), mean_pearson_r(x))
  expect_equal(fmrmr:::calcRelevance(time, status, x), cindex_r(time, status, x))

  expect_equal(fmrmr(time, status, x, redundance = "pearson"),
               cindex_r(time, status, x) - mean_pearson_r(x))
})
