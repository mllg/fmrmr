context("fmrmr")

mean_pearson_r = function(x) colMeans(abs(cor(x)) - diag(ncol(x)))

test_that("fmrmr works with surv", {
  library(survival)
  library(Hmisc)
  cindex_r = function(time, status, x) 2 * abs(apply(x, 2L, function(x) rcorr.cens(x, S = Surv(time, status))[[1L]]) - 0.5)

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

  expect_equal(fmrmrSurv(time, status, x, redundance = "pearson")[, "score"],
    cindex_r(time, status, x) - mean_pearson_r(x))
})

test_that("fmrmr works with classif", {
  x = iris[,1:4]
  y = iris$Species
  v = fmrmrClassif(y, x)
  expect_true(is.data.frame(v) && nrow(v) == ncol(x) && !any(is.na(v)))
  expect_true(setequal(rownames(v), colnames(x)))

  v = fmrmrClassif(y, x, redundance = "pearson", alpha = 0)
  w = mean_pearson_r(x)
  expect_equal(v$score, -w, check.names = FALSE)
  expect_equal(v$score, -v$red)
})

test_that("fmrmr works with regr", {
  x = iris[, 1:4]
  y = rnorm(nrow(x))
  v = fmrmrRegr(y, x)
  expect_true(is.data.frame(v) && nrow(v) == ncol(x) && !any(is.na(v)))
  expect_true(setequal(rownames(v), colnames(x)))
})


