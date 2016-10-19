context("return value")

test_that("mrmr works", {
  x = getData()

  y = mrmr_survival(x$time, x$status, x$x)
  expect_true(is.numeric(y))
  expect_equal(length(y), ncol(x$x))
  expect_true(setequal(names(y), colnames(x$x)))

  y = mrmr_survival(x$time, x$status, x$x, nselect = 2)
  expect_true(is.numeric(y))
  expect_equal(length(y), 2)
  expect_true(all(names(y) %in% colnames(x$x)))
})

test_that("mrmr selects correct variable", {
  set.seed(1)
  x = getData()
  x$x = cbind(x$x, findme = x$time)

  y = mrmr_survival(x$time, x$status, x$x)
  expect_equal(names(y)[1], "findme")
})
