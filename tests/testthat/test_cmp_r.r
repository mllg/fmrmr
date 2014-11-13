context("compare with r implementation")

rmrmr = function(time, status, x, nselect = ncol(x)) {
  assertNumeric(time, any.missing = FALSE)
  assertLogical(status, any.missing = FALSE, len = length(time))
  assertMatrix(x, mode = "double", any.missing = FALSE, nrow = length(time), col.names = "unique")
  assertCount(nselect)

  if (nselect == 0L)
    return(numeric(0L))
  p = ncol(x)
  nselect = min(nselect, ncol(x))
  selected = logical(p)
  indices = integer(nselect)
  cors = double(nselect)
  scores = numeric(nselect)

  relevance = apply(x, 2L, function(x, s) Hmisc::rcorr.cens(x, S = s)["C Index"], s = survival::Surv(time, status))
  best = BBmisc::getMaxIndex(relevance)
  selected[best] = TRUE
  scores[1L] = relevance[best]
  indices[1L] = best
  index.map = seq_len(p)

  if (nselect >= 2L) {
    cor.aggr = numeric(p)
    for(i in seq(from = 2L, to = nselect)) {
      cor.aggr = colMeans(abs(cor(x[, selected, drop = FALSE], x[, !selected, drop = FALSE])))
      score = relevance[!selected] / cor.aggr
      best = BBmisc::getMaxIndex(score)
      best.index = index.map[!selected][best]
      selected[best.index] = TRUE
      scores[i] = score[best]
      cors[i] = cor.aggr[best]
      indices[i] = best.index
    }
  }
  # list(index = indices, score = scores, cindex = relevance[indices], cor = cors)
  setNames(scores, colnames(x)[indices])
}


test_that("identical to R implementation", {
  set.seed(1)
  x = getData(n = 100, p = 20)
  y.r = rmrmr(x$time, x$status, x$x)
  y.pkg = calcMRMR(x$time, x$status, x$x)
  expect_equal(names(y.r), names(y.pkg))
  d = max(abs(y.r - y.pkg))
  expect_true(d < sqrt(.Machine$double.eps))
})
