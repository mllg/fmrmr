#' @title Minimum Redundancy, maximum relevance
#'
#' Uses Harrells C index for relevance and the Pearson correlation to quantify redundancy.
#'
#' @param time [\code{numeric}]\cr
#'   Vector of survival times.
#' @param status [\code{logical}]\cr
#'   Vector of survival event indicators.
#' @param x [\code{matrix[double]}]\cr
#'   Matrix of \code{[observations]x[features]}.
#' @param nselect [\code{integer(1)}]\cr
#'   Number of features to score. Default is all features.
#' @return [\code{named numeric}]: Vector of scores, named with column
#'   names of \code{x}, in order of selection.
#' @export
mrmr_survival = function(time, status, x, nselect = ncol(x)) {
  assertNumeric(time, lower = 0, any.missing = FALSE)
  assert(
    checkLogical(status, any.missing = FALSE, len = length(time)),
    checkIntegerish(status, any.missing = FALSE, len = length(time), lower = 0, upper = 1)
  )
  assertMatrix(x, mode = "double", any.missing = FALSE, nrows = length(time), col.names = "unique")
  assertInt(nselect, lower = 0L, upper = ncol(x))
  if (nselect == 0L)
    return(setNames(numeric(0L), character(0L)))
  relevance = cindex(time, as.logical(status), x)
  res = mrmr_generic(relevance, x, as.integer(nselect))
  setNames(res$score, colnames(x)[res$index + 1L])
}

#' @rdname mrmr_survival
#' @export
mrmr_with_relevance = function(relevance, x, nselect = ncol(x)) {
  assertNumeric(relevance, any.missing = FALSE)
  assertMatrix(x, mode = "double", any.missing = FALSE, ncols = length(relevance), col.names = "unique")
  assertInt(nselect, lower = 0L, upper = ncol(x))
  if (nselect == 0L)
    return(setNames(numeric(0L), character(0L)))
  res = mrmr_generic(relevance, x, as.integer(nselect))
  setNames(res$score, colnames(x)[res$index + 1L])
}
