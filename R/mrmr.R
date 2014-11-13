#' @title Minimum Redundancy, maximum relevance
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
calcMRMR = function(time, status, x, nselect = ncol(x)) {
  assertNumeric(time, lower = 0, any.missing = FALSE)
  assert(
    checkLogical(status, any.missing = FALSE, len = length(time)),
    checkIntegerish(status, any.missing = FALSE, len = length(time), lower = 0, upper = 1)
  )
  assertMatrix(x, mode = "double", any.missing = FALSE, nrows = length(time), col.names = "unique")
  assertInt(nselect, lower = 0L, upper = ncol(x))
  if (nselect == 0L)
    return(numeric(0L))
  res = mrmr(time, as.logical(status), x, as.integer(nselect))
  setNames(res$score, colnames(x)[res$index + 1L])
}
