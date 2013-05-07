#' Calculate the aggregated correlation of matrix columns.
#'
#' @param x [\code{matrix}]\cr
#'   An \code{n*p} matrix of \code{n} observations and \code{p} features.
#' @param type [\code{character(1)}]\cr
#'   Type of correlation. Right now, only pearson correlation is supported.
#' @param aggr [\code{character(1)}]\cr
#'   Aggregation function. Supported are \dQuote{mean} and \dQuote{max}. Default is \dQuote{mean}.
#' @param method [character(1)]\cr
#'   Possible choices are \dQuote{cpp} and \dQuote{R}. Mainly for testing purposes.
#'   This argument may be removed in a future release.
#'   Default is \dQuote{cpp}.
#' @return [\code{numeric}] Numeric vector of length \code{p} containing the concordance indices
#'   for all features provided in \code{x}.
#' @export
aggr.cor = function(x, type = "pearson", aggr = "mean", method = "cpp") {
  if (!is.matrix(x))
    x = as.matrix(x)
  if (typeof(x) != "double")
    storage.mode(x) = "double"

  type = match.arg(type, choices = c("pearson"))
  aggr = match.arg(aggr, choices = c("mean", "max"))
  method = match.arg(method, choices = c("cpp", "R"))

  if (aggr == "mean") {
    if (method == "R")
      return(colMeans(abs(cor(x)) - diag(ncol(x))))
    return(.Call("meancor", x, PACKAGE = "fmrmr"))
  } else {
    if (method == "R")
      return(apply(abs(cor(x)) - diag(ncol(x)), 2L, max))
    return(.Call("maxcor", x, PACKAGE = "fmrmr"))
  }
}
