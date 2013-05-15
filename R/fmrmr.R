#' Calculate mRMR feature scores.
#'
#' @param time [\code{numeric}]\cr
#'   Vector of length \code{n} providing (possibly right censored) survival times.
#' @param status [\code{logical}]\cr
#'   Vectpr of length \code{n} providing censoring informations: \code{FALSE} for
#'   censored observations, \code{TRUE} for uncensored.
#' @param x [\code{matrix(n*p)}]\cr
#'   An matrix of \code{n} observations and \code{p} features/scores.
#' @param combine [character(1)]\cr
#'   How to combine redundance and relevance. Possible choices are \dQuote{difference}
#'   and \dQuote{quotient}.
#'   Default is \dQuote{difference}.
#' @param method [character(1)]\cr
#'   Possible choices are \dQuote{cpp} and \dQuote{R}. Mainly for testing purposes.
#'   This argument may be removed in a future release.
#'   Default is \dQuote{cpp}.
#' @return [\code{numeric}] Numeric vector of length \code{p} containing the mRMR scores
#'   for all features provided in \code{x}.
#' @export
fmrmr = function(time, status, x, combine = "difference", method = "cpp") {
  if (!is.double(time))
    time = as.double(time)
  if (!is.logical(status))
    status = as.logical(status)
  if (!is.matrix(x))
    x = as.matrix(x)
  if (is.double(x))
    storage.mode(x) = "double"
  if (length(time) != length(status) || length(time) != nrow(x))
    stop("Length mismatch: Argmuments 'time', 'status' and 'x' must have the same number of rows")
  method = match.arg(method, choices = c("cpp", "R"))
  method = match.arg(combine, choices = c("difference", "quotient"))

  if (method == "R") {
    rel = apply(x, 2L, function(x) rcorr.cens(x, S = Surv(time, status))[[1L]])
    red = colMeans(abs(cor(x)) - diag(ncol(x)))
  } else {
    rel = .Call("rel_cindex", time, status, x, PACKAGE="fmrmr")
    red = .Call("red_mean_abs_pearson", x, PACKAGE="fmrmr")
  }

  switch(combine,
         "difference" = rel - red,
         "quotient" = rel / red)

}
