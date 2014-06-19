#' Calculate mRMR feature scores.
#'
#' @param time [\code{numeric}]\cr
#'   Vector of length \code{n} providing (possibly right censored) survival times.
#' @param status [\code{logical}]\cr
#'   Vectpr of length \code{n} providing censoring informations: \code{FALSE} for
#'   censored observations, \code{TRUE} for uncensored.
#' @param x [\code{matrix(n*p)}]\cr
#'   An matrix of \code{n} observations and \code{p} features/scores.
#' @param relevance [character(1)]\cr
#'   Measure for relevance. Possible values are \dQuote{cindex}.
#'   For \dQuote{cindex}, the relevance is determined by twice the absolute distance
#'   to \code{0.5} (random predictor), resulting in a relevance between 0 and 1.
#'   Default is \dQuote{cindex}.
#' @param redundance [character(1)]\cr
#'   Measure for redundance Possible values are \dQuote{pearson} and \dQuote{mi}.
#'   Default is \dQuote{mi}.
#' @param combine [character(1)]\cr
#'   How to combine redundance and relevance. Possible values are \dQuote{difference}
#'   and \dQuote{quotient}.
#'   Default is \dQuote{difference}.
#' @return [\code{numeric}] Numeric vector of length \code{p} containing the mRMR scores
#'   for all features provided in \code{x}.
#' @export
fmrmr = function(time, status, x, relevance = "cindex", redundance = "mi", combine = "difference") {
  if (!is.double(time))
    time = as.double(time)
  if (!is.logical(status))
    status = as.logical(status)
  if (!is.matrix(x))
    x = as.matrix(x)
  if (!is.double(x))
    storage.mode(x) = "double"
  if (length(status) != length(time) || length(time) != nrow(x))
    stop("Dimension mismatch: Argmuments 'time', 'status' and 'x' must have the same number of rows")
  relevance = match.arg(relevance, choices = c("cindex"))
  redundance = match.arg(redundance, choices = c("mi", "pearson"))
  combine = match.arg(combine, choices = c("difference", "quotient"))

  rel = calcRelevance(time, status, x, method = relevance)
  red = calcRedundance(x, method = redundance)

  switch(combine,
         "difference" = rel - red,
         "quotient" = rel / red
  )
}

#' @useDynLib fmrmr rel_cindex
calcRelevance = function(time, status, x, method = "cindex") {
  switch(method,
    cindex = .Call("rel_cindex", time, status, x, PACKAGE = "fmrmr"),
    stop("Unknown method in relevance calculation: ", method)
  )
}


#' @useDynLib fmrmr red_mean_abs_pearson
calcRedundance = function(x, method = "mi") {
  switch(method,
    pearson = .Call("red_mean_abs_pearson", x, PACKAGE = "fmrmr"),
    mi = .Call("red_mean_mim", x, PACKAGE = "fmrmr"),
    stop("Unknown method in redundance calculation: ", method)
  )
}
