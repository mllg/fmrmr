#' Calculate the concordance index for survival data.
#'
#' @param time [\code{numeric}]\cr
#'   Vector of length \code{n} providing (possibly right censored) survival times.
#' @param status [\code{logical}]\cr
#'   Vectpr of length \code{n} providing censoring informations: \code{FALSE} for
#'   censored observations, \code{TRUE} for uncensored.
#' @param x [\code{matrix(n*p)}]\cr
#'   An matrix of \code{n} observations and \code{p} features/scores.
#' @param method [character(1)]\cr
#'   Possible choices are \dQuote{cpp} and \dQuote{R}. Mainly for testing purposes.
#'   This argument may be removed in a future release.
#'   Default is \dQuote{cpp}.
#' @return [\code{numeric}] Numeric vector of length \code{p} containing the concordance indices
#'   for all features provided in \code{x}.
#' @export
cindex = function(time, status, x, method = "cpp") {
  if (!is.matrix(x))
    x = as.matrix(x)
  if (typeof(x) != "double")
    storage.mode(x) = "double"
  if (length(time) != length(status) || length(time) != nrow(x))
    stop("Length mismatch: Argmuments 'time', 'status' and 'x' must have the same number of rows")

  method = match.arg(method, choices = c("cpp", "Hmisc"))

  if (method == "Hmisc") {
    return(apply(x, 2L, function(x) rcorr.cens(x, S = Surv(time, status))[[1L]]))
  }

  return(.Call("cindex", as.double(time), as.logical(status), x, PACKAGE = "fmrmr"))
}
