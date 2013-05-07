#' Calculate the concordance index for survival data.
#'
#' @param surv [\code{Surv}]\cr
#'   Survival object of right censored survival times. See \code{\link[survival]{Surv}}.
#' @param x [\code{matrix(n*p)}]\cr
#'   An matrix of \code{n} observations and \code{p} features.
#' @param method [character(1)]\cr
#'   Possible choices are \dQuote{cpp} and \dQuote{R}. Mainly for testing purposes.
#'   This argument may be removed in a future release.
#'   Default is \dQuote{cpp}.
#' @return [\code{numeric}] Numeric vector of length \code{p} containing the concordance indices
#'   for all features provided in \code{x}.
#' @export
cindex = function(surv, x, method = "cpp") {
  if (!is.Surv(surv))
    stop("Argument 'surv' must be a Surv object")
  if (!attr(surv, "type") == "right")
    stop("Only right-censored survival times supported")

  if (!is.matrix(x))
    x = as.matrix(x)
  if (typeof(x) != "double")
    storage.mode(x) = "double"

  if (nrow(x) != nrow(surv))
    stop("Length mismatch: Argmuments 'surv' and 'x' must have equal number of rows")

  method = match.arg(method, choices = c("cpp", "Hmisc"))

  if (any(is.na(surv)) || any(is.na(x)))
    stop("Handling of NAs is not yet implemented")

  if (method == "Hmisc") {
    return(apply(x, 2L, function(x) rcorr.cens(x, S = surv)[[1L]]))
  }

  if (is.unsorted(surv[, 1L])) {
    # TODO do this w/o a copy in c++
    ord = order(surv[, 1L])
    surv = surv[ord]
    x = x[ord,, drop = FALSE]
  }

  time = as.double(surv[, 1L])
  status = as.double(surv[, 2L])

  return(.Call("cindex", time, status, x, PACKAGE = "fmrmr"))
}
