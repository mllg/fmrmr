#' @param classes [\code{factor}]\cr
#'   Classs labels.
#' @export
#' @rdname fmrmr
fmrmrClassif = function(classes, x, relevance = "chi.squared", redundance = "pearson", combine = "difference", alpha = 1) {
  assertFactor(classes)
  assertChoice(relevance, c("chi.squared", "symmetrical.uncertainty", "information.gain"))
  x = checkArgs(length(classes), x, redundance, combine, alpha)

  rel = calcRelevanceFSelector(classes, x, method = relevance)
  red = calcRedundance(x, method = redundance)
  combineValues(x, combine, rel, red, alpha)
}


