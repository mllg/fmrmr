#' @param y [\code{numeric}]\cr
#'   Continuous target values.
#' @export
#' @rdname fmrmr
fmrmrRegr = function(y, x, relevance = "rank.correlation", redundance = "pearson", combine = "difference", alpha = 1) {
  assertNumeric(y)
  assertChoice(relevance,  c("linear.correlation", "rank.correlation"))
  x = checkArgs(length(y), x, redundance, combine, alpha)

  x = as.matrix(x)
  redundance = match.arg(redundance, choices = c("mi", "pearson"))
  rel = calcRelevanceFSelector(y, x, method = relevance)
  red = calcRedundance(x, method = redundance)
  combineValues(x, combine, rel, red, alpha)
}



