#' @export
#' @rdname fmrmr
fmrmrSurv = function(time, status, x, relevance = "cindex", redundance = "pearson", combine = "difference", alpha = 1) {
  assertNumeric(time)
  n = length(time)
  assertLogical(status, len = n)
  assertChoice(relevance,  c("cindex"))
  x = checkArgs(n, x, redundance, combine, alpha)

  rel = calcRelevance(time, status, x, method = relevance)
  red = calcRedundance(x, method = redundance)
  combineValues(x, combine, rel, red, alpha)
}

#' @useDynLib fmrmr rel_cindex
calcRelevance = function(time, status, x, method = "cindex") {
  switch(method,
    cindex = .Call("rel_cindex", time, status, x, PACKAGE = "fmrmr"),
    stop("Unknown method in relevance calculation: ", method)
  )
}


#' @useDynLib fmrmr red_mean_abs_pearson
calcRedundance = function(x, method) {
  switch(method,
    pearson = .Call("red_mean_abs_pearson", x, PACKAGE = "fmrmr"),
    stop("Unknown method in redundance calculation: ", method)
  )
}
