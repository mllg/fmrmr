calcRelevanceFSelector = function(y, x, method) {
  f = get(method, envir = getNamespace("FSelector"))
  d = as.data.frame(x)
  d$.y = y
  f(.y ~ ., data = d)
}
