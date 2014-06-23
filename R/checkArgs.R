checkArgs = function(n, x, redundance, combine, alpha) {
  if (is.data.frame(x))
    x = as.matrix(x)
  assertMatrix(x, nrows = n)
  assertChoice(redundance,  c("pearson"))
  assertChoice(combine,  c("difference", "quotient"))
  assertNumber(alpha)
  return(x)
}
