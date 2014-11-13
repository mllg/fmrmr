getData = function(n = 20, p = 10) {
  time = rexp(n, 1/10)
  repeat {
    status = sample(c(TRUE, FALSE), n, replace = TRUE)
    n.cens = sum(status)
    if (n.cens > 0 && n.cens < n)
      break
  }
  x = matrix(rnorm(n*p), nrow = n)
  colnames(x) = sprintf("x%02i", seq_len(ncol(x)))
  list(time = time, status = status, x = x)
}
