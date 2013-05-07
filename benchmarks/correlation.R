library(fmrmr)
library(microbenchmark)

n = 200L
p = 1000L
x = matrix(rnorm(n * p), n, p)

mb = microbenchmark(
  aggr.cor(x, method = "cpp"),
  aggr.cor(x, method = "R")
, times = 10L)
print(mb, unit = "relative", order = "median")


n = 200L
p = 5000L
x = matrix(rnorm(n * p), n, p)

mem.before = gc(reset = TRUE)
aggr.cor(x, method = "cpp")
mem.after = gc()
mem.after - mem.before

mem.before = gc(reset = TRUE)
aggr.cor(x, method = "R")
mem.after = gc()
mem.after - mem.before
