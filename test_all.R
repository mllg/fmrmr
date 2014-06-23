library(methods)
library(devtools)
library(testthat)
library(Rcpp)

if (interactive()) {
  load_all(".")
} else {
  library(fmrmr)
}
test_dir("tests/testthat")
