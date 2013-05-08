library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all(".", reset=TRUE)
} else {
  library(fmrmr)
}
test_dir("inst/tests")

