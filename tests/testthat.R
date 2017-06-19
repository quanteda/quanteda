Sys.setenv("R_TESTS" = "")

library(testthat)
library(quanteda)

qopts <- quanteda_options()
quanteda_options(reset = TRUE)
test_check("quanteda")
quanteda_options(qopts)

