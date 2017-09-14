Sys.setenv("R_TESTS" = "")

library(testthat)
library(quanteda)

ops <- quanteda_options()
quanteda_options(reset = TRUE)
test_check("quanteda")
quanteda_options(ops)

