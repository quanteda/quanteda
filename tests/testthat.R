Sys.setenv("R_TESTS" = "")
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = TRUE)

library(testthat)
library(quanteda)

ops <- quanteda_options()
quanteda_options(reset = TRUE)
test_check("quanteda")
quanteda_options(ops)
