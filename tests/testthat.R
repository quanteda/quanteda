Sys.setenv("R_TESTS" = "")

library(testthat)
library(quanteda)

Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = TRUE)

quanteda_options(reset = TRUE)
#quanteda_options(threads = 2)

test_check("quanteda")

