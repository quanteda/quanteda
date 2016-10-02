Sys.setenv("R_TESTS" = "")

library(testthat)
library(quanteda)

test_check("quanteda")
