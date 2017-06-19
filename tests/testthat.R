Sys.setenv("R_TESTS" = "")

library(testthat)
library(quanteda)

quanteda_options(reset = TRUE)
test_check("quanteda")

