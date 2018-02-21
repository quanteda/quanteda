Sys.setenv("R_TESTS" = "")

library(testthat)
library(quanteda)

ops <- quanteda_options()
quanteda_options(reset = TRUE)
# change threads to 2 for CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    quanteda_options(threads = 2)
}

test_check("quanteda")
quanteda_options(ops)

