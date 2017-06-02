context("test quanteda_options")

test_that("quanteda_options initialize works correctly", {
    quanteda_options(reset = TRUE)
    
    expect_equal(
        quanteda_options(),
        list(threads = getOption("quanteda_threads"),
             verbose = getOption("quanteda_verbose"),
             print_dfm_max_ndoc = getOption("quanteda_print_dfm_max_ndoc"),
             print_dfm_max_nfeature = getOption("quanteda_print_dfm_max_nfeature"))
    )
})


test_that("quanteda_options returns an error for non-existing options", {
    expect_error(
        quanteda_options(notanoption = TRUE),
        "notanoption is not a valid quanteda option"
    )
    expect_error(
        quanteda_options("notanoption"),
        "notanoption is not a valid quanteda option"
    )
})


test_that("quanteda_options works correctly to set options", {
    quanteda_options(verbose = TRUE)
    expect_equal(
        quanteda_options("verbose"),
        getOption("quanteda_verbose")
    )
    
    quanteda_options(threads = 2)
    expect_equal(
        quanteda_options("threads"),
        getOption("quanteda_threads")
    )
    
    quanteda_options(print_dfm_max_ndoc = 50L)
    expect_equal(
        quanteda_options("print_dfm_max_ndoc"),
        getOption("quanteda_print_dfm_max_ndoc")
    )
    
    quanteda_options(print_dfm_max_nfeature = 50L)
    expect_equal(
        quanteda_options("print_dfm_max_nfeature"),
        getOption("quanteda_print_dfm_max_nfeature")
    )
})



