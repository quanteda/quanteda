test_that("quanteda_options initialization works", {
    options('quanteda_initialized' = NULL)
    quanteda_options()
    expect_true(getOption('quanteda_initialized'))
})

test_that("quanteda_options initialize works correctly", {
    
    quanteda_options(verbose = TRUE, 
                     print_dfm_max_ndoc = 1L, 
                     print_dfm_max_nfeat = NULL)
    quanteda_options(initialize = TRUE)
    expect_equal(quanteda_options("verbose"), TRUE)
    expect_equal(quanteda_options("print_dfm_max_ndoc"), 1L)
    expect_equal(quanteda_options("print_dfm_max_nfeat"), 10L)

})

test_that("quanteda_options returns an error for non-existing options", {
    expect_error(
        quanteda_options(notanoption = TRUE),
        "notanoption is not a valid quanteda option"
    )
    expect_equal(
        quanteda_options("notanoption"),
        NULL
    )
})


test_that("quanteda_options works correctly to set options", {
    quanteda_options(verbose = TRUE)
    expect_equal(
        quanteda_options("verbose"),
        getOption("quanteda_verbose")
    )
    
    quanteda_options(print_dfm_max_ndoc = 13L)
    expect_equal(
        quanteda_options("print_dfm_max_ndoc"),
        getOption("quanteda_print_dfm_max_ndoc")
    )
    
    quanteda_options(print_dfm_max_nfeat = 13L)
    expect_equal(
        quanteda_options("print_dfm_max_nfeat"),
        getOption("quanteda_print_dfm_max_nfeat")
    )
})

test_that("quanteda functions work if package is not attached (#864)", {
    skip("skipping test of option setting when quanteda is not attached")
    detach("package:quanteda", unload = TRUE)
    expect_output(
        print(quanteda::dfm(c("a b c d", "a c d e f"))),
        "Document-feature matrix of: 2 documents, 6 features"
    )
    require(quanteda)
})

test_that("quanteda_options reset works correctly", {
    quanteda_options(reset = TRUE)
    opts <- quanteda:::get_options_default()
    expect_equal(
        quanteda_options(),
        opts
    )
})

test_that("quanteda_options works with threads", {
    
    quanteda_options(threads = 2)
    Sys.setenv('RCPP_PARALLEL_NUM_THREADS' = 1)
    quanteda_options(reset = TRUE)
    expect_equal(
        quanteda_options("threads"), 1
    )
    expect_error(quanteda_options(threads = "abc"),
                 "Number of threads must be coercible to integer")
    expect_error(quanteda_options(threads = 0),
                 "Number of threads must be greater or equal to 1")
    expect_warning(quanteda_options(threads = 100),
                  "Setting threads instead to maximum available (\\d+)")
    
    options(quanteda_threads = NULL)
    expect_identical(quanteda:::get_threads(), -1L)
    
    options(quanteda_threads = 100L)
    expect_identical(quanteda:::get_threads(), 100L)
    
    options(quanteda_threads = 1L:2L)
    expect_error(
        quanteda:::get_threads(),
        "Invalid value of threads in quanteda options"
    )
    expect_error(
        quanteda:::get_threads(),
        "Invalid value of threads in quanteda options"
    )
    quanteda_options(reset = TRUE)
})
