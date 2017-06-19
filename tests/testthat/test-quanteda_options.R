context("test quanteda_options")


test_that("quanteda_options initialize works correctly", {
    quanteda_options(verbose = TRUE, threads = 1)
    quanteda_options(initialize = TRUE)
    expect_equal(quanteda_options("threads"), 1)
    expect_equal(quanteda_options("verbose"), TRUE)
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
    
    quanteda_options(print_dfm_max_ndoc = 13L)
    expect_equal(
        quanteda_options("print_dfm_max_ndoc"),
        getOption("quanteda_print_dfm_max_ndoc")
    )
    
    quanteda_options(print_dfm_max_nfeature = 13L)
    expect_equal(
        quanteda_options("print_dfm_max_nfeature"),
        getOption("quanteda_print_dfm_max_nfeature")
    )
})

test_that("quanteda functions work if package is not attached", {
    skip_on_travis()
    skip_on_appveyor()
    skip_on_cran()
    detach("package:quanteda", unload = TRUE)
    expect_output(
        print(quanteda::dfm(c("a b c d", "a c d e f"))),
        "Document-feature matrix of: 2 documents, 6 features"
    )
    require(quanteda)
})

test_that("quanteda_options reset works correctly", {
    quanteda_options(reset = TRUE)
    qopts <- quanteda:::QUANTEDA_OPTION_LIST
    names(qopts) <- stringi::stri_replace_all_fixed(names(qopts), "quanteda_", "")
    expect_equal(
        quanteda_options(),
        qopts        
    )
})
