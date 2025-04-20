test_that("msg works", {
    
    expect_equal(
        quanteda:::msg("there are %s features", 10000),
        "there are 10,000 features"
    )
    expect_equal(
        quanteda:::msg("there are %s features", 10000, pretty = FALSE),
        "there are 10000 features"
    )
})
