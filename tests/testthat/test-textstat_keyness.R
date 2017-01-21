
test_that("keyness_textstat chi2 computation is correct", {
    mydfm <- dfm(c(d1 = "b b b b b b b a a a",
                   d2 = "a a a a a a a b b"))
    suppressWarnings(
        result <- stats::chisq.test(as.matrix(mydfm), correct = TRUE)
    )
    expect_equivalent(
        result$statistic,
        textstat_keyness(mydfm, sort = FALSE)[1]
    )
})

test_that("keyness_chi2 internal methods are equivalent", {
    skip("Skipped because stats::chisq.test is wrong for small-value 2x2 tables")
    mydfm <- dfm(c(d1 = "a a a b b c c c c c c d e f g h h",
                   d2 = "a a b c c d d d d e f h"))
    expect_equal(
        quanteda:::keyness_chi2_stats(mydfm),
        quanteda:::keyness_chi2_dt(mydfm)
    )
    
    ## stats::chisq.test is wrong for small tables
    mat <- matrix(c(3, 2, 14, 10), ncol = 2)
    chi <- stats::chisq.test(mat)
    ## Warning message:
    ## In stats::chisq.test(mat) : Chi-squared approximation may be incorrect
    
    # from the function
    chi$statistic
    ##    X-squared 
    ## 1.626059e-31 
    
    # as it should be (with Yates correction)
    sum((abs(chi$observed - chi$expected) - 0.5)^2 / chi$expected)
    ## [1] 0.1851001
})

test_that("basic textstat_keyness works on two rows", {
    mydfm <- dfm(c(d1 = "a a a b b c c c c c c d e f g h h",
                   d2 = "a a b c c d d d d e f h"))
    expect_equal(names(textstat_keyness(mydfm)),
                 c("c", "a", "b", "h", "g", "e", "f", "d"))
    expect_equal(names(textstat_keyness(mydfm, target = 2)),
                 c("d", "e", "f", "g", "b", "h", "a", "c"))
})

test_that("textstat_keyness works with different targets", {
    mydfm <- dfm(c(d1 = "a a a b b c c c c c c d e f g h h",
                   d2 = "a a b c c d d d d e f h"))
    expect_equal(textstat_keyness(mydfm),
                 textstat_keyness(mydfm, target = 1))    
    expect_equal(textstat_keyness(mydfm, target = "d1"),
                 textstat_keyness(mydfm, target = 1))    
    expect_equal(textstat_keyness(mydfm, target = "d2"),
                 textstat_keyness(mydfm, target = 2))    
})

test_that("textstat_keyness works with different targets", {
    mydfm <- dfm(c(d1 = "a a a b b c c c c c c d e f g h h",
                   d2 = "a a b c c d d d d e f h", 
                   d3 = "a a a a b b c c d d d d d d"))
    expect_equal(textstat_keyness(mydfm, 3),
                 textstat_keyness(mydfm, target = "d3"))    
})

test_that("textstat_keyness combines non-target rows correctly", {
    mydfm <- dfm(c(d1 = "a a a b b c c c c c c d e f g h h",
                   d2 = "a a b c c d d d d e f h", 
                   d3 = "a a a a b b c c d d d d d d"))
    expect_equal(textstat_keyness(mydfm, 1),
                 textstat_keyness(rbind(mydfm[1, ], new("dfmSparse", mydfm[2, ] + mydfm[3, ])), target = "d1"))    
})


test_that("textstat_keyness errors", {
    mydfm <- dfm(c(d1 = "a a a b b c c c c c c d e f g h h",
                   d2 = "a a b c c d d d d e f h"))
    expect_error(textstat_keyness(mydfm, target = 3),
                 "target index outside range of documents")
    expect_error(textstat_keyness(mydfm, target = "d3"),
                 "target not found in docnames\\(x\\)")
    expect_error(textstat_keyness(mydfm[1, ]),
                 "x must have at least two documents")
})



test_that("keyness_textstat exact computation is correct", {
    mydfm <- dfm(c(d1 = "b b b b b b b a a a",
                   d2 = "a a a a a a a b b"))
    result <- stats::fisher.test(as.matrix(mydfm))
    expect_equivalent(
        result$estimate,
        textstat_keyness(mydfm, measure = "exact", sort = FALSE)[1]
    )
})

test_that("basic textstat_keyness exact works on two rows", {
    mydfm <- dfm(c(d1 = "a a a b b c c c c c c d e f g h h",
                   d2 = "a a b c c d d d d e f h"))
    expect_equal(names(textstat_keyness(mydfm, measure = "exact")),
                 c("g", "c", "b", "h", "a", "e", "f", "d"))
    expect_equal(names(textstat_keyness(mydfm, target = 2, measure = "exact")),
                 c("d", "e", "f", "a", "b", "h", "c", "g"))
})