context("dfm_sample")

test_that("dfm_sample works as expected", {
    mt <- dfm(data_corpus_inaugural[1:10], verbose = FALSE)
    expect_equal(ndoc(dfm_sample(mt, margin = "documents", size = 5)), 5)
    expect_equal(ndoc(dfm_sample(mt, margin = "documents", size = 15, replace = TRUE)), 15)
    expect_error(dfm_sample(mt, margin = "documents", size = 20),
                 "size cannot exceed the number of documents \\(10\\)")
    expect_error(dfm_sample(mt, margin = "features", size = 3500),
                 "size cannot exceed the number of features \\(33\\d{2}\\)")
    expect_error(dfm_sample(data_corpus_inaugural[1:10]),
                 "only works on dfm objects")
})

test_that("dfm_sample for features works as expected", {
    RNGversion("3.5.3")
    dfmat <- dfm(c("a b c c d", "a a c c d d d"))
    expect_identical({
        set.seed(100)
        as.matrix(dfm_sample(dfmat, size = 2, margin = "features", replace = TRUE))
    },
    matrix(c(1, 0, 1, 0), nrow = 2, dimnames = list(docs = c("text1", "text2"), features = c("b", "b")))
    )
    expect_identical({
        set.seed(100)
        as.matrix(dfm_sample(dfmat, size = 3, margin = "features", replace = FALSE))
    },
    matrix(c(1, 0, 1, 2, 1, 3), nrow = 2, dimnames = list(docs = c("text1", "text2"), features = c("b", "a", "d")))
    )
})

test_that("dfm_sample default size arguments work as expected", {
    dfmat <- dfm(c("a b c c d", "a a c c d d d"))
    expect_identical({
        set.seed(100)
        as.matrix(dfm_sample(dfmat, margin = "documents", replace = TRUE))
    },
    matrix(rep(c(1, 1, 2, 1), 2), byrow = TRUE, nrow = 2,
           dimnames = list(docs = c("text1", "text1"), features = letters[1:4]))
    )
    expect_identical({
        set.seed(100)
        as.matrix(dfm_sample(dfmat, margin = "features", replace = TRUE))
    },
    matrix(c(1, 1, 2, 1,  0, 0, 2, 2), byrow = TRUE, nrow = 2,
           dimnames = list(docs = c("text1", "text2"), features = c("b", "b", "c", "a")))
    )
})
