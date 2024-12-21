test_that("dfm_sample works as expected", {
    mt <- dfm(tokens(data_corpus_inaugural[1:10]), verbose = FALSE)
    expect_equal(ndoc(dfm_sample(mt, size = 5)), 5)
    expect_equal(ndoc(dfm_sample(mt, size = 15, replace = TRUE)), 15)
    expect_error(dfm_sample(mt, size = 20),
                 "size cannot exceed the number of items")
    expect_error(dfm_sample(data_corpus_inaugural[1:10]),
                 "dfm_sample() only works on dfm objects.", fixed = TRUE)
})

test_that("dfm_sample default size arguments work as expected", {
    suppressWarnings(RNGversion("3.5.3"))
    dfmat <- dfm(tokens(c("a b c c d", "a a c c d d d")))
    
    mat1 <- matrix(rep(c(1, 1, 2, 1), 2), byrow = TRUE, nrow = 2,
                   dimnames = list(docs = c("text1.1", "text1.2"), features = letters[1:4]))
    mat2 <- matrix(c(1, 1, 2, 1,  0, 0, 2, 2), byrow = TRUE, nrow = 2,
                   dimnames = list(docs = c("text1", "text2"), features = c("b", "b", "c", "a")))

    expect_identical({
        set.seed(100)
        as.matrix(dfm_sample(dfmat, replace = TRUE))
    }, mat1)
})

test_that("test tokens_sample works with verbose", {
    dfmat <- dfm(tokens(data_corpus_inaugural[1:10]), verbose = FALSE)
    expect_message(
        dfm_sample(dfmat, size = 2, verbose = TRUE),
        "dfm_sample() changed from 3,366 features (10 documents) to 3,366 features (2 documents)",
        fixed = TRUE
    )
    expect_message(
        dfm_sample(dfmat, size = 5, replace = TRUE, verbose = TRUE),
        "dfm_sample() changed from 3,366 features (10 documents) to 3,366 features (5 documents)",
        fixed = TRUE
    )
})
