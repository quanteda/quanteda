test_that("copy works on other objects", {
    expect_identical(
        data_corpus_inaugural,
        copy(data_corpus_inaugural)
    )
    expect_identical(
        tmp <- c(TRUE, FALSE, TRUE, FALSE),
        copy(tmp)
    )
})

test_that("deep copy xtokens", {
    xtoks <- tokens(data_corpus_inaugural[1:3], xptr = TRUE)
    expect_identical(
        as.tokens(as.tokens_xptr(xtoks)),
        as.tokens(copy(xtoks))
    )
})
