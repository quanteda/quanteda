test_that("texts produces a defunct message", {
    expect_error(
        texts(data_corpus_inaugural),
        "`texts\\(\\)` was deprecated in quanteda 3.0 and is now defunct."
    )
    expect_error(
        texts(data_corpus_inaugural, groups = Party),
        "`texts\\(x, groups = \\.\\.\\.\\)` was deprecated"
    )
})
