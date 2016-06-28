context('test collocations.R')

row_in_df <- function(df, row) {
    nrow(merge(df, row)) > 0
}

test_that("test that collocations do not span texts by default", {
    actual_collocations <- data.frame(
            collocations(
                tokenize(c('this is a test', 'this is also a test'))
            )
        )
    expect_false(
        row_in_df(
            actual_collocations,
            data.frame(word1='test', word2='this')
        )
    )
})

test_that("test that collocations do not span punctuation by default", {
    actual_collocations <- data.frame(
        collocations(
        tokenize(c('this is a test.', 'this is also a test'))
        )
    )
    expect_false(
        row_in_df(
            actual_collocations,
            data.frame(word1='test', word2='this')
        )
    )
})

test_that("test that collocations which span punctuation do not include punctuation", {

    actual_collocations <- data.frame(
        collocations(
            tokenize(c('This is a test. This is also a test')), spanPunct=T
        )
    )
    expect_false('.' %in% actual_collocations$word1)
    expect_false('.' %in% actual_collocations$word2)
})
