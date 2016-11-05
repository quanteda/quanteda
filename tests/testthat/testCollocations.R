context('test collocations.R')

row_in_df <- function(df, row) {
    nrow(merge(df, row)) > 0
}

test_that("test that collocations do not span texts", {
    actual_collocations <- data.frame(
            collocations(
                tokenize(c('this is a test', 'this also a test'))
            )
        )
    expect_false(
        row_in_df(
            actual_collocations,
            data.frame(word1='test', word2='this')
        )
    )
})


test_that("test that collocations span punctuation, but not texts, if punctuation = 'span'", {
    actual_collocations <- data.frame(
        collocations(
            tokenize(c('This is a test.', 'This also, a test.')), punctuation = "ignore"
        )
    )
    #  Never span texts
    expect_false(
        row_in_df(
            actual_collocations,
            data.frame(word1='test', word2='this')
        )
    )
    #  But do span punctuation within tests
    expect_true(
        row_in_df(
            actual_collocations,
            data.frame(word1='also', word2='a')
        )
    )
})


test_that("test that collocations includes punctuation is punctuation = 'include'", {
    actual_collocations <- data.frame(
        collocations(
            tokenize(c('This is a test.', 'This also, a test.')), punctuation = "include"
        )
    )
    #  Never span texts
    expect_false(
        row_in_df(
            actual_collocations,
            data.frame(word1='test', word2='this')
        )
    )
    #  Never span texts, even including punctuation
    expect_false(
        row_in_df(
            actual_collocations,
            data.frame(word1='.', word2='this')
        )
    )
    expect_true(
        row_in_df(
            actual_collocations,
            data.frame(word1='test', word2='.')
        )
    )
        expect_true(
        row_in_df(
            actual_collocations,
            data.frame(word1='also', word2=',')
        )
    )
    expect_false(
        row_in_df(
            actual_collocations,
            data.frame(word1='also', word2='a')
        )
    )
})


test_that("test that collocations span punctuation, but not texts, if punctuation is removed.", {
    actual_collocations <- data.frame(
        collocations(
            tokenize(c('This is a test.', 'This also, a test.'), removePunct = TRUE), punctuation = "ignore"
        )
    )
    #  Never span texts
    expect_false(
        row_in_df(
            actual_collocations,
            data.frame(word1='test', word2='this')
        )
    )
    expect_true(
        row_in_df(
            actual_collocations,
            data.frame(word1='also', word2='a')
        )
    )
    expect_false('.' %in% actual_collocations$word1)
    expect_false('.' %in% actual_collocations$word2)
    expect_false(',' %in% actual_collocations$word1)
    expect_false(',' %in% actual_collocations$word2)

})
