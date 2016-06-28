context('test collocations.R')

row_in_df <- function(df, row) {
    nrow(merge(df, row)) > 0
}

test_that("test that collocations do not span texts by default", {
    actual_collocations <- data.frame(
            collocations(
                tokenize(c('this is a test', 'this: also a test'))
            )
        )
    expect_false(
        row_in_df(
            actual_collocations,
            data.frame(word1='test', word2='this')
        )
    )
    expect_false(
        row_in_df(
            actual_collocations,
            data.frame(word1='this', word2='also')
        )
    )
})

test_that("test that collocations span punctuation, but not texts, if spanPunct = TRUE", {
    actual_collocations <- data.frame(
        collocations(
            tokenize(c('This is a test.', 'This: also a test.')), spanPunct = TRUE
        )
    )
    expect_false(
        row_in_df(
            actual_collocations,
            data.frame(word1='test', word2='this')
        )
    )
    expect_false(
        row_in_df(
            actual_collocations,
            data.frame(word1='this', word2='also')
        )
    )
})


test_that("test that collocations do not span punctuation by default", {
    actual_collocations <- data.frame(
        collocations(
            tokenize(c('This is a test.', 'This: also a test.'))
        )
    )
    expect_false(
        row_in_df(
            actual_collocations,
            data.frame(word1='test', word2='this')
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

    (actual_collocations <- data.frame(
        collocations(
            tokenize(c('This is a test. This: also a test.')), spanPunct = TRUE
        )
    ))
    expect_false('.' %in% actual_collocations$word1)
    expect_false('.' %in% actual_collocations$word2)
})


char <- "This, a test."

collocations(char)
collocations(char, spanPunct = FALSE)
collocations(char, spanPunct = TRUE)
collocations(char, spanPunct = FALSE, removePunct = TRUE)
collocations(char, spanPunct = TRUE, removePunct = TRUE)

collocations(tokenize(char))
collocations(tokenize(char), spanPunct = FALSE)
collocations(tokenize(char), spanPunct = TRUE)
collocations(tokenize(char, removePunct = TRUE), spanPunct = FALSE)
collocations(tokenize(char, removePunct = TRUE), spanPunct = TRUE)



