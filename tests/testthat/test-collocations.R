context('test collocations.R')

row_in_df <- function(df, row) {
    nrow(merge(df, row)) > 0
}

test_that("test that collocations do not span texts", {
    actual_collocations <- data.frame(
            collocations(
                tokenize(c('this is a test', 'this also a test')), 
                size =2:3
            )
        )
    expect_false(
        row_in_df(
            actual_collocations,
            data.frame(word1='test', word2='this')
        )
    )
    
    # test the trigram collocations 
    expect_false(
        row_in_df(
            actual_collocations,
            data.frame(word1='test', word2='this', word3='also')
        )
    )
    
    expect_true(
        row_in_df(
            actual_collocations,
            data.frame(word1='this', word2='also', word3='a')
        )
    )
})


test_that("test that collocations span punctuation, but not texts, if punctuation = 'span'", {
    actual_collocations <- data.frame(
        collocations(
            tokenize(c('This is a test.', 'This also, a test.')), punctuation = "ignore", size = 2:3
        )
    )
    #  Never span texts
    expect_false(
        row_in_df(
            actual_collocations,
            data.frame(word1='test', word2='this')
        )
    )
    #  But do span punctuation within texts
    expect_true(
        row_in_df(
            actual_collocations,
            data.frame(word1='also', word2='a')
        )
    )
    
    # test the trigram collocations 
    #  Never span texts
    expect_false(
        row_in_df(
            actual_collocations,
            data.frame(word1='test', word2='this', word3='also')
        )
    )
    #  But do span punctuation within texts
    expect_true(
        row_in_df(
            actual_collocations,
            data.frame(word1='also', word2='a', word3 = 'test')
        )
    )
})


test_that("test that collocations includes punctuation if punctuation = 'include'", {
    actual_collocations <- data.frame(
        collocations(
            tokenize(c('This is a test.', 'This also, a test.')), punctuation = "include", size=2:3
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
    
    # test the trigram collocations 
    expect_false(
        row_in_df(
            actual_collocations,
            data.frame(word1='test', word2='.', word3='this')
        )
    )
    expect_true(
        row_in_df(
            actual_collocations,
            data.frame(word1='a', word2='test', word3='.')
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

test_that("test the correctness of the association measure for collocations, aganist proxy::dist.", {
    skip_if_not_installed("proxy")
    toks <- tokenize(c('this is a test', 'this also a test'))
    collo <- collocations(toks, method = "dice")
    
    proxy_dice <- as.matrix(proxy::simil(as.matrix(dfm(toks)), "dice", by_rows = F))
    collo[,pdice := proxy_dice[word1,word2], by = word1]
    expect_equivalent(collo$dice, collo$pdice)
})
