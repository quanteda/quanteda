test_that("test dfm_replace", {
    txt <- c(doc1 = "aa bb BB cc DD ee",
             doc2 = "aa bb cc DD ee")
    dfmt <- dfm(tokens(txt), tolower = FALSE)
    
    # case-insensitive
    expect_equal(featnames(dfm_replace(dfmt, c('aa', 'bb'), c('a', 'b'), case_insensitive = TRUE)),
                 c("a", "b", "cc", "DD", "ee"))
    
    # case-sensitive
    expect_equal(featnames(dfm_replace(dfmt, c('aa', 'bb'), c('a', 'b'), case_insensitive = FALSE)),
                 c("a", "b", "BB", "cc", "DD", "ee"))
    
    # duplicated types in from
    expect_equal(featnames(dfm_replace(dfmt, c('aa', 'aa'), c('a', 'aaa'), case_insensitive = FALSE)),
                 c("a", "bb", "BB", "cc", "DD", "ee"))
    
    # equivalent to dfm conversion method
    feat <- featnames(dfmt)
    expect_equal(dfm_replace(dfmt, feat, char_toupper(feat), case_insensitive = FALSE),
                 dfm_toupper(dfmt))
    
    # error when lenfths of from and to are different
    expect_error(dfm_replace(dfmt, c('aa', 'bb'), c('a')),
                 "The length of pattern and replacement must be the same")
    
    expect_error(dfm_replace(dfmt, c(1, 2), c(10, 20)),
                 "The type of pattern must be character")

    # does nothing when input vector is zero length
    expect_equal(dfm_replace(dfmt, character(), character()),
                 dfmt)
    
})

test_that("dfm_replace() verbose works", {
    dfmat <- dfm(tokens(c("a a b c d", "a a b c", "b c c d")))
    expect_message(
        dfm_replace(dfmat, c("a", "c"), c("X", "X"), verbose = TRUE),
        "dfm_replace() changed from 4 features (3 documents) to 3 features (3 documents)",
        fixed = TRUE
    )
})
