test_that("msg works", {
    
    expect_equal(
        quanteda:::msg("there are %s features", 10000),
        "there are 10,000 features"
    )
    expect_equal(
        quanteda:::msg("there are %s features", 10000, 
                       prepend = "[ ", append = " ]"),
        "[ there are 10,000 features ]"
    )
})

test_that("object stats are correct", {
    
    corp <- data_corpus_inaugural[1:5]
    toks <- tokens(corp, xptr = TRUE) %>% 
        tokens_tolower() %>% 
        tokens_trim(max_n = 1000)
    dfmt <- dfm(toks)
    
    expect_identical(quanteda:::stats_corpus(corp),
                     list(ndoc = 5L, 
                          nchar = sum(nchar(corp)))
                     )
    expect_identical(quanteda:::stats_tokens(toks),
                     list(ndoc = 5L, 
                          ntoken = 7616L,
                          ntype = 1000L)
                     )
    expect_identical(quanteda:::stats_dfm(dfmt),
                     list(ndoc = 5L, 
                          nfeat = 1000L)
                    )
})
