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
        tokens_trim(max_n = 1000, padding = TRUE)
    dfmt <- dfm(toks)
    fcmt <- fcm(dfmt)
    
    expect_equal(
        quanteda:::stats_corpus(corp),
        list(ndoc = 5L, 
             nchar = sum(nchar(corp)),
             ndocvar = 4L)
    )
    expect_equal(
        quanteda:::stats_tokens(toks),
        list(ndoc = 5L, 
             ntoken = sum(ntoken(toks)),
             ntype = 1000L,
             ndocvar = 4L)
    )
    expect_equal(
        quanteda:::stats_dfm(dfmt),
        list(ndoc = 5L, 
             nfeat = 1000L,
             nocc = sum(dfm_remove(dfmt, "")),
             spar = sparsity(dfmt),
             ndocvar = 4L),
        tolerance = 0.001
    )
    expect_equal(
        quanteda:::stats_fcm(fcmt),
        list(nrow = 1000L, 
             ncol = 1000L,
             nocc = sum(fcm_remove(fcmt, "")),
             spar = sparsity(fcmt)),
        tolerance = 0.001
    )
})

test_that("verbose messages report object stats in the correct order", {
    toks <- tokens(c(d1 = "a b c a", d2 = "b d d d"))
    expect_message(
        tokens_remove(toks, "c", verbose = TRUE),
        "Returning tokens of 2 documents (7 tokens, 3 types) from tokens_remove()",
        fixed = TRUE
    )
})
