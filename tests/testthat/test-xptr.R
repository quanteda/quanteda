require(quanteda)
require(testthat)

toks <- tokens(data_corpus_inaugural)
xtoks <- as.tokens_xptr(toks)

test_that("attributes are the same", {
    expect_identical(attr(toks, "docvars"), attr(xtoks, "docvars"))
    expect_identical(attr(toks, "meta"), attr(xtoks, "meta"))
})

test_that("subsetting work", {
    expect_identical(docnames(as.tokens_xptr(toks)[2:6]),
                     docnames(toks)[2:6])
    expect_identical(docnames(as.tokens_xptr(toks)[2:6 * -1]),
                     docnames(toks)[2:6 * -1])
    
    expect_identical(docnames(head(as.tokens_xptr(toks))), 
                     docnames(head(toks)))
    expect_identical(docnames(head(as.tokens_xptr(toks), 0)), 
                     docnames(head(toks, 0)))
    expect_identical(docnames(head(as.tokens_xptr(toks), 100)), 
                     docnames(head(toks, 100)))
    expect_identical(docnames(tail(as.tokens_xptr(toks))), 
                     docnames(tail(toks)))
    expect_identical(docnames(tail(as.tokens_xptr(toks), -10)), 
                     docnames(tail(toks, -10)))
})

test_that("R-like functions work", {
    expect_identical(ndoc(toks), ndoc(xtoks))
    expect_identical(ntoken(toks), ntoken(xtoks))
    expect_identical(types(toks), types(xtoks))
})

test_that("deep copy xtokens", {
    xtoks_copy <- as.tokens_xptr(xtoks)
    expect_identical(as.tokens(xtoks_copy), 
                     as.tokens(xtoks))
})

test_that("operations on copied xtokens do not affect the original xtokens", {
    expect_identical(
        as.tokens(tokens_subset(as.tokens_xptr(toks), Party == "Republican")),
        tokens_subset(toks, Party == "Republican")
    )
    expect_identical(
        as.tokens(tokens_select(as.tokens_xptr(toks), stopwords("en"))),
        tokens_select(toks, stopwords("en"))
    )
    expect_identical(
        as.list(as.tokens(tokens_ngrams(as.tokens_xptr(toks)))),
        as.list(tokens_ngrams(toks))
    )
})

test_that("operations on copied xtokens do not affect the original xtokens", {
    xtoks_copy <- as.tokens_xptr(xtoks)
    xtoks_copy <- tokens_remove(xtoks_copy, stopwords(), padding = TRUE)
    expect_false(identical(as.list(xtoks_copy), 
                           as.list(xtoks)))
    expect_identical(as.list(toks), 
                     as.list(xtoks))
})

test_that("tokens_select modify tokens and xtokens in the same way", {
    
    toks2 <- tokens_remove(toks, stopwords(), padding = TRUE) %>% 
        tokens_select(data_dictionary_LSD2015, padding = TRUE)
    xtoks2 <- as.tokens_xptr(xtoks) %>% 
        tokens_remove(stopwords(), padding = TRUE) %>% 
        tokens_select(data_dictionary_LSD2015, padding = TRUE)
    expect_identical(as.list(toks2), as.list(xtoks2))
    
})
