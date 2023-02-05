require(quanteda)

toks <- tokens(data_corpus_inaugural)
xtoks <- as.externalptr(toks)
xtoks_copy <- as.externalptr(xtoks)

test_that("attributes are the same", {
    expect_identical(types(toks), types(xtoks))
    expect_identical(attr(toks, "docvars"), attr(xtoks, "docvars"))
    expect_identical(attr(toks, "meta"), attr(xtoks, "meta"))
})

test_that("deep copy xtokens", {
    expect_identical(as.tokens(xtoks_copy), 
                     as.tokens(xtoks))
})

test_that("operations on copied xtokens do not affect the original xtokens", {
    xtoks_copy <- tokens_remove(xtoks_copy, stopwords(), padding = TRUE)
    expect_false(identical(as.list(xtoks_copy), 
                           as.list(xtoks)))
    expect_identical(as.list(toks), 
                     as.list(xtoks))
})

test_that("tokens_select modify tokens and xtokens in the same way", {
    
    toks2 <- tokens_remove(toks, stopwords(), padding = TRUE) %>% 
        tokens_select(data_dictionary_LSD2015, padding = TRUE)
    xtoks2 <- as.externalptr(xtoks) %>% 
        tokens_remove(stopwords(), padding = TRUE) %>% 
        tokens_select(data_dictionary_LSD2015, padding = TRUE)
    expect_identical(as.list(toks2), as.list(xtoks2))
    
})
