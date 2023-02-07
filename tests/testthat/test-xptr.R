require(quanteda)
require(testthat)

toks <- tokens(data_corpus_inaugural)
xtoks <- as.tokens_xptr(toks)
xtoks_copy <- as.tokens_xptr(xtoks)
head(as.tokens_xptr(toks))


expect_identical(
    as.tokens(tokens_subset(as.tokens_xptr(toks), Party == "Republican")),
    tokens_subset(toks, Party == "Republican")
)

# native support
expect_identical(ndoc(toks), ndoc(xtoks))
expect_identical(ntoken(toks), ntoken(xtoks))
expect_identical(types(toks), types(xtoks))

tokens_subset(xtoks, )

as.tokens_xptr(toks)

ndoc(as.tokens_xptr(toks)[2:6])
ndoc(as.tokens_xptr(toks)[10])

# compatibility

test_that("attributes are the same", {
    expect_identical(attr(toks, "docvars"), attr(xtoks, "docvars"))
    expect_identical(attr(toks, "meta"), attr(xtoks, "meta"))
})

test_that("deep copy xtokens", {
    expect_identical(as.tokens(xtoks_copy), 
                     as.tokens(xtoks))
})

## does not work
head(xtoks)
tail(xtoks)

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
    xtoks2 <- as.tokens_xptr(xtoks) %>% 
        tokens_remove(stopwords(), padding = TRUE) %>% 
        tokens_select(data_dictionary_LSD2015, padding = TRUE)
    expect_identical(as.list(toks2), as.list(xtoks2))
    
})
