require(quanteda)
require(testthat)
data(data_corpus_sotu, package = "quanteda.corpora")
corp <- as.corpus(data_corpus_sotu)
toks <- tokens(corp)
xtoks <- as.externalptr(toks)

class(toks)
class(xtoks)
expect_identical(types(toks), types(xtoks))

# deep copy the xtokens
xtoks_copy <- as.externalptr(xtoks)
expect_identical(as.tokens(xtoks_copy), as.tokens(xtoks))

# operations on copied xtokens do not affect the original xtokens
xtoks_copy <- tokens_remove(xtoks_copy, stopwords(), padding = TRUE)
expect_false(identical(as.list(xtoks_copy), as.list(as.tokens(xtoks))))
expect_identical(as.list(toks), as.list(xtoks))

# tokens_select modify tokens and xtokens in the same way
toks2 <- tokens_remove(toks, stopwords(), padding = TRUE) %>% 
    tokens_select(data_dictionary_LSD2015, padding = TRUE)
xtoks2 <- tokens_remove(xtoks, stopwords(), padding = TRUE) %>% 
    tokens_select(data_dictionary_LSD2015, padding = TRUE)
expect_identical(as.list(toks2), as.list(as.tokens(xtoks2)))

