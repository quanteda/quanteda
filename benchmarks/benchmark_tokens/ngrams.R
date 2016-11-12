library(profvis) # for profiling
library(tokenizers)
library(ngram)
library(testthat)
library(magrittr)

txt <- rep(paste0(letters, collapse=' '), 10000)
toks <- tokens(txt, hash=FALSE)
toks_hash <- tokens(txt, hash=TRUE)

testthat("same ngrams", {
         expect_equivalent(
             tokenize_skip_ngrams(paste(letters, collapse = " "), n = 3, k = 1),
             quanteda::ngrams(tokenize(paste(letters, collapse = " ")), n = 3, skip = 0:1, concatenator = " "),
         )
})


## previously: this was not really fair, since the tokenizers package test
## started with the character, whereas we started with tokens
## -- KB

microbenchmark::microbenchmark(
    tokenizers = tokenize_skip_ngrams(txt, n = 3, k = 1),
    quanteda_hashed = tokens(txt) %>% quanteda::ngrams(n = 3, skip = 1),
    quanteda_unhashed = tokenize(txt) %>% quanteda::ngrams(n = 3, skip = 1),
    unit='relative', times = 20
)

microbenchmark::microbenchmark(
    tokenize_skip_ngrams(txt, n=3, k=0),
    quanteda::ngrams(toks_hash, n=3, skip=0),
    get.phrasetable(ngram(txt, n=3)),
    unit='relative', times = 20
)

profvis(
  quanteda::ngrams(toks_hash, n=3, skip=0)
)