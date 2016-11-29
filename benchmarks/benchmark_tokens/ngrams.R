library(profvis) # for profiling
library(tokenizers)
library(testthat)
library(magrittr)

txt <- readLines('~/Documents/Brexit/Analysis/all_bbc_2015.txt') # 80MB
tok <- tokens(txt, removeSymbols = TRUE, removeNumbers = TRUE)

microbenchmark::microbenchmark(
    tokenizers = tokenize_skip_ngrams(txt, n = 2, k = 1),
    quanteda_unhashed = tokenize(txt, what='fastestword') %>% quanteda::ngrams(n = 2, skip = 1),
    quanteda_hashed = tokens(txt, what='fastestword') %>% quanteda::ngrams(n = 2, skip = 1),
    quanteda_hashed_mt = tokens(txt, what='fastestword') %>% quanteda::ngrams(n = 2, skip = 1, thread=4),
    unit='relative', times = 1
)


microbenchmark::microbenchmark(
    ngrams(tok_all, n=2, thread=1),
    ngrams(tok_all, n=2, thread=2),
    ngrams(tok_all, n=2, thread=3),
    ngrams(tok_all, n=2, thread=4),
    times=1
)


profvis::profvis(ngrams(tok_all, 2))