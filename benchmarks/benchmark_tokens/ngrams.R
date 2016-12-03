library(profvis) # for profiling
library(tokenizers)
library(testthat)
library(magrittr)

txt <- readLines('~/Documents/Brexit/Analysis/all_bbc_2015.txt') # 80MB

microbenchmark::microbenchmark(
    tokenizers = tokenize_ngrams(txt, n = 2),
    quanteda_t1 = as.tokens(tokenize_words(txt)) %>% quanteda::ngrams(n = 2, thread=1),
    quanteda_t4 = as.tokens(tokenize_words(txt)) %>% quanteda::ngrams(n = 2, thread=4),
    unit='relative', times = 1
)

microbenchmark::microbenchmark(
    tokenizers = tokenize_skip_ngrams(txt, n = 2, k = 1),
    quanteda_t1 = as.tokens(tokenize_words(txt)) %>% quanteda::ngrams(n = 2, skip = 1, thread=1),
    quanteda_t4 = as.tokens(tokenize_words(txt)) %>% quanteda::ngrams(n = 2, skip = 1, thread=4),
    unit='relative', times = 1
)

tok <- tokens(txt, removeSymbols = TRUE, removeNumbers = TRUE)

microbenchmark::microbenchmark(
    ngrams(tok, n=2, thread=1),
    ngrams(tok, n=2, thread=2),
    ngrams(tok, n=2, thread=3),
    ngrams(tok, n=2, thread=4),
    ngrams(tok, n=2, thread=8),
    times=1
)

profvis::profvis(ngrams(tok, 2, thread=4))
profvis::profvis(quanteda::ngrams(tokens(txt), n = 2, thread=4))


# Make dfm

sum(lengths(tok))
length(attr(tok, 'types'))
mx <- dfm(tok)

ngm <- ngrams(tok, 2, thread=4)
sum(lengths(ngm))
length(attr(ngm, 'types'))
mx_ngm <- dfm(ngm)
