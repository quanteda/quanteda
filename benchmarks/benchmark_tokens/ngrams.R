library(profvis) # for profiling
library(tokenizers)
library(quanteda)

txt <- readLines('~/Documents/Brexit/Analysis/all_bbc_2015.txt') # 80MB
txt <- readLines('~/Documents/LSS/Data/Ukracine crisis/ua_3agency.txt') # 660MB

microbenchmark::microbenchmark(
    tokenizers = as.tokens(tokenize_ngrams(txt, n = 2)),
    quanteda_t1 = quanteda::ngrams(as.tokens(tokenize_words(txt)), n = 2, thread=1),
    quanteda_t4 = quanteda::ngrams(as.tokens(tokenize_words(txt)), n = 2, thread=4),
    unit='relative', times = 1
)

microbenchmark::microbenchmark(
    tokenizers = as.tokens(tokenize_skip_ngrams(txt, n = 2, k = 1)),
    quanteda_t1 = quanteda::ngrams(as.tokens(tokenize_words(txt)), n = 2, skip = 1, thread=1),
    quanteda_t4 = quanteda::ngrams(as.tokens(tokenize_words(txt)), n = 2, skip = 1, thread=4),
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

