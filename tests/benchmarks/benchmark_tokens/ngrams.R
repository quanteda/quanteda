library(profvis) # for profiling
library(tokenizers)
library(quanteda)

txt <- readLines('~/Documents/Brexit/Analysis/all_bbc_2015.txt') # 80MB
txt <- readLines('~/Documents/LSS/Data/Ukracine crisis/ua_3agency.txt') # 660MB

data(SOTUCorpus, package = "quantedaData")
txt <- texts(SOTUCorpus)

microbenchmark::microbenchmark(
    tokenizers = as.tokens(tokenize_ngrams(txt, n = 2)),
    quanteda = quanteda::tokens_ngrams(as.tokens(tokenize_words(txt)), n = 2),
    unit='relative', times = 10
)

microbenchmark::microbenchmark(
    tokenizers = as.tokens(tokenize_skip_ngrams(txt, n = 2, k = 1)),
    quanteda = quanteda::tokens_ngrams(as.tokens(tokenize_words(txt)), n = 2, skip = 1),
    unit='relative', times = 10
)

tok <- tokens(txt, removeSymbols = TRUE, removeNumbers = TRUE)

microbenchmark::microbenchmark(
    tokens_ngrams(tok, n=2),
    tokens_ngrams(tok, n=3),
    tokens_ngrams(tok, n=4),
    tokens_ngrams(tok, n=5),
    times=1
)

profvis::profvis(tokens_ngrams(tok, 2))
profvis::profvis(quanteda::tokens_ngrams(tokens(txt), n = 2))

