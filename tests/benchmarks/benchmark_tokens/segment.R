library(quanteda)
quanteda_options(threads = 7)

load("/home/kohei/Documents/Brexit/Analysis/data_corpus_guardian.RData")
corp <- data_corpus_guardian

load("/home/kohei/Documents/Brexit/Analysis/data_tokens_guardian.RData")
toks <- data_tokens_guardian


microbenchmark::microbenchmark(
    corpus = corpus_segment(corp, what = 'other', delimiter = '[\\p{P}]', valuetype = 'regex'),
    token = tokens_segment(toks, pattern = '^[\\p{P}]$', valuetype = 'regex'),
    times=1
)

profvis::profvis(tokens_segment(toks, pattern = '^[\\p{P}]$', valuetype = 'regex'))
