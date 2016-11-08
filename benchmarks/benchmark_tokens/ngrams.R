library(profvis) # for profiling
library(tokenizers)
library(ngram)


txt <- rep(paste0(letters, collapse=' '), 10000)
toks <- tokens(txt, hash=FALSE)
toks_hash <- tokens(txt, hash=TRUE)

microbenchmark::microbenchmark(
  tokenize_skip_ngrams(txt, n=3, k=1),
  quanteda::ngrams(toks_hash, n=3, k=1),
  unit='relative'
)

microbenchmark::microbenchmark(
  tokenize_skip_ngrams(txt, n=3, k=0),
  quanteda::ngrams(toks_hash, n=3, k=0),
  get.phrasetable(ngram(txt, n=3)),
  unit='relative'
)

profvis(
  quanteda::ngrams(toks_hash, n=3, k=0)
)