library(profvis) # for profiling
library(tokenizers)


txt <- rep(paste0(letters, collapse=' '), 10000)

microbenchmark::microbenchmark(
  tokenize_words(txt),
  tokens(txt, what='fastestword'),
  tokens(txt, what='fastestword', hash=FALSE),
  unit='relative'
)

profvis(
  tokens(txt, what='fastestword')
)
