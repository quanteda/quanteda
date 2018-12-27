library(profvis) # for profiling
library(tokenizers)

corp <- readRDS("/home/kohei/Documents/Brexit/Data/data_corpus_guardian.RDS")

system.time(
    tokens(corp, what = 'word', verbose = TRUE)
)

system.time(
    tokens(corp, what = 'fastestword', verbose = TRUE)
)

txt <- rep(paste0(letters, collapse=' '), 10000)

microbenchmark::microbenchmark(
    tokenizers::tokenize_words(txt),
    tokens(txt, what = 'word'),
    unit = 'relative'
)

profvis(
  tokens(txt, what = 'fastestword')
)


