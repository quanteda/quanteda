library(profvis) # for profiling
library(tokenizers)

load("/home/kohei/Documents/Brexit/Analysis/data_corpus_guardian.RData")

system.time(
    tokens(data_corpus_guardian, what='word', hash = TRUE, verbose = TRUE)
)



txt <- rep(paste0(letters, collapse=' '), 10000)

microbenchmark::microbenchmark(
    tokenizers::tokenize_words(txt),
    tokens(txt, what='word', hash = TRUE),
    tokens(txt, what='word', hash = FALSE),
    unit='relative'
)

profvis(
  tokens(txt, what='fastestword')
)


