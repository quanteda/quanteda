library(quanteda)
quanteda_options(threads = 8)

#load("/home/kohei/Documents/Brexit/Analysis/data_corpus_guardian.RData")
#toks <- tokens(data_corpus_guardian, verbose=TRUE)

corp <- readRDS("/home/kohei/Documents/Brexit/Data/data_corpus_guardian.RDS")
toks <- tokens(corp)

profvis::profvis(tokens_sample(toks, 1000))
