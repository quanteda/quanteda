library(quanteda)

load("/home/kohei/Documents/Brexit/Analysis/data_corpus_guardian.RData")
docvars(data_corpus_guardian, 'year') <- format(docvars(data_corpus_guardian, 'date'), '%Y')

tok <- tokens(data_corpus_guardian)
#tok <- tokens(corpus_subset(data_corpus_guardian, year == 2012))
tok[[1]]

tok2 <- tokens_select(tok, stopwords())
tok2[[1]]

tok3 <- tokens_select(tok, stopwords(), padding = TRUE)
tok3[[1]]

tok4 <- tokens_remove(tok, stopwords(), padding = TRUE)
tok4[[1]]

system.time(tokens_select(tok, '*'))
system.time(tokens_select(tok, list(c('President', '*'))))
system.time(tokens_remove(tok, list(c('President', '*'))))

microbenchmark::microbenchmark(
    dfm(tokens_remove(tok, stopwords(), valuetype='fixed')),
    dfm_remove(dfm(tok), stopwords(), valuetype='fixed'),
    times=1
)