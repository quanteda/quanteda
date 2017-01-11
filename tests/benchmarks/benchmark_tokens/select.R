library(quanteda)

load("/home/kohei/Documents/Brexit/Analysis/data_corpus_guardian.RData")
docvars(toks, 'year') <- format(docvars(data_corpus_guardian, 'date'), '%Y')

toks <- tokens(data_corpus_guardian, verbose=TRUE)
#toks <- tokens(corpus_subset(data_corpus_guardian, year == 2012))
toks[[1]]

toks2 <- tokens_select(toks, stopwords(), valuetype='fixed')
toks2[[1]]

toks3 <- tokens_select(toks, stopwords(), valuetype='fixed', padding = TRUE)
toks3[[1]]

system.time(tokens_select(toks, '*'))
system.time(tokens_select(toks, list(c('President', '*'))))
system.time(tokens_remove(toks, list(c('President', '*'))))

microbenchmark::microbenchmark(
    dfm(tokens_remove(toks, stopwords(), valuetype='fixed')),
    dfm_remove(dfm(toks), stopwords(), valuetype='fixed'),
    times=1
)

profvis::profvis(tokens_select(toks, stopwords(), valuetype='fixed'))
profvis::profvis(tokens_select(toks, stopwords(), valuetype='glob'))
