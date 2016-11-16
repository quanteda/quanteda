toks <- tokens(inaugCorpus)

seqs <- list(c('united', 'states'))
microbenchmark::microbenchmark(
    #old=joinTokens(toks, seqs, valuetype='fixed', verbose=TRUE),
    new=joinTokens2(toks, seqs, valuetype='fixed', verbose=TRUE),
    times=10
)

seqs_not <- list(c('not', '*'))
microbenchmark::microbenchmark(
    #old=joinTokens(toks, seqs_not, valuetype='glob', verbose=TRUE),
    new=joinTokens2(toks, seqs_not, valuetype='glob', verbose=TRUE),
    times=10
)

seqs_will <- list(c('will', '*'))
microbenchmark::microbenchmark(
    #old=joinTokens(toks, seqs_will, valuetype='glob', verbose=TRUE),
    new=joinTokens2(toks, seqs_will, valuetype='glob', verbose=TRUE),
    times=10
)

profvis::profvis(joinTokens2(toks, seqs_neg, valuetype='glob', verbose=FALSE))

