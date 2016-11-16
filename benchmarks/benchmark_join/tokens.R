toks <- tokens(inaugCorpus)

seqs <- list(c('united', 'states'))
microbenchmark::microbenchmark(
    #joinTokens(toks, seqs, valuetype='fixed', verbose=TRUE), # too unstable
    phrasetotoken(inaugCorpus, list('united states')),
    joinTokens2(toks, seqs, valuetype='fixed', verbose=TRUE),
    times=10
)

seqs_not <- list(c('not', '*'))
microbenchmark::microbenchmark(
    joinTokens(toks, seqs_not, valuetype='glob', verbose=TRUE),
    times=10
)

seqs_will <- list(c('will', '*'))
microbenchmark::microbenchmark(
    joinTokens(toks, seqs_will, valuetype='glob', verbose=TRUE),
    times=10
)

dict_lex <- dictionary(file='/home/kohei/Documents/Dictonary/Lexicoder/LSDaug2015/LSD2015_NEG.lc3')
seq_lex <- tokens(unlist(dict_lex, use.names = FALSE), hash=FALSE, what='fastest')

# microbenchmark::microbenchmark(
#     tokens(phrasetotoken(texts(inaugCorpus), dict_lex, valuetype='glob')),
#     joinTokens(tokens(inaugCorpus), seq_lex, valuetype='glob', verbose=TRUE),
#     times=1
# )

profvis::profvis(joinTokens(tokens(inaugCorpus), seq_lex, valuetype='glob', verbose=TRUE))
profvis::profvis(joinTokens(toks, seqs_neg, valuetype='glob', verbose=FALSE))

