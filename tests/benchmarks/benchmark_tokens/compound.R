toks <- tokens(inaugCorpus)
load("/home/kohei/Documents/Brexit/Analysis/data_corpus_guardian.RData")
toks <- tokens(data_corpus_guardian)

seqs <- list(c('united', 'states'))
microbenchmark::microbenchmark(
    tokens_compound(toks, seqs, valuetype='fixed'),
    times=1
)

seqs_not <- list(c('not', '*'))
microbenchmark::microbenchmark(
    tokens_compound(toks, seqs_not, valuetype='glob'),
    times=1
)

seqs_will <- list(c('will', '*'))
microbenchmark::microbenchmark(
    tokens_compound(toks, seqs_will, valuetype='glob'),
    times=1
)

dict_lex <- dictionary(file='/home/kohei/Documents/Dictionary/Lexicoder/LSDaug2015/LSD2015_NEG.lc3')
seqs_lex <- tokens(unlist(dict_lex, use.names = FALSE), hash=FALSE, what='fastest')

profvis::profvis(tokens_compound(tokens(inaugCorpus), seq_lex, valuetype='glob', verbose=TRUE))
profvis::profvis(tokens_compound(toks, seqs_lex, valuetype='glob', verbose=FALSE))

