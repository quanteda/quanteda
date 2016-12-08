toks <- tokens(inaugCorpus)
dict <- dictionary(list(country = "united states", 
                        law=c('law*', 'constitution'), 
                        freedom=c('free*', 'libert*')))
dict_fix <- dictionary(list(country = "united states", 
                            law=c('law', 'constitution'), 
                            freedom=c('freedom', 'liberty'))) 

microbenchmark::microbenchmark(
    r=applyDictionary(toks, dict_fix, valuetype='fixed', verbose=FALSE),
    cpp=applyDictionary2(toks, dict_fix, valuetype='fixed', verbose=FALSE)
)

dict_liwc <- dictionary(file='/home/kohei/Documents/Dictonary/LIWC/LIWC2007_English.dic')
microbenchmark::microbenchmark(
    r=applyDictionary(toks, dict_liwc, valuetype='fixed', verbose=FALSE),
    cpp=applyDictionary2(toks, dict_liwc, valuetype='fixed', verbose=FALSE),
    times=1
)

microbenchmark::microbenchmark(
    fixed=applyDictionary2(toks, dict_liwc, valuetype='fixed', verbose=FALSE),
    glob=applyDictionary2(toks, dict_liwc, valuetype='glob', verbose=FALSE),
    times=1
)

microbenchmark::microbenchmark(
    dfm=applyDictionary(dfm(toks), dict_liwc, valuetype='glob', verbose=FALSE),
    tokens=applyDictionary2(toks, dict_liwc, valuetype='glob', verbose=FALSE),
    times=1
)

toks_short <- tokens(tokenize(inaugCorpus, what='sentence', simplify=TRUE))
microbenchmark::microbenchmark(
    r=applyDictionary(toks_short, dict, valuetype='fixed', verbose=FALSE),
    cpp=applyDictionary2(toks_short, dict, valuetype='fixed', verbose=FALSE),
    times=1
)

profvis::profvis(applyDictionary2(toks, dict_liwc[1], valuetype='glob', verbose=FALSE))

