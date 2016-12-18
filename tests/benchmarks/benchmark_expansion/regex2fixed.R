source('R/regex2fixed4.R')
source('R/regex2fixed5.R')

load("/home/kohei/Documents/Brexit/Analysis/data_corpus_guardian.RData")
toks <- tokens(data_corpus_guardian, removePunct = TRUE)
#toks <- tokens(inaugCorpus, removePunct = TRUE)

types <- attr(toks, 'types')
dict_liwc <- dictionary(file='/home/kohei/Documents/Dictionary/LIWC/LIWC2007_English.dic')

regex_liwc <- glob2rx(unlist(dict_liwc, use.names = FALSE))
microbenchmark::microbenchmark(
    regex2fixed4(regex_liwc, index(types, 'regex', case_insensitive=TRUE)),
    regex2fixed5(regex_liwc, types, 'regex', case_insensitive=TRUE),
    times=10
)

setdiff(regex2fixed5(regex_liwc, types, 'regex', case_insensitive=FALSE),
        regex2fixed4(regex_liwc, index(types, 'regex', case_insensitive=FALSE)))

dict_lex <- dictionary(file='/home/kohei/Documents/Dictionary/Lexicoder/LSDaug2015/LSD2015_NEG.lc3')
glob_lex <- tokens(unlist(dict_lex, use.names = FALSE), hash=FALSE, what='fastest')
regex_lex <- lapply(glob_lex, glob2rx)

microbenchmark::microbenchmark(
    regex2fixed4(regex_lex, index(types, 'regex', case_insensitive=TRUE)),
    regex2fixed5(regex_lex, types, 'regex', case_insensitive=TRUE),
    times=1
)


