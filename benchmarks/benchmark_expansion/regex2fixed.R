source('R/regex2fixed.R')
source('R/regex2fixed2.R')
source('R/regex2fixed3.R')
source('R/regex2fixed4.R')

toks <- tokens(inaugCorpus, removePunct = TRUE)
types <- attr(toks, 'types')

dict_liwc <- dictionary(file='/home/kohei/Documents/Dictonary/LIWC/LIWC2007_English.dic')

regex_liwc <- glob2rx(unlist(dict_liwc, use.names = FALSE))
microbenchmark::microbenchmark(
    regex2fixed(regex_liwc, types, 'regex', case_insensitive=TRUE),
    regex2fixed2(regex_liwc, types, 'regex', case_insensitive=TRUE),
    regex2fixed3(regex_liwc, types, 'regex', case_insensitive=TRUE),
    regex2fixed4(regex_liwc, index(types, 'regex', case_insensitive=TRUE)),
    times=1
)


setdiff(regex2fixed(regex_liwc, types, 'regex', case_insensitive=TRUE),
        regex2fixed2(regex_liwc, types, 'regex', case_insensitive=TRUE))

setdiff(regex2fixed(regex_liwc, types, 'regex', case_insensitive=FALSE),
        regex2fixed2(regex_liwc, types, 'regex', case_insensitive=FALSE))

setdiff(regex2fixed(regex_liwc, types, 'regex', case_insensitive=TRUE),
        regex2fixed3(regex_liwc, types, 'regex', case_insensitive=TRUE))

setdiff(regex2fixed(regex_liwc, types, 'regex', case_insensitive=FALSE),
        regex2fixed3(regex_liwc, types, 'regex', case_insensitive=FALSE))

setdiff(regex2fixed(regex_liwc, types, 'regex', case_insensitive=FALSE),
        regex2fixed4(regex_liwc, index(types, 'regex', case_insensitive=FALSE)))

dict_lex <- dictionary(file='/home/kohei/Documents/Dictonary/Lexicoder/LSDaug2015/LSD2015_NEG.lc3')
glob_lex <- tokens(unlist(dict_lex, use.names = FALSE), hash=FALSE, what='fastest')
regex_lex <- lapply(glob_lex, glob2rx)

microbenchmark::microbenchmark(
    regex2fixed(regex_lex, types, 'regex', case_insensitive=TRUE),
    regex2fixed2(regex_lex, types, 'regex', case_insensitive=TRUE),
    regex2fixed3(regex_lex, types, 'regex', case_insensitive=TRUE),
    regex2fixed4(regex_lex, index(types, 'regex', case_insensitive=TRUE)),
    times=1
)


microbenchmark::microbenchmark(
    regex2fixed(list(c('^not$', '^go')), types, 'regex', case_insensitive=TRUE),
    regex2fixed2(list(c('^not$', '^go')), types, 'regex', case_insensitive=TRUE),
    regex2fixed3(list(c('^not$', '^go')), types, 'regex', case_insensitive=TRUE),
    regex2fixed4(list(c('^not$', '^go')), types, 'regex', case_insensitive=TRUE)
)

microbenchmark::microbenchmark(
    regex2fixed(list(c('^not$', '^go')), types, 'regex', case_insensitive=FALSE),
    regex2fixed2(list(c('^not$', '^go')), types, 'regex', case_insensitive=FALSE),
    regex2fixed3(list(c('^not$', '^go')), types, 'regex', case_insensitive=FALSE),
    regex2fixed4(list(c('^not$', '^go')), types, 'regex', case_insensitive=FALSE)
)

microbenchmark::microbenchmark(
    regex2fixed(list(c('not', 'go')), types, 'fixed', case_insensitive=TRUE),
    regex2fixed2(list(c('not', 'go')), types, 'fixed', case_insensitive=TRUE),
    regex2fixed3(list(c('not', 'go')), types, 'fixed', case_insensitive=TRUE),
    regex2fixed4(list(c('not', 'go')), types, 'fixed', case_insensitive=TRUE)
)

microbenchmark::microbenchmark(
    regex2fixed3(list(c('^not$', '^go')), types, 'regex', case_insensitive=TRUE),
    regex2fixed3(list(c('not', 'go')), types, 'fixed', case_insensitive=TRUE)
)
