source('R/regex2fixed4.R')
source('R/regex2fixed5.R')
library(stringi)
library(fastmatch)


load("/home/kohei/Documents/Brexit/Analysis/data_corpus_guardian.RData")
toks <- tokens(data_corpus_guardian, removePunct = TRUE)
toks <- tokens(inaugCorpus, removePunct = TRUE)

types <- attr(toks, 'types')
dict_liwc <- dictionary(file='/home/kohei/Documents/Dictionary/LIWC/LIWC2007_English.dic')
regex_liwc <- glob2rx(unlist(dict_liwc, use.names = FALSE))

microbenchmark::microbenchmark(
stringi_sub = unique(unlist(lapply(head(regex_liwc, 1000), function(x, y) stri_subset_regex(y, x, case_insensitive=TRUE), types))),
stringi = unique(unlist(lapply(regex_liwc, function(x, y) stri_subset_regex(y, x, case_insensitive=TRUE), types))),
regex2fixed_sub = unlist(regex2fixed5(head(regex_liwc, 1000), types, 'regex', case_insensitive=TRUE)),
regex2fixed = unlist(regex2fixed5(regex_liwc, types, 'regex', case_insensitive=TRUE)),
times=1
)

out1 = unique(unlist(lapply(head(regex_liwc, 1000), function(x, y) stri_subset_regex(y, x, case_insensitive=TRUE), types)))
out2 = unlist(regex2fixed5(head(regex_liwc, 1000), types, 'regex', case_insensitive=TRUE))
identical(out1, out2)
setdiff(out1, out2)
setdiff(out2, out1)

microbenchmark::microbenchmark(
    lapply(regex_liwc, function(x, y) stri_subset_regex(x, y, case_insensitive=TRUE), types),
    regex2fixed5(regex_liwc, types, 'regex', case_insensitive=TRUE),
    times=1
)

dict_lex <- dictionary(file='/home/kohei/Documents/Dictionary/Lexicoder/LSDaug2015/LSD2015_NEG.lc3')
glob_lex <- tokens(unlist(dict_lex, use.names = FALSE), hash=FALSE, what='fastest')
regex_lex <- lapply(glob_lex, glob2rx)

microbenchmark::microbenchmark(
    regex2fixed4(regex_lex, index(types, 'regex', case_insensitive=TRUE)),
    regex2fixed5(regex_lex, types, 'regex', case_insensitive=TRUE),
    times=1
)
profvis::profvis(regex2fixed5(regex_liwc, types, 'regex', case_insensitive=TRUE))
profvis::profvis(regex2fixed4(regex_liwc, index(types, 'regex', case_insensitive=TRUE)))
profvis::profvis(regex2fixed5(list(c('*')), types, 'glob', case_insensitive=TRUE))
profvis::profvis(regex2fixed5(list(c('not', '*')), types, 'glob', case_insensitive=TRUE))

