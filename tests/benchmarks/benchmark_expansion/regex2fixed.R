require(stringi)
require(fastmatch)
require(quanteda)
quanteda_options(threads = 8)

corp <- readRDS("/home/kohei/Documents/Brexit/Data/data_corpus_guardian.RDS")
toks <- tokens(corp)
#toks <- tokens(inaugCorpus)

types <- attr(toks, 'types')
dict_liwc <- dictionary(file='/home/kohei/Documents/Dictionary/LIWC/LIWC2007_English.dic')
regex_liwc <- glob2rx(unlist(dict_liwc, use.names = FALSE))


out1 <- unique(unlist(lapply(head(regex_liwc, 1000), function(x, y) stri_subset_regex(y, x, case_insensitive=TRUE), types)))
out2 <- unlist(pattern2fixed(head(regex_liwc, 1000), types, 'regex', case_insensitive=TRUE))
identical(out1, out2)
setdiff(out1, out2)
setdiff(out2, out1)

microbenchmark::microbenchmark(
    lapply(regex_liwc, function(x, y) stri_subset_regex(x, y, case_insensitive=TRUE), types),
    pattern2fixed(regex_liwc, types, 'regex', case_insensitive=TRUE),
    times=1
)

dict_lex <- dictionary(file='/home/kohei/Documents/Dictionary/Lexicoder/LSDaug2015/LSD2015_NEG.lc3')
glob_lex <- tokens(unlist(dict_lex, use.names = FALSE), hash=FALSE, what='fastest')
regex_lex <- lapply(glob_lex, glob2rx)

microbenchmark::microbenchmark(
    regex2fixed4(regex_lex, index(types, 'regex', case_insensitive=TRUE)),
    pattern2fixed(regex_lex, types, 'regex', case_insensitive=TRUE),
    times=1
)

profvis::profvis(pattern2fixed(regex_liwc, types, 'glob', case_insensitive=TRUE))
profvis::profvis(pattern2fixed(list(c('*')), types, 'glob', case_insensitive=TRUE))
profvis::profvis(pattern2fixed(list(c('not', '*')), types, 'glob', case_insensitive=TRUE))

