toks <- tokens(inaugCorpus, removePunct = TRUE)
types <- attr(toks, 'types')

dict_liwc <- dictionary(file='/home/kohei/Documents/Dictonary/LIWC/LIWC2007_English.dic')
regex_liwc <- glob2rx(unlist(dict_liwc, use.names = FALSE))

microbenchmark::microbenchmark(
    regex2fixed(regex, types, 'regex', case_insensitive=TRUE),
    regex2fixed2(regex, types, 'regex', case_insensitive=TRUE),
    times=1
)

(out <- regex2fixed(head(regex, 100), types, 'regex', case_insensitive=TRUE))
(out2 <- regex2fixed2(head(regex, 100), types, 'regex', case_insensitive=TRUE))

dict_lex <- dictionary(file='/home/kohei/Documents/Dictonary/Lexicoder/LSDaug2015/LSD2015_NEG.lc3')
glob_lex <- tokens(unlist(dict_lex, use.names = FALSE), hash=FALSE, what='fastest')
regex_lex <- lapply(glob_lex, glob2rx)

microbenchmark::microbenchmark(
    regex2fixed(regex_lex, types, 'regex', case_insensitive=TRUE),
    regex2fixed2(regex_lex, types, 'regex', case_insensitive=TRUE),
    times=1
)

(out3 <- regex2fixed(head(regex_lex, 10000), types, 'regex', case_insensitive=TRUE))
(out4 <- regex2fixed2(head(regex_lex, 10000), types, 'regex', case_insensitive=TRUE))

l <- as.list(1:10000)
microbenchmark::microbenchmark(
    for(i in 1:10000){l[[i]]},
    lapply(l, function(x){})
)

microbenchmark::microbenchmark(
    types[fmatch(c('go', 'to'), types)],
    types[c('go', 'to') %in% types]
)

lapply(as.list(1:10), function(x){list(1:2)})

microbenchmark::microbenchmark(
    regex2fixed(list(c('^not$', '^go')), types, 'regex', case_insensitive=TRUE),
    regex2fixed2(list(c('^not$', '^go')), types, 'regex', case_insensitive=TRUE)
)
microbenchmark::microbenchmark(
    regex2fixed(list(c('^not$', '^go')), types, 'regex', case_insensitive=FALSE),
    regex2fixed2(list(c('^not$', '^go')), types, 'regex', case_insensitive=FALSE)
)



#regex <- list(c('^a$', '^b'), c('c'), c('d'))
#types <- c('A', 'AA', 'B', 'BB', 'BBB', 'C', 'CC')

toks <- tokens(inaugCorpus, removePunct = TRUE)
types <- attr(toks, 'types')

glob <- paste0(letters, rep('*', 26))
regex <- paste0(rep('^', 26), letters)
regex <- paste0(rep('^', 26), letters, rep('$', 26))


dict_liwc <- dictionary(file='/home/kohei/Documents/Dictonary/LIWC/LIWC2007_English.dic')
regex <- glob2rx(unlist(dict_liwc, use.names = FALSE))

microbenchmark::microbenchmark(
    stri_subset_regex(types, paste0(regex, collapse = '|'), case_insensitive=TRUE),
    lapply(regex, function(x, y) stri_subset_regex(y, x, case_insensitive=TRUE), types),
    subset_types(regex, types, case_insensitive=TRUE),
    subset_types(paste0(regex, collapse = '|'), types, case_insensitive=TRUE)
)

microbenchmark::microbenchmark(
    lapply(regex, function(x, y) stri_subset_regex(y, x, case_insensitive=TRUE), types),
    lapply(regex, function(x, y) stri_subset_regex(y, x, case_insensitive=FALSE), types),
    times=1
)


