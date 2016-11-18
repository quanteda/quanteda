# This function converts regex to fixed patterns. This is one of the coner strones of 
# the new artchitecture, but not really fast. Performance improvement is needed.

# @param regex regular expression
# @param types unique types of tokens
# @param case_insensitive case sensitivity
#
# regex <- list(c('^a$', '^b'), c('c'), c('d'))
# types <- c('A', 'AA', 'B', 'BB', 'BBB', 'C', 'CC')
# regex2fixed2(regex, types, 'fixed', case_insensitive=TRUE)
# regex2fixed2(regex, types, 'regex', case_insensitive=TRUE)

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

regex2fixed2 <- function(regex, types, valuetype, case_insensitive = FALSE) {
    
    # Initialize
    fixed <- list()
    if(case_insensitive){
        types_lower <- toLower(types)
        regex <- lapply(regex, toLower)
    }
    
    # Separate multi and single-entry patterns
    len <- lengths(regex)
    pats_multi <- regex[len>1] 
    pats_single <- regex[len==1]
    
    # Process multi-entry patterns
    for(pat_multi in pats_multi) {
        if(valuetype == 'fixed'){
            if(case_insensitive){
                fixed_multi <- lapply(pat_multi, function(x, y, z) y[fmatch(x, z)], types, types_lower)
            }else{
                fixed_multi <- lapply(pat_multi, function(x, y) y[fmatch(x, y)], types)
            }
        }else{
            if(case_insensitive){
                fixed_multi <- subset_types(pat_multi, types, types_lower)
            }else{
                fixed_multi <- subset_types(pat_multi, types, types)
            }
        }
        fixed_comb <- as.matrix(do.call(expand.grid, c(fixed_multi, stringsAsFactors = FALSE))) # create all possible combinations
        fixed <- c(fixed, unname(split(fixed_comb, row(fixed_comb))))
    }
    
    # Process single-entry patterns
    if(length(pats_single) > 0){
        pats_single <- unlist(pats_single, use.names = FALSE)
        if (valuetype == 'fixed'){
            if(case_insensitive){
                fixed_single <- as.list(types[fmatch(pats_single, types_lower)])
            }else{
                fixed_single <- as.list(types[fmatch(pats_single, types)])
            }
        }else{
            if(case_insensitive){
                fixed_single <- unlist(subset_types(pats_single, types, types_lower), use.names = FALSE)
            }else{
                fixed_single <- unlist(subset_types(pats_single, types, types), use.names = FALSE)
            }
        }
        fixed <- c(fixed, fixed_single)
    }
    return(fixed)
}

# This function subset types avoiding expensive full regular expression matching
subset_types <- function (regex, types, types_search){
    
    subset <- lapply(regex, function(x, y, z){
                head <- (stri_sub(x, 1, 1) == '^')
                tail <- (stri_sub(x, -1, -1) == '$')
                if(head & tail & !is_regex((body <- stri_sub(x, 2, -2)))){
                    y[(fmatch(body, z))]
                }else if(head & !is_regex((headless <- stri_sub(x, 2, -1)))){
                    y[stri_startswith_fixed(z, headless)]   
                }else if(head & !is_regex((tailless <- stri_sub(x, 1, -2)))){
                    y[stri_endswith_fixed(z, tailless)]
                }else{
                    y[stri_detect_regex(z, x)]
                }
            }, types, types_search)
    return(subset)
}

# This function checks if a string is regular expression
is_regex <- function(x){
    any(stri_detect_fixed(x, c(".", "(", ")", "^", "{", "}", "+", "$", "*", "?", "[", "]", "\\")))
}    



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
