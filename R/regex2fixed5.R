# This function converts regex to fixed patterns. This is one of the coner strones of 
# the new artchitecture, but not yet really fast. Performance improvement is needed.

# @param regex regular expression
# @param types unique types of tokens
# @param case_insensitive case sensitivity
#
# regex <- list(c('^a$', '^b'), c('c'), c('d'))
# types <- c('A', 'AA', 'B', 'BB', 'BBB', 'C', 'CC')
# regex2fixed5(regex, types, 'regex', case_insensitive=TRUE)
# index <- index_regex(types, 'regex', case_insensitive=TRUE)
# regex2fixed5(regex, types, 'regex', case_insensitive=TRUE, index=index)

regex2fixed5 <- function(regex, types, valuetype, case_insensitive = FALSE, index = NULL) {
    
    # Make case insensitive
    if(case_insensitive){
        types_search <- stri_trans_tolower(types)
    }else{
        types_search <- types
    }
    
    # Set if exact match of not
    if(valuetype == 'fixed'){
        exact <- TRUE
    }else{
        exact <- FALSE
    }
    
    # Make case-insensitive
    if(case_insensitive) regex <- lapply(regex, stri_trans_tolower)
    
    # Convert fixed or glob to regex
    if(valuetype == 'glob') regex <- lapply(regex, glob2rx)

    # Construct index if not given
    #if(length(regex) > 10 && is.null(index)){ # do not construct index for few patterns
    if(is.null(index)){
        len_max <- max(stri_length(unlist(regex, use.names = FALSE)))
        index <- index_regex(types_search, valuetype, case_insensitive, len_max)
    }
    
    # Separate multi and single-entry patterns
    len <- lengths(regex)
    pats_multi <- regex[len>1] 
    pats_single <- regex[len==1]
    
    # Process multi-entry patterns
    fixed <- list()
    for(pat_multi in pats_multi) {
        fixed_multi <- select_types(pat_multi, types, types_search, exact, index)
        fixed <- c(fixed, expand(fixed_multi))
    }
    
    # Process single-entry patterns
    if(length(pats_single) > 0){
        pats_single <- unlist(pats_single, use.names = FALSE)
        fixed_single <- unlist(select_types(pats_single, types, types_search, exact, index), use.names = FALSE)
        fixed <- c(fixed, fixed_single)
    }
    return(unique(fixed))
}


# This function subset types avoiding expensive full regular expression matching
select_types <- function (regex, types, types_search, exact, index){


    subset <- lapply(regex, function(regex, types, types_search, exact, index){
        if(length(index)){
            if(exact){
                #cat('Exact match', regex, '\n')
                types[search_index(regex, index)]
            }else{
                if(regex == ''){
                    NULL # return nothing for empty pattern
                }else if(regex == '^'){
                    types # return all types when glob is *
                }else if(length((ids <- search_index(regex, index)))){
                    #cat('Index search', regex, '\n')
                    types[ids]
                }else if(!is_indexed(regex)){
                    #cat('Regex search', regex, '\n')
                    types[stri_detect_regex(types_search, regex)]
                }else{
                    #cat("Not found\n")
                    NULL
                }
            }
        }else{
            types[stri_detect_regex(types_search, regex)]
        }
    }, types, types_search, exact, index)
    return(subset)
}

# This function construct an index of regex patters of ^xxxx, xxxx$ and ^xxxx$ 
# to avoide expensive sequential search by stri_detect_regex. len_max should be obtained 
# from the longest regex queries to limit the size of the index.
index_regex <- function(types, valuetype, case_insensitive, len_max){
    
    if(case_insensitive) types <- stri_trans_tolower(types)
    if(valuetype == 'fixed'){
        exact <- TRUE
    }else{
        exact <- FALSE
    }
    # Create regex patterns from types
    if(!exact){ 
        types <- escape_regex(types) # punctuations are not regular expressions
        types <- stri_c("^", types, "$")
    }
    # Index for regex patterns of ^xxxx$ 
    pos_tmp <- seq_along(types)
    key_tmp <- list(types)

    # Index for regex patterns of ^xxxx and xxxx$
    if(!exact){
        len <- stri_length(types)
        if(missing(len_max)) len_max <- max(len) # index all the types if len_max is unknown
        for(i in 2:len_max){
            k <- which(len > i)
            pos_tmp <- c(pos_tmp, list(rep(k, 2)))
            key_tmp <- c(key_tmp, list(stri_sub(types[k], 1, i)))
            key_tmp <- c(key_tmp, list(stri_sub(types[k], i * -1, -1)))
        }
    }
    # Faster to join vectors in the end
    key <- unlist(key_tmp, use.names = FALSE) 
    pos <- unlist(pos_tmp, use.names = FALSE)
    index <- split(pos, factor(key, ordered=FALSE, levels=unique(key))) # set factor for quick split
    attr(index, 'key') <- names(index)
    index <- unname(index)
    return(index)
}

search_index <- function(key, index){
    index[[fmatch(key, attr(index, 'key'))]] # use fmatch instead of names for quick access
}

# This function is a simplyfied version of expand.grid() in base package
expand <- function(elem){
    
    m <- prod(lengths(elem))
    comb <- vector("list", m)
    if(m == 0) return(comb)
    k <- 1L
    for (i in 1:length(elem)) {
        vec <- elem[[i]]
        l <- length(vec)
        m <- m / l
        vec_rep <- vec[rep.int(rep.int(seq_len(l), rep.int(k, l)), m)]
        k <- k * l
        for (j in 1:length(vec_rep)){
            comb[[j]] <- c(comb[[j]], vec_rep[j])
        }
    }
    return(comb)
}


# This function checks if a string is regular expression
is_regex <- function(x){
    any(stri_detect_fixed(x, c(".", "(", ")", "^", "{", "}", "+", "$", "*", "?", "[", "]", "\\")))
}

escape_regex <- function(x){
    stri_replace_all_regex(x, "([.()^\\{\\}+$*\\[\\]\\\\])", "\\\\$1")
}

is_indexed <- function(x){
    head <- stri_startswith_fixed(x, '^')
    tail <- stri_endswith_fixed(x, '$')
    if(head && tail && !is_regex(stri_sub(x, 2, -2))) return(TRUE)
    if(head && !is_regex(stri_sub(x, 2, -1))) return(TRUE)
    if(tail && !is_regex(stri_sub(x, 1, -2))) return(TRUE)
    return(FALSE)
}


vector2list <- function(x){
    if(typeof(x) == "list"){
        return(x)
    }else{
        #message('Vector pattern is converted to list')
        return(stri_split_fixed(x, ' '))
    }
}

