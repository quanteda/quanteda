# This function converts regex to fixed patterns. This is one of the coner strones of 
# the new artchitecture, but not yet really fast. Performance improvement is needed.

# @param regex regular expression
# @param types unique types of tokens
# @param case_insensitive case sensitivity
#
# regex <- list(c('^a$', '^b'), c('c'), c('d'))
# types <- c('A', 'AA', 'B', 'BB', 'BBB', 'C', 'CC')
# regex2fixed4(regex, types, 'fixed', case_insensitive=TRUE)
# regex2fixed4(regex, types, 'regex', case_insensitive=TRUE)

regex2fixed4 <- function(regex, types, valuetype, case_insensitive = FALSE) {
    
    # Initialize
    fixed <- list()
    types_lower <- stri_trans_tolower(types)

    # Make case insensitive
    if(case_insensitive){
        regex <- lapply(regex, stri_trans_tolower)
        types_search = types_lower
    }else{
        types_search = types
    }
    
    # Construct full-index of types
    if(valuetype == 'fixed'){
        exact <- TRUE
        index <- list(exact=index(types_search, 0))
    }else{
        exact <- FALSE
        index <- list(exact=index(types_search, 0),
                      head=index(types_search, 1), 
                      tail=index(types_search, -1))
    }
    
    # Separate multi and single-entry patterns
    len <- lengths(regex)
    pats_multi <- regex[len>1] 
    pats_single <- regex[len==1]
    
    # Process multi-entry patterns
    for(pat_multi in pats_multi) {
        fixed_multi <- subset_types3(pat_multi, types, types_search, exact, index)
        fixed <- c(fixed, expand(fixed_multi))
    }
    
    # Process single-entry patterns
    if(length(pats_single) > 0){
        pats_single <- unlist(pats_single, use.names = FALSE)
        fixed_single <- unlist(subset_types3(pats_single, types, types_search, exact, index), use.names = FALSE)
        fixed <- c(fixed, fixed_single)
    }
    return(unique(fixed))
}

# This function subset types avoiding expensive full regular expression matching
subset_types3 <- function (regex, types, types_search, exact, index){
    
    subset <- lapply(regex, function(regex, types, types_search, exact, index){
        if(exact){
            #cat('Exact', regex, '\n')
            types[index[[1]][[regex]]]
        }else{
            #cat('Search', regex, '\n')
            head <- stri_startswith_fixed(regex, '^')
            tail <- stri_endswith_fixed(regex, '$')
            if(head & stri_length(regex) == 1){
                #cat('Any\n')
                types # return all types when glob is *
            }else if(head & tail & !is_regex((str <- stri_sub(regex, 2, -2)))){
                #cat('Exact', str, '\n')
                types[index[[1]][[str]]]
            }else if(head & !is_regex((str <- stri_sub(regex, 2, -1)))){
                #cat('Starts with', str, '\n')
                types[index[[2]][[str]]]
            }else if(tail & !is_regex((str <- stri_sub(regex, 1, -2)))){
                #cat('End with', str, '\n')
                types[index[[3]][[str]]]
            }else if(regex == ''){
                NULL # retun nothing for empty pattern
            }else{
                types[stri_detect_regex(types_search, regex)]
            }
        }
    }, types, types_search, exact, index)
    return(subset)
}


# This function create full-index of types
index <- function(types, direction){
    key <- c()
    pos <- c()
    len <- stri_length(types)
    for(i in sort(unique(len))){
        if(direction == 0){
            j <- which(len == i)
        }else{
            j <- which(len >= i)
        }
        pos <- c(pos, j)
        if(direction >= 0){
            key <- c(key, stri_sub(types[j], 1, i))
        }else{
            key <- c(key, stri_sub(types[j], i * -1, -1))
        }
    }
    idx <- split(pos, factor(key, ordered=FALSE, levels=unique(key)))
    return(list2env(idx))
}

expand <- function(val){
    comb <- vector('list', prod(lengths(val))) # empty list for output
    expand_resursive(val, 1, comb)
}

expand_resursive <- function(val, i, comb){
    h = 1
    for(j in rep_len(1:length(val[[i]]), length(comb))){
        comb[[h]] <- c(comb[[h]], val[[i]][[j]])
        h <- h + 1
    }
    if(i < length(val)){
        comb <- expand_resursive(val, i + 1, comb)
    }
    return(comb)
}

# This function checks if a string is regular expression
is_regex <- function(x){
    any(stri_detect_fixed(x, c(".", "(", ")", "^", "{", "}", "+", "$", "*", "?", "[", "]", "\\")))
}
