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
    types_search <- ifelse(case_insensitive, stri_trans_tolower(types), types)
    
    # Set if exact match of not
    exact <- ifelse(valuetype == 'fixed', TRUE, FALSE)
    
    # Construct index if not given
    if(missing(index)) index <- index_regex(types_search, valuetype)
    
    # Make case-insensitive
    if(case_insensitive) regex <- lapply(regex, stri_trans_tolower)
    
    # Convert glob to regex
    if(valuetype == 'glob') regex <- lapply(regex, glob2rx)
    
    # Separate multi and single-entry patterns
    len <- lengths(regex)
    pats_multi <- regex[len>1] 
    pats_single <- regex[len==1]
    
    # Process multi-entry patterns
    fixed <- list()
    for(pat_multi in pats_multi) {
        fixed_multi <- select_types(pat_multi, types, types_search, exact, index)
        # cat('pat_multi:\n')
        # print(pat_multi)
        # cat('fixed_multi:\n')
        # print(fixed_multi)
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
        if(exact){
            #cat('Exact match', regex, '\n')
            types[index[[regex]]]
        }else{
            if(regex == ''){
                NULL # retun nothing for empty pattern
            }else if(length((ids <- index[[regex]]))){
                #cat('Index search', regex, '\n')
                types[ids];
            }else{
                if(is_regex(regex)){ # check if it is a complex regex pattern
                    #cat('Sequential search', regex, '\n')
                    types[stri_detect_regex(types_search, regex)]
                }else{
                    NULL
                }
            }
        }
    }, types, types_search, exact, index)
    return(subset)
}

index_regex <- function(types, valuetype, case_insensitive = FALSE){
    
    if(case_insensitive) types <- stri_trans_tolower(types)
    exact <- ifelse(valuetype == 'fixed', TRUE, FALSE)
    
    key <- c()
    pos <- c()
    types <- stri_c("^", types, "$")
    len <- stri_length(types)
    for(i in 2:max(len)){
    
        # Exact
        j <- which(len == i)
        pos <- c(pos, j)
        key <- c(key, types[j])
        
        # Starts or ends with
        if(!exact){
            k <- which(len > i)
            pos <- c(pos, k, k)
            key <- c(key, stri_sub(types[k], 1, i), stri_sub(types[k], i * -1, -1))
        }
    }
    idx <- split(pos, factor(key, ordered=FALSE, levels=unique(key)))
    return(list2env(idx))
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
    any(stri_detect_fixed(x, c(".", "(", ")", "{", "}", "+", "*", "?", "[", "]", "\\")))
}


