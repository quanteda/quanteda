# This function converts regex to fixed patterns. This is one of the coner strones of 
# the new artchitecture, but not yet really fast. Performance improvement is needed.

# @param regex regular expression
# @param types unique types of tokens
# @param case_insensitive case sensitivity
#
# regex <- list(c('^a$', '^b'), c('c'), c('d'))
# types <- c('A', 'AA', 'B', 'BB', 'BBB', 'C', 'CC')
# regex2fixed3(regex, types, 'fixed', case_insensitive=TRUE)
# regex2fixed3(regex, types, 'regex', case_insensitive=TRUE)

regex2fixed3 <- function(regex, types, valuetype, case_insensitive = FALSE) {
    
    # Initialize
    fixed <- list()
    types_lower <- toLower(types)
    if(valuetype == 'fixed'){
        exact <- TRUE
    }else{
        exact <- FALSE
    }
    
    # Construct tree to reduce search time
    tree <- list(head=split(types, stri_sub(types_lower, 1, 1)),
                 tail=split(types, stri_sub(types_lower, -1, -1)))
    
    # Make case insensitive
    if(case_insensitive){
        regex <- lapply(regex, toLower)
        types_search <- types_lower
        tree_search <- list(head=list2env(lapply(tree$head, toLower)),
                            tail=list2env(lapply(tree$tail, toLower)))
    }else{
        types_search <- types
        tree_search <- list(head=list2env(tree$head, toLower),
                            tail=list2env(tree$tail, toLower))
    }
    
    # Separate multi and single-entry patterns
    len <- lengths(regex)
    pats_multi <- regex[len>1] 
    pats_single <- regex[len==1]
    
    # Process multi-entry patterns
    for(pat_multi in pats_multi) {
        fixed_multi <- subset_types2(pat_multi, types, types_search, tree, tree_search, exact)
        fixed <- c(fixed, expand(fixed_multi))
    }
    
    # Process single-entry patterns
    if(length(pats_single) > 0){
        pats_single <- unlist(pats_single, use.names = FALSE)
        fixed_single <- unlist(subset_types2(pats_single, types, types_search, tree, tree_search, exact), use.names = FALSE)
        fixed <- c(fixed, fixed_single)
    }
    return(unique(fixed))
}

# This function subset types avoiding expensive full regular expression matching
subset_types2 <- function (regex, types, types_search, tree, tree_search, exact){
    
    subset <- lapply(regex, function(regex, types, types_search, tree, tree_search, exact){
                if(exact){
                    subset_types_exact(regex, tree, tree_search)
                }else{
                    #cat('Search', regex, '\n')
                    head <- (stri_sub(regex, 1, 1) == '^')
                    tail <- (stri_sub(regex, -1, -1) == '$')
                    if(head & stri_length(regex) == 1){
                        #cat('Any\n')
                        types # return all types when glob is *
                    }else if(head & tail & !is_regex((body <- stri_sub(regex, 2, -2)))){
                        #cat('Exact', body, '\n')
                        subset_types_exact(body, tree, tree_search)
                    }else if(head & !is_regex((headless <- stri_sub(regex, 2, -1)))){
                        #cat('Starts with', headless, '\n')
                        subset_types_startswith(headless, tree, tree_search)
                    }else if(head & !is_regex((tailless <- stri_sub(regex, 1, -2)))){
                        #cat('End with', tailless, '\n')
                        subset_types_endswith(tailless, tree, tree_search)
                    }else if(regex == ''){
                        NULL # retun nothing for empty pattern
                    }else{
                        types[stri_detect_regex(types_search, regex)]
                    }
                }
            }, types, types_search, tree, tree_search, exact)
    return(subset)
}

subset_types_exact <- function(str, tree, tree_search){
    i <- toLower(stri_sub(str, 1, 1))
    tree$head[[i]][!is.na(fmatch(tree_search$head[[i]], str))]
}

subset_types_startswith <- function(str, tree, tree_search){
    i <- toLower(stri_sub(str, 1, 1))
    tree$head[[i]][stri_startswith_fixed(tree_search$head[[i]], str)]
}

subset_types_endswith <- function(str, tree, tree_search){
    i <- toLower(stri_sub(str, -1, -1))
    tree$tail[[i]][stri_startswith_fixed(tree_search$tail[[i]], str)]
}

# This function is a simplyfied version of expand.grid() in base package
expand <- function(elem){
    k <- 1L
    m <- prod(lengths(elem))
    comb <- vector("list", m)
    if(m == 0) return(comb)
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
