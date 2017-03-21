# @param regex a list of regular expression
# @param types unique types of tokens
# @param case_insensitive case sensitivity
# @param If TRUE, index is constructed automatically. It also accept index constructed 
#        by index_regex(). 
#
# regex <- list(c('^a$', '^b'), c('c'), c('d'))
# types <- c('A', 'AA', 'B', 'BB', 'BBB', 'C', 'CC')
# regex2fixed(regex, types, 'regex', case_insensitive=TRUE)
# index <- index_regex(types, 'regex', case_insensitive=TRUE)
# regex2fixed(regex, types, 'regex', case_insensitive=TRUE, index=index)

regex2fixed <- function(regex, types, valuetype, case_insensitive = FALSE, index = TRUE){
    id <- regex2id(regex, types, valuetype, case_insensitive, index)
    fixed <- lapply(id, function(x) types[x])
    return(fixed)
}

# This function converts regex to type IDs. This is one of the coner strones of 
# the new artchitecture, but not yet really fast. Performance improvement is needed.
# @params the same as regex2fixed()
# regex <- list(c('^a$', '^b'), c('c'), c('d'))
# types <- c('A', 'AA', 'B', 'BB', 'BBB', 'C', 'CC')
# regex2id(regex, types, 'regex', case_insensitive=TRUE)

regex2id <- function(regex, types, valuetype, case_insensitive = FALSE, index = TRUE) {
    
    # Make case insensitive
    if (case_insensitive) {
        types_search <- stringi::stri_trans_tolower(types)
        regex <- lapply(regex, stringi::stri_trans_tolower)
    } else {
        types_search <- types
    }
    
    # Only when index is not constructed externally
    if (is.logical(index)) {
        # Glob is treated as fixed if neither * or ? is found
        regex_unlist <- unlist(regex, use.names = FALSE)
        if (valuetype == 'glob' && 
           !any(stringi::stri_detect_fixed(regex_unlist, '*')) &&
           !any(stringi::stri_detect_fixed(regex_unlist, '?'))) {
            valuetype <- 'fixed'
        } 
    }
    
    # Convert fixed or glob to regex
    if (valuetype == 'glob') regex <- lapply(regex, function(x) utils::glob2rx(escape_regex(x)))

    # Set if exact match of not
    if (valuetype == 'fixed'){
        exact <- TRUE
    } else {
        exact <- FALSE
    }
    
    if (is.logical(index)) {
        if (index){
            # Construct index if not given
            len_max <- max(stringi::stri_length(unlist(regex, use.names = FALSE)))
            index <- index_regex(types_search, valuetype, case_insensitive, len_max)
        } else {
            # Use stri_detect when index is null
            index <- NULL
        }
    }
    
    # Separate multi and single-entry patterns
    len <- lengths(regex)
    pats_multi <- regex[len>1] 
    pats_single <- regex[len==1]
    
    # Process multi-entry patterns
    id <- list()
    for (pat_multi in pats_multi) {
        id_multi <- select_types(pat_multi, types_search, exact, index)
        id <- c(id, expand(id_multi))
    }
    
    # Process single-entry patterns
    if (length(pats_single) > 0) {
        pats_single <- unlist(pats_single, use.names = FALSE)
        id_single <- unlist(select_types(pats_single, types_search, exact, index), use.names = FALSE)
        id <- c(id, id_single)
    }
    return(unique(id))
}

# This is an internal function for regex2id().
# This function subset types avoiding expensive full regular expression matching.
# @param regex a list of regular expression
# @param types_search lowercase types when case_insensitive=TRUE
# @param exact TRUE, if valuetype=fixed
# @param index index is used to find types without sequential search
select_types <- function (regex, types_search, exact, index){


    subset <- lapply(regex, function(regex, types_search, exact, index){
        if (length(index)) {
            if (exact) {
                #cat('Exact match', regex, '\n')
                return(search_index(regex, index))
            } else {
                if (regex == '') {
                    return(NULL) # return nothing for empty pattern
                } else if (regex == '^') {
                    return(seq_along(types_search)) # return all types when glob is *
                } else if (length((pos <- search_index(regex, index)))) {
                    #cat('Index search', regex, '\n')
                    return(pos)
                } else if (!is_indexed(regex)) {
                    #cat('Regex search', regex, '\n')
                    return(which(stringi::stri_detect_regex(types_search, regex)))
                } else {
                    #cat("Not found\n")
                    return(NULL)
                }
            }
        } else {
            if (exact) {
                return(which(types_search %in% regex))
            } else {
                return(which(stringi::stri_detect_regex(types_search, regex)))
            }
        }
    }, types_search, exact, index)
    return(subset)
}


# This is an internal function for regex2id().
# This function construct an index of regex patters of ^xxxx, xxxx$ and ^xxxx$ 
# to avoide expensive sequential search by stri_detect_regex. len_max should be obtained 
# from the longest regex queries to limit the size of the index.
index_regex <- function(types, valuetype, case_insensitive, len_max){
    
    if (case_insensitive) types <- stringi::stri_trans_tolower(types)
    if (valuetype == 'fixed') {
        exact <- TRUE
    } else {
        exact <- FALSE
    }
    # Create regex patterns from types
    if (!exact) { 
        types <- stringi::stri_c("^", types, "$")
    }
    # Index for regex patterns of ^xxxx$ 
    pos_tmp <- seq_along(types)
    key_tmp <- list(types)

    # Index for regex patterns of ^xxxx and xxxx$
    if (!exact) {
        len <- stringi::stri_length(types)
        if (missing(len_max)) len_max <- max(len) # index all the types if len_max is unknown
        for (i in 2:len_max) {
            k <- which(len > i)
            pos_tmp <- c(pos_tmp, list(rep(k, 2)))
            key_tmp <- c(key_tmp, list(stringi::stri_sub(types[k], 1, i)))
            key_tmp <- c(key_tmp, list(stringi::stri_sub(types[k], i * -1, -1)))
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

# This is an internal function for select_types().
search_index <- function(key, index){
    index[[fastmatch::fmatch(key, attr(index, 'key'))]] # use fmatch instead of names for quick access
}

# This function is a simplyfied version of expand.grid() in base package
expand <- function(elem){
    
    m <- prod(lengths(elem))
    comb <- vector("list", m)
    if(m == 0) return(comb)
    k <- 1L
    for (i in seq_along(elem)) {
        vec <- elem[[i]]
        l <- length(vec)
        m <- m / l
        vec_rep <- vec[rep.int(rep.int(seq_len(l), rep.int(k, l)), m)]
        k <- k * l
        for (j in seq_along(vec_rep)) {
            comb[[j]] <- c(comb[[j]], vec_rep[j])
        }
    }
    return(comb)
}

# This is an internal function for select_types(). 
# This function checks if a string is regular expression
is_regex <- function(x){
    any(stringi::stri_detect_fixed(x, c(".", "(", ")", "^", "{", "}", "+", "$", "*", "?", "[", "]", "\\")))
}

# This is an internal function for select_types(). This function escapes glob patterns before
# utils:glob2rx(), therefore allow * and ? are allowed
escape_regex <- function(x){
    #stringi::stri_replace_all_regex(x, "([.()^\\{\\}+$*\\[\\]\\\\])", "\\\\$1") # escape any
    stringi::stri_replace_all_regex(x, "([.()^\\{\\}+$\\[\\]\\\\])", "\\\\$1") # allow glob
}

# This is an internal function for select_types().
is_indexed <- function(x){
    head <- stringi::stri_startswith_fixed(x, '^')
    tail <- stringi::stri_endswith_fixed(x, '$')
    if (head && tail && !is_regex(stringi::stri_sub(x, 2, -2))) return(TRUE)
    if (head && !is_regex(stringi::stri_sub(x, 2, -1))) return(TRUE)
    if (tail && !is_regex(stringi::stri_sub(x, 1, -2))) return(TRUE)
    return(FALSE)
}

#' convert a vector to a list
#' 
#' Utility function to convert a vector to a list, used by \code{link{kwic}}.
#' @param x character (vector)
#' @keywords internal utilities
vector2list <- function(x) {
    if (is.list(x)) {
        if (!all(vapply(x, is.character, logical(1), USE.NAMES = FALSE)))
            stop("Patterns must be a list of character type only")
        return(x)
    } else {
        # message('Vector pattern is converted to list')
        return(stringi::stri_split_fixed(x, ' '))
    }
}

