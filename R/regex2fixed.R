#' converts regex and glob patterns to fixed patters
#' @inherit regex2id
#' @return a list of character vectors containing types
#' @keywords internal
#' @rdname regex2id
#' @examples
#' pattern <- list(c('^a$', '^b'), c('c'), c('d'))
#' types <- c('A', 'AA', 'B', 'BB', 'BBB', 'C', 'CC')
#' quanteda:::regex2fixed(pattern, types, 'regex', case_insensitive=TRUE)
#' index <- quanteda:::index_types(types, 'regex', case_insensitive=TRUE)
#' quanteda:::regex2fixed(pattern, types, 'regex', case_insensitive=TRUE, index=index)

regex2fixed <- function(pattern, types, valuetype, case_insensitive = FALSE, index = TRUE){
    id <- regex2id(pattern, types, valuetype, case_insensitive, index)
    fixed <- lapply(id, function(x) types[x])
    return(fixed)
}

#' converts regex and glob patterns to type IDs
#' 
#' This function converts regex or glob to type IDs to allow C++ function to
#' perform fast searches in tokens object. C++ functions use a list of type IDs
#' to construct a hash table, agaist which sub-vectors of tokens object are
#' matched.
#' @inheritParams pattern
#' @param types unique types of tokens obtaine by \code{quanteda:::types()}
#' @inheritParams valuetype
#' @param case_insensitive ignore case when matching, if \code{TRUE}
#' @param index If TRUE, index is constructed automatically. It also accept index 
#'   constructed by index_types().
#' @return a list of integer vectors containing type IDs
#' @keywords internal
#' @examples
#' pattern <- list(c('^a$', '^b'), c('c'), c('d'))
#' types <- c('A', 'AA', 'B', 'BB', 'BBB', 'C', 'CC')
#' quanteda:::regex2id(pattern, types, 'regex', case_insensitive=TRUE)

regex2id <- function(pattern, types, valuetype, case_insensitive = FALSE, index = TRUE) {
    
    if (!length(pattern)) return(list())
        
    # Normalize
    pattern <- lapply(pattern, stri_trans_nfc)
    types <- stri_trans_nfc(types)
    
    # Make case insensitive
    if (case_insensitive) {
        types_search <- stri_trans_tolower(types)
        pattern <- lapply(pattern, stri_trans_tolower)
    } else {
        types_search <- types
    }
    
    # Only when index is not constructed externally
    if (is.logical(index)) {
        # Glob is treated as fixed if neither * or ? is found
        pattern_unlist <- unlist(pattern, use.names = FALSE)
        if (valuetype == 'glob' && 
           !any(stri_detect_fixed(pattern_unlist, '*')) &&
           !any(stri_detect_fixed(pattern_unlist, '?'))) {
            valuetype <- 'fixed'
        } 
    }
    if (valuetype == 'glob') {
        # Convert fixed or glob to regex
        regex <- lapply(pattern, function(x) utils::glob2rx(escape_regex(x)))
    } else {
        regex <- pattern
    }
    
    # Set if exact match of not
    if (valuetype == 'fixed'){
        exact <- TRUE
    } else {
        exact <- FALSE
    }
    
    if (is.logical(index)) {
        if (index){
            # Construct index if not given
            max_len <- max(stri_length(unlist(regex, use.names = FALSE)))
            index <- index_types(types_search, valuetype, case_insensitive, max_len)
        } else {
            # Use stri_detect when index is null
            index <- NULL
        }
    }
    
    # Separate multi and single-entry patterns
    len <- lengths(regex)
    pats_multi <- regex[len > 1] 
    pats_single <- regex[len == 1]
    
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

#' select types without performing slow regex search
#' 
#' This is an internal function for \code{regex2id()} that select types using an
#' index of types by reular expressions.
#' @param regex a list of regular expressions
#' @param types_search lowercased types when \code{case_insensitive=TRUE}
#' @param exact set TRUE for \code{valuetype = fixed}
#' @param index index object created by \code{index_types()}
#' @keywords internal
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
                    return(which(stri_detect_regex(types_search, regex)))
                } else {
                    #cat("Not found\n")
                    return(NULL)
                }
            }
        } else {
            if (exact) {
                return(which(types_search %in% regex))
            } else {
                return(which(stri_detect_regex(types_search, regex)))
            }
        }
    }, types_search, exact, index)
    return(subset)
}


#' index types for fastest regular expression matches 
#' 
#' This is an internal function for regex2id() that constructs an index of regex
#' patters(e.g. ^xxxx, xxxx$ and ^xxxx$) to avoide expensive sequential search
#' by stri_detect_regex.
#' @inheritParams valuetype
#' @param case_insensitive ignore case when matching, if \code{TRUE}
#' @param max_len maximum length of types to be indexed
#' @return a list of integer vectors containing type IDs with index keys as an
#'   attribute
#' @keywords internal
#' @examples
#' types <- c('xxx', 'yyyy', 'ZZZ')
#' index <- quanteda:::index_types(types, 'regex', FALSE, 3)
#' type_id <- quanteda:::search_index('^y', index)
#' types[type_id]
index_types <- function(types, valuetype, case_insensitive, max_len = NULL){
    
    # Normalize
    types <- stri_trans_nfc(types)
    
    if (case_insensitive)
        types <- stri_trans_tolower(types)

    if (valuetype == 'fixed') {
        exact <- TRUE
    } else {
        exact <- FALSE
    }
    # Create regex patterns from types
    if (!exact) { 
        types <- stri_c("^", types, "$")
    }
    # Index for regex patterns of ^xxxx$ 
    pos_tmp <- seq_along(types)
    key_tmp <- list(types)

    # Index for regex patterns of ^xxxx and xxxx$
    if (!exact) {
        len <- stri_length(types)
        if (is.null(max_len)) max_len <- max(len) # index all the types if max_len is unknown
        for (i in 2:max_len) {
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

#' internal function for \code{select_types()} to seach the index using fastmatch.
#' @param regex a regular expression to search
#' @param index an index object created by \code{index_types()}
#' @seealso index_types
#' @keywords internal
search_index <- function(regex, index){
    index[[fastmatch::fmatch(regex, attr(index, 'key'))]] # use fmatch instead of names for quick access
}

#' simpler and faster version of expand.grid() in base package
#' @param elem list of elements to be combined
#' @keywords internal
#' @examples
#' quanteda:::expand(list(c('a', 'b', 'c'), c('x', 'y')))
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

#' internal function for \code{select_types()} to check if a string is a regular expression
#' @param x a character string to be tested
#' @keywords internal
is_regex <- function(x){
    any(stri_detect_fixed(x, c(".", "(", ")", "^", "{", "}", "+", "$", "*", "?", "[", "]", "\\")))
}

#' internal function for \code{select_types()} to escape regular expressions 
#' 
#' This function escapes glob patterns before \code{utils:glob2rx()}, therefore * and ?
#' are unescaped.
#' @param x character vector to be escaped
#' @keywords internal
escape_regex <- function(x){
    #stri_replace_all_regex(x, "([.()^\\{\\}+$*\\[\\]\\\\])", "\\\\$1") # escape any
    stri_replace_all_regex(x, "([.()^\\{\\}+$\\[\\]\\\\])", "\\\\$1") # allow glob
}

#' internal function for select_types() to check if regex a pattern is indexed by /code{index_types()}
#' @param x a regex pattern to be tested
#' @keywords internal
is_indexed <- function(x){
    head <- stri_startswith_fixed(x, '^')
    tail <- stri_endswith_fixed(x, '$')
    if (head && tail && !is_regex(stri_sub(x, 2, -2))) return(TRUE)
    if (head && !is_regex(stri_sub(x, 2, -1))) return(TRUE)
    if (tail && !is_regex(stri_sub(x, 1, -2))) return(TRUE)
    return(FALSE)
}


