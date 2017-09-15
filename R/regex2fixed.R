#' @rdname regex2id
#' @description \code{regex2fixed} converts regex and glob patterns to fixed patterns.
#' @inherit regex2id
#' @return \code{regex2fixed} returns a list of character vectors containing
#'   types
#' @keywords internal
#' @examples
#' pattern <- list(c('^a$', '^b'), c('c'), c('d'))
#' types <- c('A', 'AA', 'B', 'BB', 'BBB', 'C', 'CC')
#' quanteda:::regex2fixed(pattern, types, 'regex', case_insensitive = TRUE)
#' index <- quanteda:::index_types(types, 'regex', case_insensitive = TRUE)
#' quanteda:::regex2fixed(pattern, index = index)
regex2fixed <- function(pattern, types = NULL, valuetype = NULL, case_insensitive = NULL, index = NULL){
    id <- regex2id(pattern, types, valuetype, case_insensitive, index)
    fixed <- lapply(id, function(x) types[x])
    return(fixed)
}

#' convert regex and glob patterns to type IDs or fixed patterns
#'
#' @description \code{regex2id} converts regex or glob to type IDs to allow C++
#'   function to perform fast searches in tokens object. C++ functions use a
#'   list of type IDs to construct a hash table, agaist which sub-vectors of
#'   tokens object are matched.
#' @inheritParams pattern
#' @param types unique types of tokens obtaine by \code{quanteda:::types()}
#' @inheritParams valuetype
#' @param case_insensitive ignore case when matching, if \code{TRUE}
#' @param index If TRUE, index is constructed automatically. It also accept
#'   index constructed by index_types().
#' @return  \code{regex2id} returns a list of integer vectors containing type
#'   IDs
#' @keywords internal
#' @examples
#' pattern <- list(c('^a$', '^b'), c('c'), c('d'))
#' types <- c('A', 'AA', 'B', 'BB', 'BBB', 'C', 'CC')
#' quanteda:::regex2id(pattern, types, 'regex', case_insensitive = TRUE)
regex2id <- function(pattern, types = NULL, valuetype = NULL, case_insensitive = NULL, index = NULL) {
    
    if (!length(pattern)) return(list())
    
    if (!is.null(index)) {
        if (!is.null(types)) stop('types must be NULL when index is provided')
        if (!is.null(valuetype)) stop('valuetype must be NULL when index is provided')
        if (!is.null(case_insensitive)) stop('case_insensitive must be NULL when index is provided')
        valuetype <- attr(index, 'valuetype')
        case_insensitive <- attr(index, 'case_insensitive')
    }

    # normalize and convert patterns
    pattern <- lapply(pattern, stri_trans_nfc)
    
    if (case_insensitive)
        pattern <- lapply(pattern, stri_trans_tolower)
    
    if (is.null(index)) {
        # glob is treated as fixed if neither * or ? is found
        pattern_unlist <- unlist(pattern, use.names = FALSE)
        if (valuetype == 'glob' &&
            !any(stri_detect_fixed(pattern_unlist, '*')) &&
            !any(stri_detect_fixed(pattern_unlist, '?'))) {
            valuetype <- 'fixed'
        }
    }
    
    if (valuetype == 'glob') {
        regex <- lapply(pattern, function(x) utils::glob2rx(escape_regex(x)))
    } else {
        regex <- pattern
    }
    
    # index types for quick search
    if (is.null(index)) {
        max_len <- max(stri_length(unlist(regex, use.names = FALSE)))
        index <- index_types(types, valuetype, case_insensitive, max_len)
    }
    
    # extract values from index attributes
    types_search <- attr(index, 'types_search')
    exact <- attr(index, 'exact')
    
    # separate multi and single-entry patterns
    len <- lengths(regex)
    pats_multi <- regex[len > 1] 
    pats_single <- regex[len == 1]
    
    # process multi-entry patterns
    id <- list()
    for (pat_multi in pats_multi) {
        id_multi <- select_types(pat_multi, types_search, exact, index)
        id <- c(id, expand(id_multi))
    }
    
    # process single-entry patterns
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
#' An internal function for \code{\link{regex2id}} that constructs an index of
#' regex patterns (e.g. \code{^xxxx}, \code{xxxx$} and \code{^xxxx$}) to avoid
#' expensive sequential search by \link[stringi]{stri_detect_regex}.
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
    
    if (is.null(types)) stop('types cannot be NULL')
    if (is.null(valuetype)) stop('valuetype cannot be NULL')
    if (is.null(case_insensitive)) stop('case_insensitive cannot be NULL')
    
    # normalize unicode
    types <- stri_trans_nfc(types)
    
    if (case_insensitive) {
        types_search <- stri_trans_tolower(types)
    } else {
        types_search <- types
    }

    if (valuetype == 'fixed') {
        exact <- TRUE
    } else {
        exact <- FALSE
    }
    
    # create regex patterns from types
    if (exact) { 
        types_index <- types_search
    } else {
        types_index <- stri_c("^", types_search, "$")
    }
    
    # index for regex patterns of ^xxxx$ 
    pos_tmp <- seq_along(types_index)
    key_tmp <- list(types_index)

    # index for regex patterns of ^xxxx and xxxx$
    if (!exact) {
        len <- stri_length(types_index)
        if (is.null(max_len)) max_len <- max(len) # index all the types if max_len is unknown
        for (i in 2:max_len) {
            k <- which(len > i)
            pos_tmp <- c(pos_tmp, list(rep(k, 2)))
            key_tmp <- c(key_tmp, list(stri_sub(types_index[k], 1, i)))
            key_tmp <- c(key_tmp, list(stri_sub(types_index[k], i * -1, -1)))
        }
    }
    
    # faster to join vectors in the end
    key <- unlist(key_tmp, use.names = FALSE) 
    pos <- unlist(pos_tmp, use.names = FALSE)
    index <- split(pos, factor(key, ordered = FALSE, levels = unique(key))) # set factor for quick split
    
    attr(index, 'key') <- names(index)
    attr(index, 'types') <- types
    attr(index, 'types_search') <- types_search
    attr(index, 'valuetype') <- valuetype
    attr(index, 'case_insensitive') <- case_insensitive
    attr(index, 'exact') <- exact
    attr(index, 'names') <- NULL # names attribute slows down
    
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


