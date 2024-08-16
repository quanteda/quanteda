#' Match patterns against token types
#'
#' Developer function to match regex, fixed or glob patterns against token
#' types. This allows C++ function to perform fast searches in tokens object.
#' C++ functions use a list of type IDs to construct a hash table, against which
#' sub-vectors of tokens object are matched. This function constructs an index
#' of glob patterns for faster matching.
#' @inheritParams pattern
#' @param types token types against which patterns are matched
#' @param keep_nomatch keep patterns that did not match
#' @param use_index construct index of types for quick search
#' @inheritParams valuetype
#' @return a list of integer vectors containing indices of matched types
#' @keywords development
#' @export
#' @examples
#' types <- c("A", "AA", "B", "BB", "BBB", "C", "CC")
#' 
#' pats_regex <- list(c("^a$", "^b"), c("c"), c("d"))
#' pattern2id(pats_regex, types, "regex", case_insensitive = TRUE)
#'
#' pats_glob <- list(c("a*", "b*"), c("c"), c("d"))
#' pattern2id(pats_glob, types, "glob", case_insensitive = TRUE)
#' 
#' @importFrom stringi stri_endswith_fixed
pattern2id <- function(pattern, types, valuetype = c("glob", "fixed", "regex"),
                       case_insensitive = TRUE, keep_nomatch = FALSE,
                       use_index = TRUE) {
    
    types <- check_character(types, min_len = 0, max_len = Inf, strict = TRUE)
    valuetype <- match.arg(valuetype)
    case_insensitive <- check_logical(case_insensitive)
    keep_nomatch <- check_logical(keep_nomatch)
    
    if (!length(pattern)) return(list())
    
    # normalize unicode
    pattern <- lapply(pattern, stri_trans_nfc) 
    types <- stri_trans_nfc(types)
    
    # glob is treated as fixed if neither * or ? is found
    if (valuetype == "glob" && !any(is_glob(pattern)))
        valuetype <- "fixed"
    
    # lowercases for case-insensitive search
    if (valuetype != "regex" && case_insensitive)
        pattern <- lapply(pattern, stri_trans_tolower)
    
    # construct glob or fixed index for quick search
    if (use_index) {
        index <- index_types(pattern, types, valuetype, case_insensitive)
        types_search <- attr(index, "types_search")
    } else {
        index <- NULL
        types_search <- types
    }

    temp <- vector("list", length(pattern)) 
    for (i in seq_along(pattern)) {
        if (length(pattern[[i]]) > 1) {
            if (valuetype == "regex") {
                temp[[i]] <- search_regex_multi(pattern[[i]], types_search, case_insensitive)
            } else if (valuetype == "glob") {
                temp[[i]] <- search_glob_multi(pattern[[i]], types_search, case_insensitive, index)
            } else {
                temp[[i]] <- search_fixed_multi(pattern[[i]], types_search, index)
            }
        } else {
            if (valuetype == "regex") {
                temp[[i]] <- as.list(search_regex(pattern[[i]], types_search, case_insensitive))
            } else if (valuetype == "glob") {
                temp[[i]] <- as.list(search_glob(pattern[[i]], types_search, case_insensitive, index))
            } else {
                temp[[i]] <- as.list(search_fixed(pattern[[i]], types_search, index))
            }
        }
    }
    if (keep_nomatch) #{
        temp <- lapply(temp, function(x) if (!length(x)) list(integer()) else x)
    result <- unlist(temp, FALSE, FALSE)
    names(result) <- rep(names(pattern), lengths(temp))
    return(result)
}

#' @rdname pattern2id
#' @description `pattern2fixed` converts regex and glob patterns to fixed patterns.
#' @inherit pattern2id
#' @return `pattern2fixed` returns a list of character vectors containing
#'   types
#' @keywords internal
#' @export
#' @examples
#' pattern <- list(c("^a$", "^b"), c("c"), c("d"))
#' types <- c("A", "AA", "B", "BB", "BBB", "C", "CC")
#' pattern2fixed(pattern, types, "regex", case_insensitive = TRUE)
pattern2fixed <- function(pattern, types, valuetype = c("glob", "fixed", "regex"),
                          case_insensitive = TRUE, keep_nomatch = FALSE,
                          use_index = TRUE) {
    
    temp <- pattern2id(pattern, types, valuetype, case_insensitive, keep_nomatch,
                       use_index = use_index)
    result <- lapply(temp, function(x) types[x])
    return(result)
}


#' Select types without performing slow regex search
#' 
#' This is an internal function for `pattern2id` that select types using
#' keys in index when available.
#' @param pattern a "glob", "fixed" or "regex" pattern
#' @param types_search lowercased types when `case_insensitive=TRUE`, but
#'   not used in glob and fixed matching as types are in the index.
#' @inheritParams valuetype
#' @param index index object created by `index_types`
#' @keywords internal
search_glob <- function(pattern, types_search, case_insensitive, index = NULL) {
    if (length(pattern) == 0)  {
        return(integer())
    } else if (pattern == "") {
        return(0L)
    } else if (pattern == "*") {
        return(seq_along(types_search)) # return all types when glob is *
    } else if (is.null(index)) {
        return(which(stri_detect_regex(types_search, utils::glob2rx(pattern),
                                       case_insensitive = case_insensitive)))
    } else {
        if (is_indexed(pattern)) {
            return(search_index(pattern, index))
        } else {
            l <- stri_detect_regex(types_search, utils::glob2rx(pattern),
                                   case_insensitive = case_insensitive)
            return(which(l))
        }
    }
}

#' @rdname search_glob
#' @param patterns a list of "glob", "fixed" or "regex" patterns
#' @keywords internal
search_glob_multi <- function(patterns, types_search, case_insensitive, index) {
    expand(lapply(patterns, search_glob, types_search, case_insensitive, index))
}

#' @rdname search_glob
#' @keywords internal
search_regex <- function(pattern, types_search, case_insensitive) {
    if (length(pattern) == 0)  {
        return(integer())
    } else if (pattern == "") {
        return(0L)
    } else {
        return(which(stri_detect_regex(types_search, pattern,
                                       case_insensitive = case_insensitive)))
    }
}

#' @rdname search_glob
#' @keywords internal
search_regex_multi <- function(patterns, types_search, case_insensitive) {
    expand(lapply(patterns, search_regex, types_search, case_insensitive))
}

#' @rdname search_glob
#' @keywords internal
search_fixed <- function(pattern, types_search, index = NULL) {
    if (length(pattern) == 0)  {
        return(integer())
    } else if (pattern == "") {
        return(0L)
    } else if (is.null(index)) {
        return(which(types_search %in% pattern))
    } else {
        return(search_index(pattern, index))
    }
}

#' @rdname search_glob
#' @keywords internal
search_fixed_multi <- function(patterns, types_search, index) {
    expand(lapply(patterns, search_fixed, types_search, index))
}

#' @description
#' `index_types` is an internal function for `pattern2id` that
#' constructs an index of "glob" or "fixed" patterns to avoid expensive
#' sequential search.
#' @rdname search_glob
#' @inheritParams valuetype
#' @return `index_types` returns a list of integer vectors containing type
#'   IDs with index keys as an attribute
#' @keywords internal
#' @examples
#' index <- quanteda:::index_types("yy*", c("xxx", "yyyy", "ZZZ"), "glob", FALSE)
#' quanteda:::search_glob("yy*", attr(index, "types_search"), index)
index_types <- function(pattern, types, valuetype = c("glob", "fixed", "regex"), 
                        case_insensitive = TRUE) {
    
    pattern <- unlist_character(pattern, use.names = FALSE)
    types <- check_character(types, min_len = 0, max_len = Inf, strict = TRUE)
    valuetype <- match.arg(valuetype)
    
    # lowercase for case-insensitive search
    if (valuetype != "regex" && case_insensitive) {
        # NOTE: disabled for regex to keep conrol character (#2407)
        types_search <- stri_trans_tolower(types)
    } else {
        types_search <- types
    }
    
    if (valuetype == "regex" || length(types) == 0) {
        index <- list()
        attr(index, "types_search") <- types_search
        attr(index, "types") <- types
        attr(index, "valuetype") <- valuetype
        attr(index, "case_insensitive") <- case_insensitive
        attr(index, "key") <- character()
        return(index)
    }

    index <- cpp_index_types(pattern, types_search, valuetype == "glob")
    index <- index[lengths(index) > 0]
    
    attr(index, "types_search") <- types_search
    attr(index, "types") <- types
    attr(index, "valuetype") <- valuetype
    attr(index, "case_insensitive") <- case_insensitive
    attr(index, "key") <- attr(index, "names")
    attr(index, "names") <- NULL # names attribute slows down
    
    return(index)
}

#' Internal function for `select_types` to search the index using
#' fastmatch.
#' @param index an index object created by `index_types`
#' @seealso [index_types()]
#' @keywords internal
 search_index <- function(pattern, index) {
    # use fmatch instead of names for quick access
    result <- index[[fastmatch::fmatch(pattern, attr(index, "key"))]]
    if (is.null(result))
        result <- integer()
    return(result)
}

#' Simpler and faster version of expand.grid() in base package
#' @param elem list of elements to be combined
#' @keywords internal
#' @examples
#' quanteda:::expand(list(c("a", "b", "c"), c("x", "y")))
expand <- function(elem){
    m <- prod(lengths(elem))
    comb <- vector("list", m)
    if (m == 0) return(comb)
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


#' Check if a glob pattern is indexed by index_types
#' 
#' Internal function for `select_types` to check if a glob pattern is indexed by
#' `index_types`.
#' @param pattern a glob pattern to be tested
#' @keywords internal
is_indexed <- function(pattern) {
    
    # without wildcard
    if (!any(stri_detect_fixed(pattern, c("*", "?"))))
        return(TRUE)
    
    # wildcard only on the right
    if (any(stri_sub(pattern, -1, -1) == c("*", "?")) && 
        !any(stri_detect_fixed(stri_sub(pattern, 1, -2), c("*", "?"))))
        return(TRUE)
    
    # wildcard only on the left
    if (any(stri_sub(pattern, 1, 1) == c("*", "?")) && 
        !any(stri_detect_fixed(stri_sub(pattern, 2, -1), c("*", "?"))))
        return(TRUE)
    
    return(FALSE)
}

#' Check if patterns contains glob wildcard
#' @param pattern a glob pattern to be tested
#' @keywords internal
is_glob <- function(pattern) {
    pat <- unlist_character(pattern, use.names = FALSE)
    return(any(stri_detect_fixed(pat, "*")) || any(stri_detect_fixed(pat, "?")))
}

#' Unlist a list of integer vectors safely
#' @param x a list of integers
#' @param unique if `TURE` remove duplicated elements
#' @param ... passed to `unlist`
#' @keywords internal
#' @return integer vector
unlist_integer <- function(x, unique = FALSE, ...) {
    result <- unlist(x, ...)
    if (unique)
        result <- unique(result)
    if (is.null(result)) {
        return(integer())
    } else {
        return(result)
    }
}

#' Unlist a list of character vectors safely
#' @param x a list of integers
#' @param unique if `TRUE` remove duplicated elements
#' @param ... passed to `unlist`
#' @keywords internal
#' @return character vector
unlist_character <- function(x, unique = FALSE, ...) {
    result <- unlist(x, ...)
    if (unique)
        result <- unique(result)
    if (is.null(result)) {
        return(character())
    } else {
        return(result)
    }
}
