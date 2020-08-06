#' Convert regex and glob patterns to type IDs or fixed patterns
#'
#' @description `pattern2id` converts regex or glob to type IDs to allow
#'   C++ function to perform fast searches in tokens object. C++ functions use a
#'   list of type IDs to construct a hash table, against which sub-vectors of
#'   tokens object are matched. This function constructs an index of glob
#'   patterns for faster matching.
#' @inheritParams pattern
#' @param types unique types of tokens obtained by [types()]
#' @param keep_nomatch keep patterns not found
#' @inheritParams valuetype
#' @return  `pattern2id` returns a list of integer vectors containing type
#'   IDs
#' @keywords internal
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
#' @export
pattern2id <- function(pattern, types, valuetype = c("glob", "fixed", "regex"),
                       case_insensitive = TRUE, keep_nomatch = FALSE) {
    
    if (!length(pattern)) return(list())
    
    pattern <- lapply(pattern, stri_trans_nfc) # normalize unicode
    stopifnot(is.character(types))
    valuetype <- match.arg(valuetype)
    
    # glob is treated as fixed if neither * or ? is found
    if (valuetype == "glob" && !is_glob(pattern))
        valuetype <- "fixed"
    
    len <- stri_length(unlist(pattern, use.names = FALSE))
    if (length(len)) {
        max_len <- max(len)    
    } else {
        max_len <- 0
    }
        
    # construct glob or fixed index for quick search
    index <- index_types(types, valuetype, case_insensitive, max_len)
    types_search <- attr(index, "types_search")

    # lowercases for case-insensitive search
    if (valuetype != "regex" && case_insensitive)
        pattern <- lapply(pattern, stri_trans_tolower)

    temp <- vector("list", length(pattern)) 
    for (i in seq_along(pattern)) {
        if (length(pattern[[i]]) > 1) {
            if (valuetype == "regex") {
                temp[[i]] <- search_regex_multi(pattern[[i]], types_search, case_insensitive)
            } else if (valuetype == "glob") {
                temp[[i]] <- search_glob_multi(pattern[[i]], types_search, index)
            } else {
                temp[[i]] <- search_fixed_multi(pattern[[i]], types_search, index)
            }
        } else {
            if (valuetype == "regex") {
                temp[[i]] <- as.list(search_regex(pattern[[i]], types_search, case_insensitive))
            } else if (valuetype == "glob") {
                temp[[i]] <- as.list(search_glob(pattern[[i]], types_search, index))
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
                          case_insensitive = TRUE, keep_nomatch = FALSE) {
    
    temp <- pattern2id(pattern, types, valuetype, case_insensitive, keep_nomatch)
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
search_glob <- function(pattern, types_search, index) {
    if (length(pattern) == 0)  {
        return(integer())
    } else if (pattern == "") {
        return(0L)
    } else if (pattern == "*") {
        return(seq_along(types_search)) # return all types when glob is *
    } else {
        pos <- search_index(pattern, index)
        if (length((pos))) {
            #cat("Index search", pattern, "\n")
            return(pos)
        } else if (!is_indexed(pattern)) {
            #cat("Regex search", pattern, "\n")
            return(which(stri_detect_regex(types_search, utils::glob2rx(pattern),
                                           case_insensitive = FALSE)))
        } else {
            #cat("Not found\n")
            return(integer())
        }
    }
}

#' @rdname search_glob
#' @param patterns a list of "glob", "fixed" or "regex" patterns
#' @keywords internal
search_glob_multi <- function(patterns, types_search, index) {
    expand(lapply(patterns, search_glob, types_search, index))
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
search_fixed <- function(pattern, types_search, index) {
    if (length(pattern) == 0)  {
        return(integer())
    } else if (pattern == "") {
        return(0L)
    } else {
        return(search_index(pattern, index))
    }
}

#' @rdname search_glob
#' @keywords internal
search_fixed_multi <- function(patterns, types_search, index) {
    expand(lapply(patterns, search_fixed, types_search, index))
}

#' Index types for fastest "glob" or "fixed" pattern matches
#'
#' `index_types` is an auxiliary function for `pattern2id` that
#' constructs an index of "glob" or "fixed" patterns to avoid expensive
#' sequential search. For example, a type "cars" is index by keys "cars",
#' "car?", "c*", "ca*", "car*" and "cars*" when `valuetype="glob"`.
#' @rdname pattern2id
#' @inheritParams valuetype
#' @param max_len maximum length of types to be indexed
#' @return `index_types` returns a list of integer vectors containing type
#'   IDs with index keys as an attribute
#' @keywords internal
#' @export
#' @examples
#' index <- index_types(c("xxx", "yyyy", "ZZZ"), "glob", FALSE, 3)
#' quanteda:::search_glob("yy*", attr(index, "type_search"), index)
index_types <- function(types, valuetype, case_insensitive, max_len = NULL) {

    if (is.null(types)) stop("types cannot be NULL")
    if (is.null(valuetype)) stop("valuetype cannot be NULL")
    if (is.null(case_insensitive)) stop("case_insensitive cannot be NULL")

    # normalize unicode
    types <- stri_trans_nfc(types)

    if (!valuetype %in% c("glob", "fixed", "regex"))
        stop('valuetype should be "glob", "fixed" or "regex"')
    if (valuetype == "regex" || length(types) == 0) {
        index <- list()
        attr(index, "types_search") <- types
        attr(index, "types") <- types
        attr(index, "valuetype") <- valuetype
        attr(index, "case_insensitive") <- case_insensitive
        attr(index, "key") <- character()
        return(index)
    }

    # lowercases for case-insensitive search
    if (case_insensitive) {
        types_search <- stri_trans_tolower(types)
    } else {
        types_search <- types
    }
    
    # index for fixed patterns
    pos_tmp <- seq_along(types_search)
    key_tmp <- list(types_search)

    # index for glob patterns
    if (valuetype == "glob") {
        len <- stri_length(types_search)
        id <- seq_along(types_search)
        # index all the types if max_len is unknown
        if (is.null(max_len)) max_len <- max(len)
        for (i in seq(1, max_len)) {
            k <- id[len >= i]
            # index for patterns with * at the end
            pos_tmp <- c(pos_tmp, list(k))
            key_tmp <- c(key_tmp,
                         list(stri_c(stri_sub(types_search[k], 1, i), "*")))
            # # index for patterns with * at the top or end
            #pos_tmp <- c(pos_tmp, list(rep(k, 2)))
            #key_tmp <- c(key_tmp, list(stri_c(stri_sub(types_search[k], 1, i), "*")))
            #key_tmp <- c(key_tmp, list(stri_c("*", stri_sub(types_search[k], i * -1, -1))))
        }
    
        l <- id[len >= 2]
        # index for patterns with ? at the end
        pos_tmp <- c(pos_tmp, list(l))
        key_tmp <- c(key_tmp, list(stri_c(stri_sub(types_search[l], 1, -2), "?")))
        # # index for patterns with ? at the top or end
        # pos_tmp <- c(pos_tmp, list(rep(l, 2)))
        # key_tmp <- c(key_tmp, list(stri_c(stri_sub(types_search[l], 1, -2), "?")))
        # key_tmp <- c(key_tmp, list(stri_c("?", stri_sub(types_search[l], 2, -1))))
    }

    # faster to join vectors in the end
    key <- unlist(key_tmp, use.names = FALSE)
    pos <- unlist(pos_tmp, use.names = FALSE)
    # set factor for quick split
    index <- split(pos, factor(key, ordered = FALSE, levels = unique(key)))
    key <- names(index)

    attr(index, "names") <- NULL # names attribute slows down
    attr(index, "types_search") <- types_search
    attr(index, "types") <- types
    attr(index, "valuetype") <- valuetype
    attr(index, "case_insensitive") <- case_insensitive
    attr(index, "key") <- key

    return(index)
}

#' Internal function for `select_types` to search the index using
#' fastmatch.
#' @param regex a glob expression to search
#' @param index an index object created by `index_types`
#' @seealso [index_types()]
#' @keywords internal
 search_index <- function(pattern, index){
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
#' Internal function for `select_types` to check if a glob pattern is
#' indexed by
#' `index_types`.
#' @param pattern a glob pattern to be tested
#' @keywords internal
is_indexed <- function(pattern) {
    pattern <- stri_sub(pattern, 1, -2)
    if (pattern == "") {
        return(FALSE)
    } else {
        # check index for patterns with ? or * at the end
        return(!any(stri_detect_fixed(pattern, c("*", "?"))))
    
        # # check index for patterns with ? or * at the top or end
        #return(!any(stri_detect_fixed(stri_sub(pattern, 1, -2), c("*", "?"))) ||
        #       !any(stri_detect_fixed(stri_sub(pattern, 2, -1), c("*", "?"))))
    }
}

#' Check if patterns contains glob wildcard
#' @param pattern a glob pattern to be tested
#' @keywords internal
is_glob <- function(pattern) {
    pattern <- unlist(pattern, use.names = FALSE)
    return(any(stri_detect_fixed(pattern, "*")) || any(stri_detect_fixed(pattern, "?")))
}

#' Unlist a list of integer vectors safely
#' @param x a list of integers
#' @param unique if `TURE` remove duplicated elements
#' @param ... passed to `unlist`
#' @keywords internal
#' @return integer vector
unlist_integer <- function(x, unique = FALSE, ...) {
    stopifnot(all(unlist(lapply(x, typeof), use.names = FALSE) == "integer"))
    result <- integer()
    if (!length(x))
        return(result)
    result <- unlist(x, ...)
    if (unique)
        result <- unique(result)
    return(result)
}

#' Unlist a list of character vectors safely
#' @param x a list of integers
#' @param unique if `TURE` remove duplicated elements
#' @param ... passed to `unlist`
#' @keywords internal
#' @return character vector
unlist_character <- function(x, unique = FALSE, ...) {
    stopifnot(all(unlist(lapply(x, typeof), use.names = FALSE) == "character"))
    result <- character()
    if (!length(x))
        return(result)
    result <- unlist(x, ...)
    if (unique)
        result <- unique(result)
    return(result)
}



# internal-only aliases for backward compatibility
# TODO: this should be removed with in a year (by April 2019).
regex2id <- pattern2id
regex2fixed <- pattern2fixed

