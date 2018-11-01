#' Convert regex and glob patterns to type IDs or fixed patterns
#'
#' @description \code{pattern2id} converts regex or glob to type IDs to allow
#'   C++ function to perform fast searches in tokens object. C++ functions use a
#'   list of type IDs to construct a hash table, against which sub-vectors of
#'   tokens object are matched. This function constructs an index of glob
#'   patterns for faster matching.
#' @inheritParams pattern
#' @param types unique types of tokens obtained by \code{\link{types}}
#' @inheritParams valuetype
#' @param case_insensitive if \code{TRUE}, ignores case when matching
#' @param index If \code{NULL}, index is constructed automatically. It also
#'   accept index constructed by \code{index_types}. In that case, \code{types},
#'   \code{valuetype} and \code{case_insensitive} should be NULL.
#' @return  \code{pattern2id} returns a list of integer vectors containing type
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
pattern2id <- function(pattern, types = NULL, valuetype = NULL,
                       case_insensitive = NULL, index = NULL) {
    
    
    if (!length(pattern)) return(list())
    pattern <- lapply(pattern, stri_trans_nfc) # normalize unicode
    
    if (is.null(index)) {
        if (is.null(types)) stop("types cannot be NULL when index is not provided")
        if (is.null(valuetype)) stop("valuetype cannot be NULL when index is not provided")
        if (is.null(case_insensitive)) stop("case_insensitive cannot be NULL when index is not provided")
        if (!is.character(types)) stop("types must be a character vector")
        
        # glob is treated as fixed if neither * or ? is found
        pattern_unlist <- unlist(pattern, use.names = FALSE)
        if (!valuetype %in% c("glob", "fixed", "regex"))
            stop('valuetype should be "glob", "fixed" or "regex"')
        if (valuetype == "glob" &&
            !any(stri_detect_fixed(pattern_unlist, "*")) &&
            !any(stri_detect_fixed(pattern_unlist, "?"))) {
            valuetype <- "fixed"
        }
        max_len <- max(stri_length(unlist(pattern, use.names = FALSE)))
        index <- index_types(types, valuetype, case_insensitive, max_len) # index types for quick search
    } else {
        if (!is.null(types)) stop("types must be NULL when index is provided")
        if (!is.null(valuetype)) stop("valuetype must be NULL when index is provided")
        if (!is.null(case_insensitive)) stop("case_insensitive must be NULL when index is provided")
    }

    # use options in the index
    types_search <- attr(index, "types_search")
    valuetype <- attr(index, "valuetype")
    case_insensitive <- attr(index, "case_insensitive")

    # lowercases for case-insensitive search
    if (valuetype != "regex" && case_insensitive) {
        pattern <- lapply(pattern, stri_trans_tolower)
    }

    # separate multi and single-entry patterns
    len <- lengths(pattern)
    pats_multi <- pattern[len > 1]
    pats_single <- pattern[len == 1]

    # process multi-entry patterns
    ids <- list()
    for (pat_multi in pats_multi) {
        if (valuetype == "regex") {
            ids_multi <- search_regex(pat_multi, types_search, case_insensitive)
        } else if (valuetype == "glob") {
            ids_multi <- search_glob(pat_multi, types_search, index)
        } else {
            ids_multi <- search_fixed(pat_multi, types_search, index)
        }
        ids <- c(ids, expand(ids_multi))
    }

    # process single-entry patterns
    if (length(pats_single) > 0) {
        pats_single <- unlist(pats_single, use.names = FALSE)
        if (valuetype == "regex") {
            ids_single <- unlist(search_regex(pats_single, types_search, case_insensitive), use.names = FALSE)
        } else if (valuetype == "glob") {
            ids_single <- unlist(search_glob(pats_single, types_search, index), use.names = FALSE)
        } else {
            ids_single <- unlist(search_fixed(pats_single, types_search, index), use.names = FALSE)
        }
        ids <- c(ids, ids_single)
    }
    return(unique(ids))
}


#' @rdname pattern2id
#' @description \code{pattern2fixed} converts regex and glob patterns to fixed patterns.
#' @inherit pattern2id
#' @return \code{pattern2fixed} returns a list of character vectors containing
#'   types
#' @keywords internal
#' @export
#' @examples
#' pattern <- list(c("^a$", "^b"), c("c"), c("d"))
#' types <- c("A", "AA", "B", "BB", "BBB", "C", "CC")
#' pattern2fixed(pattern, types, "regex", case_insensitive = TRUE)
#' index <- index_types(types, "regex", case_insensitive = TRUE)
#' pattern2fixed(pattern, index = index)
pattern2fixed <- function(pattern, types = NULL, valuetype = NULL,
                          case_insensitive = NULL, index = NULL) {
    id <- pattern2id(pattern, types, valuetype, case_insensitive, index)
    if (!is.null(index))
        types <- attr(index, "types")
    fixed <- lapply(id, function(x) types[x])
    return(fixed)
}


#' Select types without performing slow regex search
#' 
#' This is an internal function for \code{pattern2id} that select types using
#' keys in index when available.
#' @param patterns a list of "glob", "fixed" or "regex" patterns
#' @param types_search lowercased types when \code{case_insensitive=TRUE}, but
#'   not used in glob and fixed matching as types are in the index.
#' @param case_insensitive ignore case when matching, if \code{TRUE}, but not
#'   used in glob and fixed matching as types are lowercased in the index.
#' @param index index object created by \code{index_types}
#' @keywords internal
search_glob <- function(patterns, types_search, index) {
    lapply(patterns, function(pattern, types_search, index) {
        if (pattern == "") {
            return(integer()) # return nothing for empty pattern
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
    }, types_search, index)
}

#' @rdname search_glob
#' @keywords internal
search_regex <- function(patterns, types_search, case_insensitive) {
    lapply(patterns, function(pattern, types_search, case_insensitive) {
        if (pattern == "") {
            return(integer())
        } else {
            return(which(stri_detect_regex(types_search, pattern,
                                           case_insensitive = case_insensitive)))
        }
    }, types_search, case_insensitive)
}

#' @rdname search_glob
#' @keywords internal
search_fixed <- function(patterns, types_search, index) {
    lapply(patterns, function(pattern, types_search, index) {
        if (pattern == "") {
            return(integer())
        } else {
            return(search_index(pattern, index))
        }
    }, types_search, index)
}

#' Index types for fastest "glob" or "fixed" pattern matches
#'
#' \code{index_types} is an auxiliary function for \code{pattern2id} that
#' constructs an index of "glob" or "fixed" patterns to avoid expensive
#' sequential search. For example, a type "cars" is index by keys "cars",
#' "car?", "c*", "ca*", "car*" and "cars*" when \code{valuetype="glob"}.
#' @rdname pattern2id
#' @inheritParams valuetype
#' @param max_len maximum length of types to be indexed
#' @return \code{index_types} returns a list of integer vectors containing type
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

#' Internal function for \code{select_types} to search the index using
#' fastmatch.
#' @param regex a glob expression to search
#' @param index an index object created by \code{index_types}
#' @seealso \code{\link{index_types}}
#' @keywords internal
search_index <- function(pattern, index){
    # use fmatch instead of names for quick access
    index[[fastmatch::fmatch(pattern, attr(index, "key"))]]
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
#' Internal function for \code{select_types} to check if a glob pattern is
#' indexed by
#' \code{index_types}.
#' @param x a glob pattern to be tested
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

# internal-only aliases for backward compatibility
# TODO: this should be removed with in a year (by April 2019).
regex2id <- pattern2id
regex2fixed <- pattern2fixed

