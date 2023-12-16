
#' Match quanteda objects against token types
#'
#' Developer function to match patterns in quanteda objects against token types.
#' @param x a list of character vectors, [dictionary] or collocations object
#' @inheritParams valuetype
#' @inheritParams pattern2id
#' @param concatenator the concatenation character that joins multi-word
#'   expression in `types`
#' @inheritParams tokens_lookup
#' @param remove_unigram  if `TRUE`, ignores single-word patterns
#' @return `object2fixed()` returns a list of character vectors of matched
#'   types. `object2id()` returns a list of indices of matched types with
#'   attributes. The "pattern" attribute records the indices of the matched patterns
#'   in `x`; the "key" attribute records the keys of the matched patterns when `x` is
#'   [dictionary].
#' @seealso [pattern2id()]
#' @keywords development internal
#' @export
object2id <- function(x, types, valuetype = c("glob", "fixed", "regex"),
                      case_insensitive = TRUE,
                      concatenator = "_", levels = 1, remove_unigram = FALSE,
                      keep_nomatch = FALSE) {
    
    if (is.dfm(x))
        stop("dfm cannot be used as pattern")
    
    types <- check_character(types, min_len = 0, max_len = Inf, strict = TRUE)
    valuetype <- match.arg(valuetype)
    case_insensitive <- check_logical(case_insensitive)
    concatenator <- check_character(concatenator)
    levels <- check_integer(levels, min = 1, max_len = Inf)
    remove_unigram <- check_logical(remove_unigram)
    keep_nomatch <- check_logical(keep_nomatch)
    
    if (is.collocations(x)) {
        if (nrow(x) == 0) return(list())
        temp <- stri_split_charclass(x$collocation, "\\p{Z}")
        names(temp) <- x$collocation
        if (case_insensitive) {
            result <- pattern2id(temp, types, valuetype = "fixed", 
                                 case_insensitive = TRUE)
        } else {
            temp <- lapply(temp, function(x) fastmatch::fmatch(x, types))
            result <- temp[unlist(lapply(temp, function(x) all(!is.na(x))), use.names = FALSE)]
        }
        attr(result, "pattern") <- match(names(result), names(temp))
    } else {
        if (length(x) == 0) return(list())
        if (is.dictionary(x)) {
            x <- as.dictionary(x)
            temp <- flatten_list(x, levels)
            key <- names(temp)
            temp <- split_values(temp, " ", concatenator)
        } else if (is.list(x)) {
            x <- lapply(x, function(x) check_character(x, min_len = 0, max_len = Inf, strict = TRUE))
            temp <- x
            names(temp) <- stri_c_list(x, " ")
        } else {
            x <- check_character(x, min_len = 0, max_len = Inf, strict = TRUE)
            temp <- as.list(x)
            names(temp) <- x
        }
        if (remove_unigram)
            temp <- temp[lengths(temp) > 1] # drop single-word patterns
        result <- pattern2id(temp, types, valuetype, case_insensitive, keep_nomatch)
        attr(result, "pattern") <- match(names(result), names(temp)) # TODO might need to return index in dictionary
        if (is.dictionary(x))
            attr(result, "key") <- key
    }
    return(result)
}

#' @rdname object2id
#' @keywords development internal
#' @export
#' @examples
#' types <- c("A", "AA", "B", "BB", "B_B", "C", "C-C")
#' 
#' # dictionary
#' dict <- dictionary(list(A = c("a", "aa"), 
#'                         B = c("BB", "B B"),
#'                         C = c("C", "C-C")))
#' object2fixed(dict, types)
#' object2fixed(dict, types, remove_unigram = TRUE)
#' 
#' # phrase
#' pats <- phrase(c("a", "aa", "zz", "bb", "b b"))
#' object2fixed(pats, types)
#' object2fixed(pats, types, keep_nomatch = TRUE)
object2fixed <- function(x, types, valuetype = c("glob", "fixed", "regex"),
                                  case_insensitive = TRUE,
                                  concatenator = "_", levels = 1, remove_unigram = FALSE,
                                  keep_nomatch = FALSE) {
    
    temp <- object2id(x, types, valuetype, case_insensitive, concatenator,
                      levels, remove_unigram, keep_nomatch)
    result <- lapply(temp, function(x) types[x])
    return(result)
}

# temporary duplication to avoid breaking existing packages (quanteda.textstats)
pattern2list <- object2id
