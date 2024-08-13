#' Apply a dictionary to a tokens object
#'
#' Convert tokens into equivalence classes defined by values of a dictionary
#' object.
#' @param x the [tokens] object to which the dictionary will be applied
#' @param dictionary the [dictionary]-class object that will be applied to
#'   `x`
#' @param levels integers specifying the levels of entries in a hierarchical
#'   dictionary that will be applied.  The top level is 1, and subsequent levels
#'   describe lower nesting levels.  Values may be combined, even if these
#'   levels are not contiguous, e.g. `levels = c(1:3)` will collapse the second
#'   level into the first, but record the third level (if present) collapsed
#'   below the first (see examples).
#' @inheritParams valuetype
#' @inheritParams messages
#' @param capkeys if `TRUE`, convert dictionary keys to uppercase to distinguish
#'   them from unmatched tokens.
#' @param nomatch an optional character naming a new key for tokens that do not
#'   matched to a dictionary values  If `NULL` (default), do not record
#'   unmatched tokens.
#' @param exclusive if `TRUE`, remove all features not in dictionary,
#'   otherwise, replace values in dictionary with keys while leaving other
#'   features unaffected.
#' @param nested_scope how to treat matches from different dictionary keys that
#'   are nested.  When one value is nested within another, such as "a b" being
#'   nested within "a b c", then `tokens_lookup()` will match the longer.  When
#'   `nested_scope = "key"`, this longer-match priority is applied only
#'   within the key, while `"dictionary"` applies it across keys, matching only
#'   the key with the longer pattern, not the matches nested within that longer
#'   pattern from other keys.  See Details.
#' @param append_key if `TRUE`, annotate matched tokens with keys.
#' @param separator a character to separate tokens and keys when `append_key = TRUE`.
#' @param concatenator the concatenation character that will connect the words
#'   making up the multi-word sequences.
#' @inheritParams apply_if
#' @details Dictionary values may consist of sequences, and there are different
#'   methods of counting key matches based on values that are nested or that
#'   overlap.
#'
#'   When two different keys in a dictionary are nested matches of one another,
#'   the `nested_scope` options provide the choice of matching each key's
#'   values independently (the `"key"`) option, or just counting the
#'   longest match (the `"dictionary"` option).  Values that are nested
#'   *within* the same key are always counted as a single match.  See the
#'   last example below comparing the *New York* and *New York Times*
#'   for these two different behaviours.
#'
#'   *Overlapping values*, such as `"a b"` and `"b a"` are
#'   currently always considered as separate matches if they are in different
#'   keys, or as one match if the overlap is within the same key.
#'   
#'   Note: `apply_if` This applies the dictionary lookup only to documents that
#'   match the logical condition.  When `exclusive = TRUE` (the default),
#'   however, this means that empty documents will be returned for those not
#'   meeting the condition, since no lookup will be applied and hence no tokens
#'   replaced by matching keys.
#' 
#' @keywords tokens
#' @seealso tokens_replace
#' @examples
#' toks1 <- tokens(data_corpus_inaugural)
#' dict1 <- dictionary(list(country = "united states",
#'                    law=c("law*", "constitution"),
#'                    freedom=c("free*", "libert*")))
#' dfm(tokens_lookup(toks1, dict1, valuetype = "glob", verbose = TRUE))
#' dfm(tokens_lookup(toks1, dict1, valuetype = "glob", verbose = TRUE, nomatch = "NONE"))
#'
#' dict2 <- dictionary(list(country = "united states",
#'                        law = c("law", "constitution"),
#'                        freedom = c("freedom", "liberty")))
#' # dfm(applyDictionary(toks1, dict2, valuetype = "fixed"))
#' dfm(tokens_lookup(toks1, dict2, valuetype = "fixed"))
#'
#' # hierarchical dictionary example
#' txt <- c(d1 = "The United States has the Atlantic Ocean and the Pacific Ocean.",
#'          d2 = "Britain and Ireland have the Irish Sea and the English Channel.")
#' toks2 <- tokens(txt)
#' dict3 <- dictionary(list(US = list(Countries = c("States"),
#'                                   oceans = c("Atlantic", "Pacific")),
#'                         Europe = list(Countries = c("Britain", "Ireland"),
#'                                       oceans = list(west = "Irish Sea",
#'                                                     east = "English Channel"))))
#' tokens_lookup(toks2, dict3, levels = 1)
#' tokens_lookup(toks2, dict3, levels = 2)
#' tokens_lookup(toks2, dict3, levels = 1:2)
#' tokens_lookup(toks2, dict3, levels = 3)
#' tokens_lookup(toks2, dict3, levels = c(1,3))
#' tokens_lookup(toks2, dict3, levels = c(2,3))
#'
#' # show unmatched tokens
#' tokens_lookup(toks2, dict3, nomatch = "_UNMATCHED")
#'
#' # nested matching differences
#' dict4 <- dictionary(list(paper = "New York Times", city = "New York"))
#' toks4 <- tokens("The New York Times is a New York paper.")
#' tokens_lookup(toks4, dict4, nested_scope = "key", exclusive = FALSE)
#' tokens_lookup(toks4, dict4, nested_scope = "dictionary", exclusive = FALSE)
#'
#' @export
tokens_lookup <- function(x, dictionary, levels = 1:5,
                          valuetype = c("glob", "regex", "fixed"),
                          case_insensitive = TRUE,
                          capkeys = !exclusive,
                          exclusive = TRUE,
                          nomatch = NULL,
                          append_key = FALSE,
                          separator = "/",
                          concatenator = concat(x),
                          nested_scope = c("key", "dictionary"),
                          apply_if = NULL,
                          verbose = quanteda_options("verbose")) {
    UseMethod("tokens_lookup")
}

#' @export
tokens_lookup.default <- function(x, dictionary, levels = 1:5,
                                 valuetype = c("glob", "regex", "fixed"),
                                 case_insensitive = TRUE,
                                 capkeys = !exclusive,
                                 exclusive = TRUE,
                                 nomatch = NULL,
                                 append_key = FALSE,
                                 separator = "/",
                                 concatenator = concat(x),
                                 nested_scope = c("key", "dictionary"),
                                 apply_if = NULL,
                                 verbose = quanteda_options("verbose")) {
    check_class(class(x), "tokens_lookup")
}

#' @export
tokens_lookup.tokens_xptr <- function(x, dictionary, levels = 1:5,
                          valuetype = c("glob", "regex", "fixed"),
                          case_insensitive = TRUE,
                          capkeys = !exclusive,
                          exclusive = TRUE,
                          nomatch = NULL,
                          append_key = FALSE,
                          separator = "/",
                          concatenator = concat(x),
                          nested_scope = c("key", "dictionary"),
                          apply_if = NULL,
                          verbose = quanteda_options("verbose")) {

    if (!is.dictionary(dictionary))
        stop("dictionary must be a dictionary object")
    levels <- check_integer(levels, min = 1, max_len = Inf)
    valuetype <- match.arg(valuetype)
    capkeys <- check_logical(capkeys)
    exclusive <- check_logical(exclusive)
    nomatch <- check_character(nomatch, allow_null = TRUE)
    append_key <- check_logical(append_key)
    separator <- check_character(separator)
    concatenator <- check_character(concatenator)
    nested_scope <- match.arg(nested_scope)
    apply_if <- check_logical(apply_if, min_len = ndoc(x), max_len = ndoc(x),
                               allow_null = TRUE, allow_na = TRUE)
    verbose <- check_logical(verbose)
        
    attrs <- attributes(x)
    type <- get_types(x)
    ids <- object2id(dictionary, type, valuetype, case_insensitive,
                     field_object(attrs, "concatenator"), levels)
    overlap <- match(nested_scope, c("key", "dictionary"))
    if (is.null(apply_if))
        apply_if <- rep(TRUE, length.out = ndoc(x))
    
    if (append_key) {
        fixed <- lapply(ids, function(x) type[x])
        fixed <- structure(
            stri_c_list(fixed, concatenator),
            names = names(fixed)
        )
        key <- names(fixed)
        if (capkeys)
            key <- stri_trans_toupper(key)
        key <- paste0(fixed, separator, key)
        id_key <- seq_along(key)
    } else {
        key <- attr(ids, "key")
        id_key <- match(names(ids), key)
        if (capkeys)
            key <- stri_trans_toupper(key)
    }
    if (verbose)
        before <- stats_tokens(x)
    if (exclusive) {
        if (!is.null(nomatch)) {
            result <- cpp_tokens_lookup(x, ids, id_key, c(key, nomatch), overlap, 1,
                                        !apply_if, get_threads())
        } else {
            result <- cpp_tokens_lookup(x, ids, id_key, key, overlap, 0,
                                        !apply_if, get_threads())
        }
    } else {
        if (!is.null(nomatch))
            warning("nomatch only applies if exclusive = TRUE")
        id_used <- unique(id_key)
        result <- cpp_tokens_lookup(x, ids, match(id_key, id_used), key[id_used], overlap, 2,
                                    !apply_if, get_threads())
    }
    if (append_key)
        cpp_recompile(result)
    if (exclusive)
        field_object(attrs, "what") <- "dictionary"
    result <- rebuild_tokens(result, attrs)
    if (verbose)
        message_tokens("tokens_lookup()", before, stats_tokens(result))
    return(result)
}

#' @export
tokens_lookup.tokens <- function(x, ...) {
    as.tokens(tokens_lookup(as.tokens_xptr(x), ...))
}


