#' Apply a dictionary to a tokens object
#'
#' Convert tokens into equivalence classes defined by values of a dictionary
#' object.
#' @param x tokens object to which dictionary or thesaurus will be supplied
#' @param dictionary the [dictionary]-class object that will be applied to
#'   `x`
#' @param levels integers specifying the levels of entries in a hierarchical
#'   dictionary that will be applied.  The top level is 1, and subsequent levels
#'   describe lower nesting levels.  Values may be combined, even if these
#'   levels are not contiguous, e.g. `levels = c(1:3)` will collapse the second
#'   level into the first, but record the third level (if present) collapsed
#'   below the first (see examples).
#' @inheritParams valuetype
#' @param capkeys if TRUE, convert dictionary keys to uppercase to distinguish
#'   them from other features
#' @param nomatch an optional character naming a new key for tokens that do not
#'   matched to a dictionary values  If `NULL` (default), do not record
#'   unmatched tokens.
#' @param exclusive if `TRUE`, remove all features not in dictionary,
#'   otherwise, replace values in dictionary with keys while leaving other
#'   features unaffected
#' @param nested_scope how to treat matches from different dictionary keys that
#'   are nested.  When one value is nested within another, such as "a b" being
#'   nested within "a b c", `the `tokens_lookup()` will match the longer.  When
#'   `nested_scope = "key"`, this longer-match priority is applied only
#'   within the key, while `"dictionary"` applies it across keys, matching only
#'   the key with the longer pattern, not the matches nested within that longer
#'   pattern from other keys.  See Details.
#' @param verbose print status messages if `TRUE`
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
#' *Overlapped*
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
#' @importFrom RcppParallel RcppParallelLibs
#' @export
tokens_lookup <- function(x, dictionary, levels = 1:5,
                          valuetype = c("glob", "regex", "fixed"),
                          case_insensitive = TRUE,
                          capkeys = !exclusive,
                          exclusive = TRUE,
                          nomatch = NULL,
                          nested_scope = c("key", "dictionary"),
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
                                 nested_scope = c("key", "dictionary"),
                                 verbose = quanteda_options("verbose")) {
    stop(friendly_class_undefined_message(class(x), "tokens_lookup"))
}

#' @export
tokens_lookup.tokens <- function(x, dictionary, levels = 1:5,
                          valuetype = c("glob", "regex", "fixed"),
                          case_insensitive = TRUE,
                          capkeys = !exclusive,
                          exclusive = TRUE,
                          nomatch = NULL,
                          nested_scope = c("key", "dictionary"),
                          verbose = quanteda_options("verbose")) {

    x <- as.tokens(x)
    if (!is.dictionary(dictionary))
        stop("dictionary must be a dictionary object")

    valuetype <- match.arg(valuetype)
    nested_scope <- match.arg(nested_scope)
    attrs <- attributes(x)
    type <- types(x)
    if (verbose)
        catm("applying a dictionary consisting of ", length(dictionary), " key",
             if (length(dictionary) > 1L) "s" else "", "\n", sep = "")
    ids <- pattern2list(dictionary, type, valuetype, case_insensitive,
                        field_object(attrs, "concatenator"), levels)
    key <- attr(ids, "key")
    id_key <- match(names(ids), key)
    overlap <- match(nested_scope, c("key", "dictionary"))
    if (capkeys)
        key <- char_toupper(key)
    if (exclusive) {
        if (!is.null(nomatch)) {
            result <- qatd_cpp_tokens_lookup(x, c(key, nomatch[1]), ids, id_key, overlap, 1)
        } else {
            result <- qatd_cpp_tokens_lookup(x, key, ids, id_key, overlap, 0)
        }
    } else {
        if (!is.null(nomatch))
            warning("nomatch only applies if exclusive = TRUE")
        id_used <- unique(id_key)
        result <- qatd_cpp_tokens_lookup(x, c(key[id_used], type), ids, match(id_key, id_used), overlap, 2)
    }
    field_object(attrs, "what") <- "dictionary"
    rebuild_tokens(result, attrs)
}
