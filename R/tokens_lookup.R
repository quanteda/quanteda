#' Apply a dictionary to a tokens object
#' 
#' Convert tokens into equivalence classes defined by values of a dictionary 
#' object.
#' @param x tokens object to which dictionary or thesaurus will be supplied
#' @param dictionary the \link{dictionary}-class object that will be applied to 
#'   \code{x}
#' @param levels integers specifying the levels of entries in a hierarchical 
#'   dictionary that will be applied.  The top level is 1, and subsequent levels
#'   describe lower nesting levels.  Values may be combined, even if these 
#'   levels are not contiguous, e.g. `levels = c(1:3)` will collapse the second 
#'   level into the first, but record the third level (if present) collapsed
#'   below the first (see examples).
#' @inheritParams valuetype
#' @param case_insensitive ignore the case of dictionary values if \code{TRUE} 
#'   uppercase to distinguish them from other features
#' @param capkeys if TRUE, convert dictionary keys to uppercase to distinguish 
#'   them from other features
#' @param nomatch an optional character naming a new key for tokens that do not
#'   matched to a dictionary values  If \code{NULL} (default), do not record
#'   unmatched tokens.
#' @param exclusive if \code{TRUE}, remove all features not in dictionary, 
#'   otherwise, replace values in dictionary with keys while leaving other 
#'   features unaffected
#' @param verbose print status messages if \code{TRUE}
#' @keywords tokens
#' @seealso tokens_replace
#' @examples
#' toks1 <- tokens(data_corpus_inaugural)
#' dict1 <- dictionary(list(country = "united states", 
#'                    law=c('law*', 'constitution'), 
#'                    freedom=c('free*', 'libert*')))
#' dfm(tokens_lookup(toks1, dict1, valuetype='glob', verbose = TRUE))
#' dfm(tokens_lookup(toks1, dict1, valuetype='glob', verbose = TRUE, nomatch = 'NONE'))
#' 
#' dict2 <- dictionary(list(country = "united states", 
#'                        law = c('law', 'constitution'), 
#'                        freedom = c('freedom', 'liberty'))) 
#' # dfm(applyDictionary(toks1, dict2, valuetype='fixed'))
#' dfm(tokens_lookup(toks1, dict2, valuetype='fixed'))
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
#' @importFrom RcppParallel RcppParallelLibs
#' @export
tokens_lookup <- function(x, dictionary, levels = 1:5,
                          valuetype = c("glob", "regex", "fixed"), 
                          case_insensitive = TRUE,
                          capkeys = !exclusive,
                          exclusive = TRUE,
                          nomatch = NULL,
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
                          verbose = quanteda_options("verbose")) {
    if (!is.tokens(x))
        stop("x must be a tokens object")
    
    if (!is.dictionary(dictionary))
        stop("dictionary must be a dictionary object")
    
    valuetype <- match.arg(valuetype)
    attrs <- attributes(x)
    type <- types(x)
    if (verbose) 
        catm("applying a dictionary consisting of ", length(dictionary), " key", 
             if (length(dictionary) > 1L) "s" else "", "\n", sep="")

    ids <- pattern2list(dictionary, type, valuetype, case_insensitive,
                        attr(x, "concatenator"), levels)
    key <- attr(ids, "key")
    id_key <- match(names(ids), key)
    if (capkeys)
        key <- char_toupper(key)
    if (exclusive) {
        if (!is.null(nomatch)) {
            x <- qatd_cpp_tokens_lookup(x, c(key, nomatch[1]), ids, id_key, FALSE, 1)
        } else {
            x <- qatd_cpp_tokens_lookup(x, key, ids, id_key, FALSE, 0)
        }
    } else {
        if (!is.null(nomatch))
            warning("nomatch only applies if exclusive = TRUE")
        id_used <- unique(id_key)
        x <- qatd_cpp_tokens_lookup(x, c(key[id_used], type), ids, match(id_key, id_used), FALSE, 2)
    }
    attr(x, "what") <- "dictionary"
    attr(x, "dictionary") <- dictionary
    attributes(x, FALSE) <- attrs
    if (exclusive) attr(x, "padding") <- FALSE
    return(x)
}
