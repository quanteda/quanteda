#' apply a dictionary to a tokens object
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
#'   below the first.  (See examples.)
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
#' @examples
#' toks <- tokens(data_corpus_inaugural)
#' dict <- dictionary(list(country = "united states", 
#'                    law=c('law*', 'constitution'), 
#'                    freedom=c('free*', 'libert*')))
#' dfm(tokens_lookup(toks, dict, valuetype='glob', verbose = TRUE))
#' dfm(tokens_lookup(toks, dict, valuetype='glob', verbose = TRUE, nomatch = 'NONE'))
#' 
#' dict_fix <- dictionary(list(country = "united states", 
#'                        law = c('law', 'constitution'), 
#'                        freedom = c('freedom', 'liberty'))) 
#' # dfm(applyDictionary(toks, dict_fix, valuetype='fixed'))
#' dfm(tokens_lookup(toks, dict_fix, valuetype='fixed'))
#' 
#' # hierarchical dictionary example
#' txt <- c(d1 = "The United States has the Atlantic Ocean and the Pacific Ocean.",
#'          d2 = "Britain and Ireland have the Irish Sea and the English Channel.")
#' toks <- tokens(txt)
#' dict <- dictionary(list(US = list(Countries = c("States"), 
#'                                   oceans = c("Atlantic", "Pacific")),
#'                         Europe = list(Countries = c("Britain", "Ireland"),
#'                                       oceans = list(west = "Irish Sea", 
#'                                                     east = "English Channel"))))
#' tokens_lookup(toks, dict, levels = 1)
#' tokens_lookup(toks, dict, levels = 2)
#' tokens_lookup(toks, dict, levels = 1:2)
#' tokens_lookup(toks, dict, levels = 3)
#' tokens_lookup(toks, dict, levels = c(1,3))
#' tokens_lookup(toks, dict, levels = c(2,3))
#' 
#' # show unmatched tokens
#' tokens_lookup(toks, dict, nomatch = "_UNMATCHED")
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

#' @noRd
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
    
    dictionary <- flatten_dictionary(dictionary, levels)
    valuetype <- match.arg(valuetype)
    attrs <- attributes(x)
    
    # Generate all combinations of type IDs
    values_id <- list()
    keys_id <- c()
    types <- types(x)
    
    
    if (verbose) 
        catm("applying a dictionary consisting of ", length(dictionary), " key", 
             if (length(dictionary) > 1L) "s" else "", "\n", sep="")
    
    index <- index_types(types, valuetype, case_insensitive) # index types before the loop
    for (h in seq_along(dictionary)) {
        values <- split_dictionary_values(dictionary[[h]], attr(x, 'concatenator'))
        values_temp <- regex2id(values, index = index)
        values_id <- c(values_id, values_temp)
        keys_id <- c(keys_id, rep(h, length(values_temp)))
    }
    if (capkeys) {
        keys <- char_toupper(names(dictionary))
    } else {
        keys <- names(dictionary)
    }
    if (exclusive) {
        if (!is.null(nomatch)) {
            x <- qatd_cpp_tokens_lookup(x, c(keys, nomatch[1]), values_id, keys_id, FALSE, 1)
        } else {
            x <- qatd_cpp_tokens_lookup(x, keys, values_id, keys_id, FALSE, 0)
        }
    } else {
        if (!is.null(nomatch))
            warning("nomatch only applies if exclusive = TRUE")
        x <- qatd_cpp_tokens_lookup(x, c(keys, types), values_id, keys_id, FALSE, 2)
    }
    attr(x, "what") <- "dictionary"
    attr(x, "dictionary") <- dictionary
    attributes(x, FALSE) <- attrs
    if (exclusive) attr(x, "padding") <- FALSE
    return(x)
}
