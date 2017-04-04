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
#'   level into the first, but record the third level (if present) collapsed below
#'   the first.  (See examples.)
#' @inheritParams valuetype
#' @param concatenator a charactor that connect words in multi-words entries in \code{x}
#' @param case_insensitive ignore the case of dictionary values if \code{TRUE} 
#'   uppercase to distinguish them from other features
#' @param capkeys if TRUE, convert dictionary keys to uppercase to distinguish 
#'   them from other features
#' @param exclusive if \code{TRUE}, remove all features not in dictionary, 
#'   otherwise, replace values in dictionary with keys while leaving other 
#'   features unaffected
#' @param multiword if \code{FALSE}, multi-word entries in dictionary are treated
#'   as single tokens
#' @param verbose print status messages if \code{TRUE}
#' @examples
#' toks <- tokens(data_corpus_inaugural)
#' dict <- dictionary(list(country = "united states", 
#'                    law=c('law*', 'constitution'), 
#'                    freedom=c('free*', 'libert*')))
#' dfm(tokens_lookup(toks, dict, valuetype='glob', verbose = TRUE))
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
#' @importFrom RcppParallel RcppParallelLibs
#' @export
tokens_lookup <- function(x, dictionary, levels = 1:5,
                          valuetype = c("glob", "regex", "fixed"), 
                          concatenator = ' ',
                          case_insensitive = TRUE,
                          capkeys = !exclusive,
                          exclusive = TRUE,
#                          overlap = FALSE,
                          multiword = TRUE,
                          verbose = quanteda_options("verbose")) {
    UseMethod("tokens_lookup")    
}

#' @noRd
#' @export
tokens_lookup.tokens <- function(x, dictionary, levels = 1:5,
                          valuetype = c("glob", "regex", "fixed"), 
                          concatenator = ' ',
                          case_insensitive = TRUE,
                          capkeys = !exclusive,
                          exclusive = TRUE,
                          multiword = TRUE,
                          verbose = quanteda_options("verbose")) {

    if (!is.tokens(x))
        stop("x must be a tokens object")
    
    if (!is.dictionary(dictionary))
        stop("dictionary must be a dictionary object")
    
    dictionary <- flatten_dictionary(dictionary, levels)
    valuetype <- match.arg(valuetype)
    attrs <- attributes(x)
    
    # Generate all combinations of type IDs
    entries_id <- list()
    keys_id <- c()
    types <- types(x)
    
    index <- index_regex(types, valuetype, case_insensitive) # index types before the loop
    if (verbose) 
        catm("applying a dictionary consisting of ", length(dictionary), " key", 
             ifelse(length(dictionary) > 1, "s", ""), "\n", sep="")
    
    for (h in seq_along(dictionary)) {
        entries <- dictionary[[h]]
        
        # Substitute dictionary's concatenator with tokens' concatenator 
        if (concatenator != attr(dictionary, 'concatenator'))
            entries <- stringi::stri_replace_all_fixed(entries, attr(dictionary, 'concatenator'), concatenator)
        
        # Separate entries by concatenator
        if (multiword) {
            entries <- stringi::stri_split_fixed(entries, concatenator)
        } else {
            entries <- as.list(entries)
        } 
        entries_temp <- regex2id(entries, types, valuetype, case_insensitive, index)
        entries_id <- c(entries_id, entries_temp)
        keys_id <- c(keys_id, rep(h, length(entries_temp)))
    }
    # if (verbose) 
    #     message('Searching ', length(entries_id), ' types of features...')
    if (capkeys) {
        keys <- char_toupper(names(dictionary))
    } else {
        keys <- names(dictionary)
    }
    if (exclusive) {
        x <- qatd_cpp_tokens_lookup(x, keys, entries_id, keys_id, FALSE)
    } else {
        x <- qatd_cpp_tokens_match(x, c(types, keys), entries_id, keys_id + length(types), FALSE)
    }
    attributes(x, FALSE) <- attrs
    attr(x, "what") <- "dictionary"
    attr(x, "dictionary") <- dictionary
    return(x)
}
