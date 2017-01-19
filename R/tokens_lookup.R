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
#' @param concatenator a charactor that connect words in multi-words entries
#' @param case_insensitive ignore the case of dictionary values if \code{TRUE} 
#'   uppercase to distinguish them from other features
#' @param capkeys if TRUE, convert dictionary keys to uppercase to distinguish 
#'   them from other features
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
#' 
#' dict_fix <- dictionary(list(country = "united states", 
#'                        law = c('law', 'constitution'), 
#'                        freedom = c('freedom', 'liberty'))) 
#' dfm(applyDictionary(toks, dict_fix, valuetype='fixed'))
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
                          case_insensitive = TRUE,
                          capkeys = FALSE,
                          concatenator = " ",
                          exclusive = TRUE,
                          verbose = FALSE) {
    
    names_org <- names(x)
    attrs_org <- attributes(x)
    dictionary <- dictionary_flatten(dictionary, levels)
    valuetype <- match.arg(valuetype)
    
    # Generate all combinations of type IDs
    entries_id <- list()
    keys_id <- c()
    types <- types(x)
    
    index <- index_regex(types, valuetype, case_insensitive) # index types before the loop
    if (verbose) 
        message('Registering ', length(unlist(dictionary)), ' entiries in the dictionary...');
    for (h in 1:length(dictionary)) {
        entries <- stringi::stri_split_fixed(dictionary[[h]], concatenator)
        entries_temp <- regex2id(entries, types, valuetype, case_insensitive, index)
        entries_id <- c(entries_id, entries_temp)
        keys_id <- c(keys_id, rep(h, length(entries_temp)))
    }
    if (verbose) 
        message('Searching ', length(entries_id), ' types of features...')
    
    if(exclusive){
        x <- qatd_cpp_tokens_lookup(x, entries_id, keys_id)
    }else{
        x <- qatd_cpp_tokens_replace(x, entries_id, keys_id + length(types))
    }
    attributes(x) <- attrs_org
    if(exclusive){
        if (capkeys) {
            types(x) <- char_toupper(names(dictionary))
        } else {
            types(x) <- names(dictionary)
        }
    }else{
        if (capkeys) {
            types(x) <- c(types, char_toupper(names(dictionary)))
        } else {
            types(x) <- c(types, names(dictionary))
        }
    }
    
    names(x) <- names_org
    attr(x, "what") <- "dictionary"
    attr(x, "dictionary") <- dictionary
    tokens_hashed_recompile(x)
}
