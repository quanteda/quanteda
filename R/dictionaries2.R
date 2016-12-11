#' apply a dictionary to a tokens object
#' 
#' Convert tokens into equivalence classes defined by values of a dictionary 
#' object.
#' @param x tokens object to which dictionary or thesaurus will be supplied
#' @param dictionary the \link{dictionary}-class object that will be applied to 
#'   \code{x}
#' @param valuetype how to interpret dictionary values: \code{"glob"} for 
#'   "glob"-style wildcard expressions (the format used in Wordstat and LIWC 
#'   formatted dictionary values); \code{"regex"} for regular expressions; or 
#'   \code{"fixed"} for exact matching (entire words, for instance)
#' @param concatenator a charactor that connect words in multi-words entries
#' @param case_insensitive ignore the case of dictionary values if \code{TRUE} 
#'   uppercase to distinguish them from other features
#' @param verbose print status messages if \code{TRUE}
#' @examples
#' toks <- tokens(data_corpus_inaugural)
#' dict <- dictionary(list(country = "united states", 
#'                    law=c('law*', 'constitution'), 
#'                    freedom=c('*', 'libert*')))
#' dfm(tokens_lookup(toks, dict, 'glob', verbose = TRUE))
#' 
#' dict_fix <- dictionary(list(country = "united states", 
#'                        law = c('law', 'constitution'), 
#'                        freedom = c('freedom', 'liberty'))) 
#' dfm(applyDictionary(toks, dict_fix, valuetype='fixed'))
#' dfm(tokens_lookup(toks, dict_fix, valuetype='fixed'))
#' @importFrom RcppParallel RcppParallelLibs
#' @export
tokens_lookup <- function(x, dictionary,
                           valuetype = c("glob", "regex", "fixed"), 
                           case_insensitive = TRUE,
                           concatenator = " ", 
                           verbose = FALSE) {
    
    if (!is.tokens(x))
        stop("x must be a tokens class object")
    
    valuetype <- match.arg(valuetype)

    names_org <- names(x)
    attrs_org <- attributes(x)
    

    # Generate all combinations of type IDs
    ngrams_id <- list()
    keys_id <- c()
    types <- types(x)
    index <- index(types, valuetype, case_insensitive)
    for(h in 1:length(dictionary)) {
        if(verbose) message('Searching words in "', names(dictionary[h]), '"...')
        ngrams <- stringi::stri_split_fixed(dictionary[[h]], concatenator)
        ngrams_fixed <- regex2fixed4(ngrams, index) # convert glob or regex to fixed
        if(length(ngrams_fixed) == 0) next
        ngrams_id <- c(ngrams_id, lapply(ngrams_fixed, function(x) fmatch(x, types)))
        keys_id <- c(keys_id, rep(h, length(ngrams_fixed)))
    }
    x <- qatd_cpp_tokens_lookup(x, ngrams_id, keys_id)
    
    attributes(x) <- attrs_org
    types(x) <- names(dictionary)
    names(x) <- names_org
    attr(x, "what") <- "dictionary"
    attr(x, "dictionary") <- dictionary
    
    return(x)
}
