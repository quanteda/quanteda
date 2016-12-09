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
#' toks <- tokens(data_corpus_inaugural[1:5])
#' dict <- dictionary(list(country = "united states", 
#'                    law=c('law*', 'constitution'), 
#'                    freedom=c('free*', 'libert*')))
#' dfm(tokens_lookup(toks, dict, 'glob', verbose = TRUE))
#' 
#' dict_fix <- dictionary(list(country = "united states", 
#'                        law = c('law', 'constitution'), 
#'                        freedom = c('freedom', 'liberty'))) 
#' dfm(applyDictionary(toks, dict_fix, valuetype='fixed'))
#' dfm(tokens_lookup(toks, dict_fix, valuetype='fixed'))
#' @export
tokens_lookup <- function(x, dictionary,
                           valuetype = c("glob", "regex", "fixed"), 
                           case_insensitive = TRUE,
                           concatenator = " ", 
                           verbose = FALSE) {
    
    if (!is.tokens(x))
        stop("x must be a tokens class object")
    
    valuetype <- match.arg(valuetype)
    
    # Case-insesitive 
    #### this is inefficient, better to pass through case_insensitive = TRUE
    #### to the regex match --KB
    if (case_insensitive) {
        x <- toLower(x)
        dictionary <- lapply(dictionary, toLower)
    }
    
    # Initialize
    tokens <- qatd_cpp_structcopy_int_list(x) # create empty tokens object
    types <- types(x)
    index <- index(types, valuetype, case_insensitive)
    for(h in 1:length(dictionary)) {
        
        if(verbose) message('Searching words in "', names(dictionary[h]), '"...')
        keys <- stringi::stri_split_fixed(dictionary[[h]], concatenator)
        
        # Convert to regular expressions, then to fixed
        if (valuetype %in% c("glob"))
            keys <- lapply(keys, glob2rx)
        if (valuetype %in% c("glob", "regex")) {
            # Generates all possible patterns of keys
            keys_fixed <- regex2fixed4(keys, index)
        } else {
            keys_fixed <- keys
        }
        if(length(keys_fixed) == 0) next
        keys_id <- lapply(keys_fixed, function(x) fmatch(x, types))
        #print(keys_id)
        tokens <- qatd_cpp_lookup_int_list(x, tokens, keys_id, h)
    }
    tokens <- qatd_cpp_remove_int_list(tokens, 0) # remove padding
    attributes(tokens) <- attributes(x)
    types(tokens) <- names(dictionary)
    attr(tokens, "what") <- "dictionary"
    attr(tokens, "dictionary") <- dictionary
    
    return(tokens)
}
