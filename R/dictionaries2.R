#' apply a dictionary to a tokens object
#' 
#' Faster version of applyDictionary for hashed tokens objects created by
#' \code{\link{tokens}}.
#' @param x object to which dictionary or thesaurus will be supplied
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
#' 
#' toks <- tokens(data_corpus_inaugural)
#' dict <- dictionary(list(country = "united states", 
#'                    law=c('law*', 'constitution'), 
#'                    freedom=c('free*', 'libert*')))
#' toks2 <- applyDictionary2(toks, dict, 'glob', verbose=TRUE)
#' head(dfm(toks2))
#' 
#' dict_fix <- dictionary(list(country = "united states", 
#'                        law=c('law', 'constitution'), 
#'                        freedom=c('freedom', 'liberty'))) 
#' head(dfm(applyDictionary(toks, dict_fix, valuetype='fixed', verbose=TRUE)))
#' head(dfm(applyDictionary2(toks, dict_fix, valuetype='fixed', verbose=TRUE)))
#' @export
applyDictionary2 <- function(x, dictionary,
                             valuetype = c("glob", "regex", "fixed"), 
                             case_insensitive = TRUE,
                             concatenator = ' ', 
                             verbose = FALSE) {
    
    valuetype <- match.arg(valuetype)
    
    # Case-insesitive
    if (case_insensitive) {
        x <- toLower(x)
        dictionary <- lapply(dictionary, toLower)
    }
    
    # Initialize
    tokens <- qatd_cpp_structcopy_int_list(x) # create empty tokens object
    types <- types(x)
    index <- index(types, valuetype, case_insensitive)
    for(h in 1:length(dictionary)){
        
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
