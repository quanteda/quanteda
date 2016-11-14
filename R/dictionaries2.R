#' apply a dictionary or thesaurus to an object
#' 
#' Convert features into equivalence classes defined by values of a dictionary 
#' object.
#' @note Selecting only features defined in a "dictionary" is traditionally 
#'   known in text analysis as a \emph{dictionary method}, even though
#'   technically this "dictionary" operates more like a thesarus.  If a thesaurus-like
#'   application is desired, set \code{exclusive = FALSE} to convert features 
#'   defined as values in a dictionary into their keys, while keeping all other
#'   features.
#' @return an object of the type passed with the value-matching features
#'   replaced by dictionary keys
#' @param x object to which dictionary or thesaurus will be supplied
#' @param dictionary the \link{dictionary}-class object that will be applied to
#'   \code{x}
#' @export
applyDictionary2 <- function(x, dictionary, ...) {
    UseMethod("applyDictionary2")
}


#' @rdname applyDictionary
#' @param concatenator a charactor that connect words in multi-words entries
#' @examples
#' 
#' dict_liwc <- dictionary(file='/home/kohei/Documents/Dictonary/LIWC/LIWC2007_English.dic')
#' toks <- tokens(inaugCorpus)
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
#' 
#' microbenchmark::microbenchmark(
#'   r=applyDictionary(toks, dict, valuetype='fixed', verbose=FALSE),
#'   cpp=applyDictionary2(toks, dict, valuetype='fixed', verbose=FALSE)
#' )
#' 
#' microbenchmark::microbenchmark(
#'   r=applyDictionary(toks, dict_liwc, valuetype='fixed', verbose=FALSE),
#'   cpp=applyDictionary2(toks, dict_liwc, valuetype='fixed', verbose=FALSE),
#'   times=1
#' )
#' 
#' toks_short <- tokens(tokenize(inaugCorpus, what='sentence', simplify=TRUE))
#' microbenchmark::microbenchmark(
#'   r=applyDictionary(toks_short, dict, valuetype='fixed', verbose=FALSE),
#'   cpp=applyDictionary2(toks_short, dict, valuetype='fixed', verbose=FALSE)
#' )
#' 
#' microbenchmark::microbenchmark(
#'   dfm=applyDictionary(dfm(toks), dict_liwc, valuetype='glob', verbose=FALSE),
#'   tokens=applyDictionary2(toks, dict_liwc, valuetype='glob', verbose=FALSE),
#'   times=1
#' )
#' 
#' profvis::profvis(applyDictionary2(toks, dict_liwc[1], valuetype='glob', verbose=FALSE))
#' 
#' @export 
applyDictionary2.tokens <- function(x, dictionary,
                                    valuetype = c("glob", "regex", "fixed"), 
                                    case_insensitive = TRUE,
                                    concatenator = ' ', 
                                    verbose = FALSE) {
    
    valuetype <- match.arg(valuetype)
    
    # Case-insesitive
    if(case_insensitive){
        x <- toLower(x)
        dictionary <- lapply(dictionary, toLower)
    }
    
    # Initialize
    tokens <- qatd_cpp_structcopy_int_list(x)
    types <- types(x)
    
    for(h in 1:length(dictionary)){
        
        if(verbose) message('Searching words in "', names(dictionary[h]), '"...')
        keys <- stringi::stri_split_fixed(dictionary[[h]], concatenator)
        
        # Convert to regular expressions, then to fixed
        if (valuetype %in% c("glob"))
            keys <- lapply(keys, glob2rx)
        if (valuetype %in% c("glob", "regex")) {
            # Generates all possible patterns of keys
            keys_token <- expand_regex(keys, types, valuetype, case_insensitive)
        } else {
            keys_token <- keys
        }
        if(length(keys_token) == 0) next
        keys_id <- lapply(keys_token, function(x) fmatch(x, types))
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

# mx <- applyDictionary(dfm(toks), dict_liwc)
# 
# regex <- list(c('^a$', '^b'), c('c'), c('d'))
# types <- c('A', 'AA', 'B', 'BB', 'BBB', 'C', 'CC')
# types <- attr(toks, "types")
# valuetype <- 'glob'
# case_insensitive = FALSE
# expand_regex(regex, types, 'fixed', case_insensitive=FALSE)
# expand_regex(dict_liwc, types, 'glob', case_insensitive=TRUE)

expand_regex <- function(patterns, types, valuetype, case_insensitive = FALSE) {
    
    # Initialize
    ids <- list()
    
    # Separate multi and single-entry patterns
    len <- lengths(patterns)
    pats_multi <- patterns[len>1] 
    pats_single <- patterns[len==1]
    
    # Process multi-entry patterns
    for (pat_multi in pats_multi) {
        if(valuetype == 'fixed'){
            if(case_insensitive){
                id_multi <- lapply(pat_multi, function(x, y) y[toLower(y) %in% toLower(x)], types)
            }else{
                id_multi <- lapply(pat_multi, function(x, y) y[y %in% x], types)
            }
        }else{
            id_multi <- lapply(pat_multi, function(x, y) stri_subset_regex(y, x, case_insensitive = case_insensitive), types)
        }
        id_comb <- as.matrix(do.call(expand.grid, c(id_multi, stringsAsFactors = FALSE))) # create all possible combinations
        ids <- c(ids, unname(split(id_comb, row(id_comb))))
    }
    
    # Process single-entry patterns
    if(length(pats_single) > 0){
        pats_single <- unlist(pats_single, use.names = FALSE)
        if (valuetype == 'fixed'){
            if(case_insensitive){
                id_single <- as.list(types[toLower(types) %in% toLower(pats_single)])
            }else{
                id_single <- as.list(types[types %in% pats_single])
            }
        }else{
            pats_single_all <- paste0(pats_single, collapse="|")
            id_single <- as.list(stri_subset_regex(types, pats_single_all, case_insensitive = case_insensitive))
        }
        ids <- c(ids, id_single)
    }
    return(ids)
}
