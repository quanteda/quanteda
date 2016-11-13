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
#' @param indexing search only documents that containe keywords
#' @examples
#' 
#' dict_liwc <- dictionary(file='/home/kohei/Documents/Dictonary/LIWC/LIWC2007_English.dic')
#' library(quantedaData)
#' toks <- tokens(inaugCorpus)
#' dict <- dictionary(list(country = "united states", law=c('law*', 'constitution'), freedom=c('free*', 'libert*')))
#' dict <- dictionary(list(country = "united states", law=c('law', 'constitution'), freedom=c('freedom', 'liberty')))
#' dict <- dictionary(list(country = c("united states", "united kingdom")))
#' dict <- dictionary(list(rare=c("predilection", "flattering", "asylum interruptions", "awaken", "distrustful")))
#' toks2 <- applyDictionary2(toks, dict, 'glob', verbose=TRUE)
#' head(dfm(toks2))
#' 
#' microbenchmark::microbenchmark(
#'   r=applyDictionary(toks, dict, valuetype='fixed', verbose=FALSE),
#'   cpp=applyDictionary2(toks, dict, valuetype='fixed', verbose=FALSE, indexing=FALSE),
#'   cppi=applyDictionary2(toks, dict, valuetype='fixed', verbose=FALSE, indexing=TRUE)
#' )
#' 
#' microbenchmark::microbenchmark(
#'   r=applyDictionary(toks, dict_liwc, valuetype='fixed', verbose=FALSE),
#'   cpp=applyDictionary2(toks, dict_liwc, valuetype='glob', verbose=FALSE, indexing=FALSE),
#'   cppi=applyDictionary2(toks, dict_liwc, valuetype='glob', verbose=FALSE, indexing=TRUE),
#'   times=1
#' )
#' 
#' toks_short <- tokens(tokenize(inaugCorpus, what='sentence', simplify=TRUE))
#' microbenchmark::microbenchmark(
#'   r=applyDictionary(toks_short, dict, valuetype='fixed', verbose=FALSE),
#'   cpp=applyDictionary2(toks_short, dict, valuetype='fixed', verbose=FALSE, indexing=FALSE),
#'   cppi=applyDictionary2(toks_short, dict, valuetype='fixed', verbose=FALSE, indexing=TRUE)
#' )
#' profvis::profvis(applyDictionary2(toks_long, dict, valuetype='fixed', verbose=FALSE, indexing=FALSE))
#' 
#' @export 
applyDictionary2.tokens <- function(x, dictionary,
                                   valuetype = c("glob", "regex", "fixed"), 
                                   case_insensitive = TRUE,
                                   concatenator = ' ', 
                                   indexing=TRUE, 
                                   verbose = FALSE) {
  
    valuetype <- match.arg(valuetype)
    
    # Case-insesitive
    if(case_insensitive){
      x <- toLower(x)
      dictionary <- lapply(dictionary, toLower)
    }
    
    # Initialize
    tokens <- NULL
    types <- types(x)
    
    # Index tokens
    if(indexing){
      if (verbose) message("Indexing tokens...")
      index <- dfm(x, verbose = FALSE, toLower = FALSE) # index is always case-sensitive
      index_binary <- as(index, 'nMatrix')
    }
    for(h in 1:length(dictionary)){
      
      if(verbose) message('Searching words in "', names(dictionary[h]), '"...')
      sequences <- stringi::stri_split_fixed(dictionary[[h]], concatenator)
      
      # Convert to regular expressions, then to fixed
      if (valuetype %in% c("glob"))
          sequences <- lapply(sequences, glob2rx)
      if (valuetype %in% c("glob", "regex")) {
          # Generates all possible patterns of sequences
          seqs_token <- grid_sequence(sequences, types, valuetype, case_insensitive)
      } else {
          seqs_token <- sequences
      }
      if(length(seqs_token) == 0) next
      for(i in 1:length(seqs_token)){
          seq_token <- seqs_token[[i]]
          if(verbose) message('   "', paste(seq_token, collapse=concatenator), '"')
          if (any(is.na(fmatch(seq_token, types)))) next
          if(indexing){
            flag <- Matrix::rowSums(index_binary[,seq_token, drop = FALSE]) == length(seq_token)
          }else{
            flag <- rep(TRUE, length(x))
          }
          tokens <- qatd_cpp_detect_hash_list(x, tokens, flag, fmatch(seq_token, types), h)
      }
    }
    tokens <- qatd_cpp_remove_int_list(tokens, 0) # remove padding
    attributes(tokens) <- attributes(x)
    types(tokens) <- names(dictionary)
    attr(tokens, "what") <- "dictionary"
    attr(tokens, "dictionary") <- dictionary
    
    return(tokens)
}
