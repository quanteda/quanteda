
#' @rdname applyDictionary
#' @param concatenator a charactor that connect words in multi-words entries
#' @param indexing search only documents that containe keywords
#' @examples 
#' toks <- tokens(inaugCorpus)
#' dict <- dictionary(list(country = "united_states", law=c('law*', 'constitution'), freedom=c('free*', 'libert*')))
#' toks2 <- applyDictionary(toks, dict, 'glob', concatenator='_', verbose=TRUE)
#' head(dfm(toks2))
#' 
#' @export 
applyDictionary.tokens2 <- function(x, dictionary,
                                   valuetype = c("glob", "regex", "fixed"), 
                                   case_insensitive = TRUE,
                                   concatenator = '_', 
                                   indexing=TRUE, 
                                   verbose = FALSE) {
  
    valuetype <- match.arg(valuetype)
    
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
      if (valuetype %in% c("glob", "regex") | case_insensitive) {
          # Generates all possible patterns of sequences
          seqs_token <- grid_sequence(sequences, types, valuetype, case_insensitive)
      } else {
          seqs_token <- sequences
      }
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
