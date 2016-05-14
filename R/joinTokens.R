#' join tokens function
#' 
#' Needs some more explanation
#' @param x some object
#' @param sequences list of vector of features to concatenate
#' @param concatenator character used for joining tokens
#' @param valueType how to interpret sequences: \code{fixed} for words as 
#'   is; \code{"regex"} for regular expressions; or \code{"glob"} for 
#'   "glob"-style wildcard
#' @param verbose display progress
#' @examples
#' data(SOTUCorpus, package = "quantedaData")
#' toks <- tokenize(SOTUCorpus, removePunct = TRUE)
#' seqs_token <- list(c('foreign', 'policy'), c('United', 'States'))
#' toks2 <- joinTokens(toks, seqs_token, "_")
#' kwic(toks2, 'foreign_policy', window=1) # joined
#' kwic(toks2, c('foreign', 'policy'), window=1) # not joined
#' kwic(toks2, 'United_States', window=1) # joined
#' @export
joinTokens <- function(x, sequences, concatenator='-', valueType='fixed', verbose=TRUE){
  
  tokens <- x
  seqs_token <- sequences
  if(verbose) cat("Indexing tokens ...\n")
  index <- dfm(tokens, verbose = FALSE)
  
  if(valuetype=='glob'){
    
    regex <- unlist(lapply(parts, utils::glob2rx))
    #cat("regex--------------------\n")
    #print(regex)
    match <- lapply(regex, function(x, y) y[stringi::stri_detect_regex(y, x)], colnames(index))
    #cat("match--------------------\n")
    #print(match)
    #print(length(match))
    if(length(unlist(regex)) != length(match)) next
    seqs_token <- do.call(expand.grid, c(match, stringsAsFactors=FALSE)) # Produce all possible combinations
  }
  
  for(i in 1:length(seqs_token)){
    seq_token <- seqs_token[[i]]
    if(length(seq_token) < 2) next
    if(is.list(seq_token) | !is.vector(seq_token) | length(seq_token) == 0) stop('Invalid token sequence\n');
    if(!all(seq_token %in% colnames(index))){
      if(verbose) cat(paste0('"', seq_token, concatenate='', '"'), 'are not found', '\n')
    }else{
      flag <- rowSums(as(index[,seq_token], 'nMatrix')) == length(seq_token)
      if(verbose) cat(paste0('"', seq_token, concatenate='', '"'), 'are found in', sum(flag) ,'documents ...\n')
      tokens <- join_tokens_cppl(tokens, flag, seq_token, concatenator)
    }
  }
  tokens
}