#' join tokens function
#' 
#' Needs some more explanation
#' @param x some object
#' @param seqs_token something
#' @param concatenator character used for joining tokens
#' @param display progress
#' @examples
#' data(SOTUCorpus, package = "quantedaData")
#' toks <- tokenize(SOTUCorpus, removePunct = TRUE)
#' seqs_token <- list(c('foreign', 'policy'), c('United', 'States'))
#' toks2 <- joinTokens(toks, seqs_token, "_")
#' kwic(toks2, 'foreign_policy', window=1) # joined
#' kwic(toks2, c('foreign', 'policy'), window=1) # not joined
#' kwic(toks2, 'United_States', window=1) # joined
#' @export
joinTokens <- function(x, seqs_token, concatenator='-', verbose=TRUE){
  
  tokens <- x
  if(verbose) cat("Indexing tokens ...\n")
  index <- dfm(tokens, verbose = FALSE)
  
  for(i in 1:length(seqs_token)){
    seq_token <- seqs_token[[i]]
    if(length(seq_token) < 2) next
    if(is.list(seq_token) | !is.vector(seq_token) | length(seq_token) == 0) stop('Invalid token sequence\n');
    if(!all(seq_token %in% colnames(index))){
      if(verbose) cat(paste0(seq_token, concatenate=''), 'are not found', '\n')
    }else{
      flag <- rowSums(as(index[,seq_token], 'nMatrix')) == length(seq_token)
      if(verbose) cat(paste0('"', seq_token, concatenate='', '"'), 'are found in', sum(flag) ,'documents ...\n')
      tokens <- join_tokens_cppl(tokens, flag, seq_token, concatenator)
    }
  }
  tokens
}