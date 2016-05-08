#' @export
#' @examples
#' data(SOTUCorpus, package = "quantedaData")
#' toks <- tokenize(SOTUCorpus, removePunct = TRUE)
#' seqs_token <- list(c('foreign', 'policy'), c('United', 'States'))
#' toks2 <- joinTokens(toks, seqs_token, "_")
#' kwic(toks2, 'foreign_policy', window=1) # joined
#' kwic(toks2, c('foreign', 'policy'), window=1) # not joined
#' kwic(toks2, 'United_States', window=1) # joined
joinTokens <- function(x, seqs_token, concatenator='-'){
  
  tokens <- x
  cat("Indexing tokens ...\n")
  index <- dfm(tokens, verbose = FALSE)
  
  for(i in 1:length(seqs_token)){
    seq_token <- seqs_token[[i]]
    if(is.list(seq_token) | !is.vector(seq_token) | length(seq_token) == 0) stop('Invalid token sequence\n');
    if(!all(seq_token %in% colnames(index))){
      cat(paste0(seq_token, concatenate=''), 'was not found', '\n')
    }else{
      flag <- rowSums(as(index[,seq_token], 'nMatrix')) == length(seq_token)
      cat(paste0('"', seq_token, concatenate='', '"'), 'was found in', sum(flag) ,'documents joining ...\n')
      tokens <- join_tokens_cppl(tokens, flag, seq_token, concatenator)
    }
  }
  tokens
}