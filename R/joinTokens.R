#' join tokens function
#'
#' Needs some more explanation
#' @param x some object
#' @param sequences list of vector of features to concatenate
#' @param concatenator character used for joining tokens
#' @param valuetype how to interpret sequences: \code{fixed} for words as
#'   is; \code{"regex"} for regular expressions; or \code{"glob"} for
#'   "glob"-style wildcard
#' @param verbose display progress
#' @examples
#' toks <- tokenize(inaugCorpus, removePunct = TRUE)
#' seqs_token <- list(c('foreign', 'policy'), c('United', 'States'))
#' seqs_glob <- list(c('foreign', 'polic*'), c('United', 'States'))
#' seqs_regex <- list(c('^foreign', '^polic(ie|y)'), c('^United', '^States'))
#' toks2 <- joinTokens(toks, seqs_token, "_", 'glob')
#' toks2 <- joinTokens(toks, seqs_glob, "_", 'glob')
#' toks2 <- joinTokens(toks, seqs_regex, "_", 'regex')
#' kwic(toks2, 'foreign_policy', window=1) # joined
#' kwic(toks2, c('foreign', 'policy'), window=1) # not joined
#' kwic(toks2, 'United_States', window=1) # joined
#' @export
joinTokens <- function(x, sequences, concatenator='-', valuetype='fixed', verbose=FALSE){

  if(verbose) catm("Indexing tokens...\n")
  index <- dfm(x, verbose = FALSE)
  index_binary <- as(index, 'nMatrix')
  types <- colnames(index_binary)

  seqs_token <- list()
  if(valuetype=='regex' | valuetype=='glob'){
    if(verbose) catm("Converting patterns to tokens...\n")
    for(sequence in sequences){
      if(valuetype=='glob'){
        seq_regex <- unlist(lapply(sequence, utils::glob2rx))
      }else{
        seq_regex <- sequence
      }
      #catm("seq_regex--------------------\n")
      #print(seq_regex)
      match <- lapply(seq_regex, function(x, y) y[stringi::stri_detect_regex(y, x)], types)
      #catm("match--------------------\n")
      #print(match)
      if(length(unlist(seq_regex)) != length(match)) next
      match_comb <- do.call(expand.grid, c(match, stringsAsFactors=FALSE)) # produce all possible combinations
      #catm("match_comb--------------------\n")
      #print(match_comb)
      seqs_token <- c(seqs_token, split_df_cpp(t(match_comb)))
    }
  }else{
    seqs_token <- sequences
  }
  #catm("seqs_token--------------------\n")
  #print(seqs_token)
  n_seqs <- length(seqs_token)
  if(n_seqs == 0) return(x)

  y <- deepcopy(x) # copy x to y to prevent changes in x
  for(i in 1:n_seqs){
    seq_token <- seqs_token[[i]]
    if(length(seq_token) < 2) next
    if(is.list(seq_token) | !is.vector(seq_token) | length(seq_token) == 0) stop('Invalid token sequence\n');
    if(!all(seq_token %in% types)){
      if(verbose) catm(paste0('"', seq_token, concatenate='', '"'), 'are not found', '\n')
    }else{
      flag <- Matrix::rowSums(index_binary[,seq_token]) == length(seq_token)
      if(verbose) catm(sprintf('%d/%d "%s" is found in %d texts\n', i, n_seqs, paste(seq_token, collapse=' '), sum(flag)))
      join_tokens_cppl(y, flag, seq_token, concatenator) # pass y as reference
    }
  }
  return(y)
}
