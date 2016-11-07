#' join tokens function
#'
#' Needs some more explanation
#' @param x some object
#' @param sequences list of vector of features to concatenate
#' @param concatenator character used for joining tokens
#' @param valuetype how to interpret sequences: \code{fixed} for words as
#'   is; \code{"regex"} for regular expressions; or \code{"glob"} for
#'   "glob"-style wildcard
#' @param case_insensitive if \code{TRUE}, ignore case when matching
#' @param verbose display progress
#' @author Kohei Watanabe and Kenneth Benoit
#' @examples
#' # with the inaugural corpus
#' toks <- tokenize(inaugCorpus, removePunct = TRUE)
#' seqs_token <- list(c('foreign', 'policy'), c('United', 'States'))
#' seqs_glob <- list(c('foreign', 'polic*'), c('United', 'States'))
#' seqs_regex <- list(c('^foreign', '^polic(ie|y)'), c('^United', '^States'))
#' toks2 <- joinTokens(toks, seqs_token, "_", 'fixed')
#' toks2 <- joinTokens(toks, seqs_glob, "_", 'glob')
#' toks2 <- joinTokens(toks, seqs_regex, "_", 'regex')
#' kwic(toks2, 'foreign_policy', window = 1) # joined
#' kwic(toks2, c('foreign', 'policy'), window = 1) # not joined
#' kwic(toks2, 'United_States', window = 1) # joined
joinTokensOld <- function(x, sequences, concatenator = "_", valuetype = c("glob", "fixed", "regex"), 
                       verbose = FALSE, case_insensitive = TRUE) {
    valuetype <- match.arg(valuetype)
    
    if (verbose) cat("Indexing tokens...\n")
    index <- dfm(x, verbose = FALSE) # index is always case-sensitive
    index_binary <- as(index, 'nMatrix')
    types <- colnames(index_binary)
    
    # Convert to regular expressions, then to fixed
    if (valuetype %in% c("glob"))
        sequences <- lapply(sequences, glob2rx)
    if (valuetype %in% c("glob", "regex") | case_insensitive) {
        # Generates all possible patterns of sequences
        seqs_token <- grid_sequence(sequences, types, valuetype, case_insensitive)
    } else {
        seqs_token <- sequences
    }
    #print(str(seqs_token))
    n_seqs <- length(seqs_token)
    if (n_seqs == 0) return(x)
    y <- deepcopy(x) # copy x to y to prevent changes in x
    for (i in 1:n_seqs) {
        seq_token <- seqs_token[[i]]
        if (length(seq_token) < 2) next
        if (is.list(seq_token) | !is.vector(seq_token) | length(seq_token) == 0) stop('Invalid token sequence\n');
        if (!all(seq_token %in% types)) {
            if(verbose) cat(sprintf('%d/%d "%s" is not found\n', i, n_seqs, paste(seq_token, collapse=' ')))
        } else {
            flag <- Matrix::rowSums(index_binary[,seq_token, drop = FALSE]) == length(seq_token)
            if (verbose) cat(sprintf('%d/%d "%s" is found in %d texts\n', i, n_seqs, paste(seq_token, collapse=' '), sum(flag)))
            join_tokens_cppl(y, flag, seq_token, concatenator) # pass y as reference
        }
    }
    removeFeatures(y, "")
}

#' join tokens function
#' @param x some object
#' @param sequences list of vector of features to concatenate
#' @param concatenator character used for joining tokens
#' @param valuetype how to interpret sequences: \code{fixed} for words as
#'   is; \code{"regex"} for regular expressions; or \code{"glob"} for
#'   "glob"-style wildcard
#' @param case_insensitive if \code{TRUE}, ignore case when matching
#' @param verbose display progress
#' @author Kohei Watanabe and Kenneth Benoit
#' @examples
#' # simple example
#' txt <- c("a b c d e f g", "A B C D E F G", "A b C d E f G", "aaa bbb ccc ddd eee fff ggg", "a_b b_c c_d d_e e_f f_g") 
#' toks_hash <- tokens(txt)
#' seqs <- tokens(c("a b", "C D", "aa* bb*", "eEE FFf", "d_e e_f"), hash=FALSE, what="fastestword")
#' joinTokens.tokenizedTextsHashed(toks_hash, seqs, valuetype = "glob", case_insensitive = TRUE, verbose=TRUE)
#' joinTokens.tokenizedTextsHashed(toks_hash, seqs, valuetype = "glob", case_insensitive = FALSE, verbose=TRUE)
#' 
#' # For development
#' x <- toks_hash
#' sequences <- phrasesFixed
#' valuetype <- 'glob'
#' concatenator = "_"
#' verbose = TRUE
#' case_insensitive = TRUE

#' @export
joinTokens <- function(x, sequences, concatenator = "_", 
                      valuetype = c("glob", "fixed", "regex"), 
                      verbose = FALSE, case_insensitive = TRUE) {
  
  valuetype <- match.arg(valuetype)
 
  if (verbose) cat("Indexing tokens...\n")
  types <- attr(x, 'types')
  index <- dfm(x, verbose = FALSE, toLower=FALSE) # index is always case-sensitive
  index_binary <- as(index, 'nMatrix')

  # Convert to regular expressions, then to fixed
  if (valuetype %in% c("glob"))
    sequences <- lapply(sequences, glob2rx)
  if (valuetype %in% c("glob", "regex") | case_insensitive) {
    # Generates all possible patterns of sequences
    seqs_token <- grid_sequence(sequences, types, valuetype, case_insensitive)
  } else {
    seqs_token <- sequences
  }
  
  # Check if joind tokens are in vocabulary
  types_new <- sapply(seqs_token, paste0, collapse=concatenator)
  ids_exist <- match(types_new, types)
  id_new <- length(types) + 1
  n_seqs <- length(seqs_token)
  if (n_seqs == 0) return(x)
  for (i in 1:n_seqs) {
    seq_token <- seqs_token[[i]]
    if (length(seq_token) < 2) next
    if (is.list(seq_token) | !is.vector(seq_token) | length(seq_token) == 0) stop('Invalid token sequence\n');
    if (!all(seq_token %in% types)) {
      if(verbose) cat(sprintf('%d/%d "%s" is not found\n', i, n_seqs, paste(seq_token, collapse=' ')))
    } else {
      flag <- Matrix::rowSums(index_binary[,seq_token, drop = FALSE]) == length(seq_token)
      if (verbose) cat(sprintf('%d/%d "%s" is found in %d texts\n', i, n_seqs, paste(seq_token, collapse=' '), sum(flag)))
      
      # Use exisitng ID
      if(is.na(ids_exist[i])){
        id <- id_new
      }else{
        id <- ids_exist[i]
      }
      if (verbose) cat(' Use', id , 'for', types_new[i], '\n')
      x <- qatd_cpp_replace_hash_list(x, flag, match(seq_token, types), id)
      
      # Add new ID to vocabulary only if used
      if(is.na(ids_exist[i]) & id %in% unlist(x, use.names = FALSE)){
        if (verbose) cat(' Add', id, 'for', types_new[i], '\n')
        types <- c(types, types_new[i])
        id_new <- id_new + 1
      }
    }
  }
  attr(x, 'types') <- types
  return(x)
}


grid_sequence <- function(seqs_pat, types, valuetype, case_insensitive = FALSE) {
    
    seqs_token <- list()
    for (seq_pat in seqs_pat) {
        if(valuetype == 'fixed'){
          seq_match <- lapply(seq_pat, function(x, y) y[toLower(y) %in% toLower(x)], types)
        }else{
          seq_match <- lapply(seq_pat, function(x, y) y[stringi::stri_detect_regex(y, x, case_insensitive = case_insensitive)], types)
        }
        #print(seq_match)
        if (length(unlist(seq_pat)) != length(seq_match)) next
        match_comb <- do.call(expand.grid, c(seq_match, stringsAsFactors = FALSE)) # produce all possible combinations
        #print(match_comb)
        seqs_token <- c(seqs_token, qatd_cpp_split_df(t(match_comb)))
    }
    return(seqs_token)
}
