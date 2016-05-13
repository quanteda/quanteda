#' find sequences of tokens
#'
#' This function automatically identify sequences of tokens. This algorithm is   
#' based on Blaheta and Johnson's “Unsupervised Learning of Multi-Word Verbs”.
#' @examples 
#' data(SOTUCorpus, package = "quantedaData")
#' sents <- tokenize(SOTUCorpus, what='sentence', simplify = TRUE)
#' tokens <- tokenize(sents, removePunct = TRUE)
#' types <- unique(unlist(tokens))
#' 
#' # Extracting multi-part nouns
#' types_upper <- types[stringi::stri_detect_regex(types, "^([A-Z][a-z\\-]{2,})")]
#' findSequences(tokens, types_upper, count_min=2)
#'
#' # Types can be any words
#' types_lower <- types[stri_detect_regex(types, "^([a-z]+)$") & !types %in%stopwords()]
#' findSequences(tokens, types_lower, count_min=2)
#' @export
findSequences <- function(x, tokens, count_min=2, smooth=0.001){
  
  seqs <- find_sequence_cppl(x, tokens, count_min, smooth);
  seqs$z <- seqs$lambda / seqs$sigma
  seqs$p <- 1 - pnorm(seqs$z)
  seqs$mue <- seqs$lambda - (3.29 * seqs$sigma) # mue should be greater than zero
  class(seqs) <- "tokenSequences"
  seqs
}

#' print a tokenSequences objects
#' 
#' print method for a \link{tokenSequences} object
#' @param x a tokenSequences object created by \link{findSequences}
#' @param ... further arguments passed to base print method
#' @export
#' @method print tokenSequences
print.tokenSequences <- function(x, ...){
    df <- data.frame(sequence=sapply(x[['sequence']], paste, collapse = " "),
                     len=sapply(x[['sequence']], length),
                     #lambda=x$lambda,
                     #sigma=x$sigma,
                     z=x$z,
                     p=x$p,
                     mue=x$mu)
    df <- df[order(df$z),]
    print(df, ...)
}