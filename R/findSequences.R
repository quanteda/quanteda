

#' find sequences of tokens
#' 
#' This function automatically identify sequences of tokens. This algorithm is 
#' based on Blaheta and Johnson's “Unsupervised Learning of Multi-Word Verbs”.
#' @param x tokenizedTexts objects
#' @param tokens types of token in sequuences
#' @param count_min minimum frequency of sequences
#' @param smooth smoothing factor
#' @param nested collect nested sub-sequence
#' @examples 
#' sents <- as.character(tokens(data_corpus_inaugural[1:10], what = "sentence"))
#' tokens <- tokens(sents, removePunct = TRUE)
#' tokens <- tokens_select(tokens, stopwords("english"), "remove", padding = TRUE)
#' types <- unique(as.character(tokens))
#' 
#' # extracting multi-part nouns
#' types_upper <- types[stringi::stri_detect_regex(types, "^([A-Z][a-z\\-]{2,})")]
#' seqs <- findSequences(as.tokenizedTexts(tokens), types_upper, count_min = 2)
#' head(seqs, 10)
#' 
#' # types can be any words
#' types_lower <- types[stringi::stri_detect_regex(types, "^([a-z]+)$") & !types %in%stopwords()]
#' seqs2 <- findSequences(as.tokenizedTexts(tokens), types_lower, count_min = 3)
#' head(seqs2, 10)
#' 
#' @keywords internal collocations
#' @author Kohei Watanabe
#' @references Blaheta, D., & Johnson, M. (2001). Unsupervised learning of
#'   multi-word verbs. Presented at the ACLEACL Workshop on the Computational
#'   Extraction, Analysis and Exploitation of Collocations.
#' @export
findSequences <- function(x, tokens, count_min, smooth=0.001, nested=TRUE){
    
    if(missing(count_min)) count_min <- max(2, length(unlist(tokens)) / 10 ^ 6) # alt least twice of one in million
    
    seqs <- find_sequence_cppl(x, tokens, count_min, smooth, nested)
    seqs$z <- seqs$lambda / seqs$sigma
    seqs$p <- 1 - stats::pnorm(seqs$z)
    seqs$mue <- seqs$lambda - (3.29 * seqs$sigma) # mue should be greater than zero
    class(seqs) <- "tokenSequences"
    seqs
}

# print a tokenSequences objects
# 
# print method for a tokenSequences object
# @param x a tokenSequences object created by \link{findSequences}
# @param ... further arguments passed to base print method

#' @noRd
#' @export
#' @method print tokenSequences
print.tokenSequences <- function(x, ...) {
    df <- data.frame(sequence=sapply(x[['sequence']], paste, collapse = " "),
                     len=sapply(x[['sequence']], length),
                     #lambda=x$lambda,
                     #sigma=x$sigma,
                     z=x$z,
                     p=x$p,
                     mue=x$mu)
    df <- df[order(-df$z),]
    print(df, ...)
}

#' print a tokenSequences objects
#' 
#' print method for a tokenSequences object
#' @param x a tokenSequences object created by \link{findSequences}
#' @param ... further arguments passed to base print method
#' @export
#' @method head tokenSequences
#' @noRd
head.tokenSequences <- function(x, ...) {
  
    df <- data.frame(sequence=sapply(x[['sequence']], paste, collapse = " "),
                     len=sapply(x[['sequence']], length),
                     z=x$z,
                     p=x$p,
                     mue=x$mu)
    df <- df[order(-df$z),]
    print(head(df, ...))
  
}

#' print a tokenSequences objects
#' 
#' print method for a tokenSequences object
#' @param x a tokenSequences object created by \link{findSequences}
#' @param ... further arguments passed to base print method
#' @export
#' @method tail tokenSequences
#' @noRd
tail.tokenSequences <- function(x, ...) {
  
  df <- data.frame(sequence=sapply(x[['sequence']], paste, collapse = " "),
                   len=sapply(x[['sequence']], length),
                   z=x$z,
                   p=x$p,
                   mue=x$mu)
  df <- df[order(-df$z),]
  print(head(df, ...))
  
}