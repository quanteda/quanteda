#' This function automatically identify sequences of tokens. This algorithm is   
#' based on Blaheta and Johnson's “Unsupervised Learning of Multi-Word Verbs”.
#' @export
#' @examples 
#' data(SOTUCorpus, package = "quantedaData")
#' sents <- tokenize(SOTUCorpus, what='sentence', simplify = TRUE)
#' tokens <- tokenize(sents, removePunct = TRUE)
#' types <- unique(unlist(tokens))
#' 
#' # Extracting multi-part nouns
#' types_upper <- types[stri_detect_regex(types, "^([A-Z]{2,}|[A-Z][0-9]{1,}|[A-Z][a-z\\-]{2,})")]
#' findSequences(tokens, types_upper)
#'
#' # Types can be any words
#' types_lower <- types[stri_detect_regex(types, "^([a-z]+)$") & !types %in%stopwords()]
#' findSequences(tokens, types_lower, count_min=2)
#' 

findSequences <- function(x, tokens, count_min=5, len_max = 5){
  
  seqs <- find_sequence_cppl(x, tokens, count_min, len_max);
  seqs$z <- seqs$lambda / seqs$sigma
  seqs$p <- 1 - pnorm(seqs$z)
  class(seqs) <- "tokenSequences"
  seqs
}

print.tokenSequences <- function(x){
  
  df <- data.frame(name=sapply(x[['sequence']], paste, collapse = " "),
                   len=sapply(x[['sequence']], length),
                   #lambda=x$lambda,
                   #sigma=x$sigma,
                   z=x$z,
                   p=x$p)
  df <- df[order(df$z),]
  print(df)
}