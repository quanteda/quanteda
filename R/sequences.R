

#' find sequences of tokens
#' 
#' This function automatically identify sequences of words (contiguous collocations). 
#' This algorithm is based on Blaheta and Johnson's “Unsupervised Learning of Multi-Word Verbs”.
#' @param x tokens objects
#' @param features features in sequences
#' @inheritParams valuetype
#' @param case_insensitive ignore case when matching, if \code{TRUE}
#' @param count_min minimum frequency of sequences
#' @param nested collect nested sequences
#' @examples 
#' 
#' toks <- tokens(corpus_segment(data_corpus_inaugural, what = "sentence"))
#' toks <- tokens_select(toks, stopwords("english"), "remove", padding = TRUE)
#' 
#' # extracting multi-part nouns
#' seqs <- sequences(toks, "^([A-Z][a-z\\-]{2,})", valuetype="regex", case_insensitive = FALSE)
#' head(seqs, 10)
#' 
#' # types can be any words
#' seqs2 <- sequences(toks, "^([a-z]+)$", valuetype="regex", case_insensitive = FALSE, count_min = 10)
#' head(seqs2, 10)
#' 
#' @keywords collocations
#' @author Kohei Watanabe
#' @references Blaheta, D., & Johnson, M. (2001). Unsupervised learning of
#'   multi-word verbs. Presented at the ACLEACL Workshop on the Computational
#'   Extraction, Analysis and Exploitation of Collocations.
#' @export
sequences <- function(x, features, valuetype = c("glob", "regex", "fixed"),
                      case_insensitive = TRUE, count_min = 2, nested=TRUE) {
    
    valuetype <- match.arg(valuetype)
    
    names_org <- names(x)
    attrs_org <- attributes(x)
    
    types <- types(x)
    features <- as.list(unlist(features, use.names = FALSE)) # does not accpet sequences
    features_fixed <- regex2fixed5(features, types, valuetype, case_insensitive, FALSE) # convert glob or regex to fixed
    features_id <- sapply(features_fixed, function(x) fastmatch::fmatch(x, types))
    
    seqs <- qutd_cpp_sequences(x, features_id, count_min, nested)
    seqs$length <- lengths(seqs$sequence)
    seqs$sequence <- sapply(seqs$sequence, function(x) stringi::stri_c(types[x], collapse = ' '))
    
    df <- as.data.frame(seqs, stringsAsFactors = FALSE)
    df <- df[df$count >= count_min,]
    df$z <- df$lambda / df$sigma
    df$p <- 1 - stats::pnorm(df$z)
    df <- df[order(df$z, decreasing = TRUE),]
    class(df) <- c("sequences", class(df))
    
    return(df)
}

