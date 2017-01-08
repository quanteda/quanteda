#' find variable-length collocations with filtering
#' 
#' This function automatically identifies contiguous collocations consisting of
#' variable-length term sequences whose frequency is unlikey to have occurred by
#' chance.  The algorithm is based on Blaheta and Johnson's "Unsupervised 
#' Learning of Multi-Word Verbs".
#' @param x a \link{tokens} object
#' @param features a regular expression for filtering the features to be located
#'   in sequences
#' @inheritParams valuetype
#' @param case_insensitive ignore case when matching, if \code{TRUE}
#' @param count_min minimum frequency of sequences
#' @param nested collect nested sequences
#' @examples 
#' toks <- tokens(corpus_segment(data_corpus_inaugural, what = "sentence"))
#' toks <- tokens_select(toks, stopwords("english"), "remove", padding = TRUE)
#' 
#' # extracting multi-part proper nouns (capitalized terms)
#' seqs <- sequences(toks, "^([A-Z][a-z\\-]{2,})", valuetype="regex", case_insensitive = FALSE)
#' head(seqs, 10)
#' 
#' # types can be any words
#' seqs2 <- sequences(toks, "^([a-z]+)$", valuetype="regex", case_insensitive = FALSE, count_min = 10)
#' head(seqs2, 10)
#' 
#' @keywords collocations
#' @author Kohei Watanabe
#' @references Blaheta, D., & Johnson, M. (2001). 
#'   \href{http://web.science.mq.edu.au/~mjohnson/papers/2001/dpb-colloc01.pdf}{Unsupervised
#'    learning of multi-word verbs}. Presented at the ACLEACL Workshop on the 
#'   Computational Extraction, Analysis and Exploitation of Collocations.
#' @export
sequences <- function(x, features, valuetype = c("glob", "regex", "fixed"),
                      case_insensitive = TRUE, count_min = 2, nested=TRUE) {
    UseMethod("sequences")
}

#' @rdname sequences
#' @noRd
#' @export
sequences.tokens <- function(x, features, valuetype = c("glob", "regex", "fixed"),
                      case_insensitive = TRUE, count_min = 2, nested=TRUE) {
    
    valuetype <- match.arg(valuetype)
    names_org <- names(x)
    attrs_org <- attributes(x)
    
    types <- types(x)
    features <- unlist(features, use.names = FALSE) # this funciton does not accpet list
    features_id <- unlist(regex2id(features, types, valuetype, case_insensitive, FALSE), use.names = FALSE)

    seqs <- qutd_cpp_sequences(x, features_id, count_min, nested)
    seqs$length <- lengths(seqs$sequence)
    seqs$sequence <- sapply(seqs$sequence, function(y) stringi::stri_c(types[y], collapse = ' '))
    
    df <- as.data.frame(seqs, stringsAsFactors = FALSE)
    df <- df[df$count >= count_min,]
    df$z <- df$lambda / df$sigma
    df$p <- 1 - stats::pnorm(df$z)
    df <- df[order(df$z, decreasing = TRUE),]
    class(df) <- c("sequences", class(df))
    rownames(df) <- NULL
    return(df)
}

