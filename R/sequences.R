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
#' @param min_count minimum frequency of sequences for which parameters are estimated
#' @param max_length maxium length of sequences which are collected
#' @param nested if true, collect all the subsequences of a longer sequence as separate entities.
#'   e.g. in a sequence of capitalized words "United States Congress", "States Congress" is considered 
#'   as a subsequence. But "United States" is not a subsequence because it is followed by "Congress".
#' @param ordered if true, use the Blaheta-Johnson method that distinguishs between 
#'   the order of words, and tends to promote rare sequences. 
#' @examples 
#' toks <- tokens(corpus_segment(data_corpus_inaugural, what = "sentence"))
#' toks <- tokens_select(toks, stopwords("english"), "remove", padding = TRUE)
#' 
#' # extracting multi-part proper nouns (capitalized terms)
#' seqs <- sequences(toks, "^([A-Z][a-z\\-]{2,})", valuetype="regex", case_insensitive = FALSE)
#' head(seqs, 10)
#' 
#' # types can be any words
#' seqs2 <- sequences(toks, "^([a-z]+)$", valuetype="regex", case_insensitive = FALSE, 
#'                    min_count = 2, ordered = TRUE)
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
                      case_insensitive = TRUE, min_count = 2, max_length= 5, nested=TRUE, ordered=FALSE) {
    UseMethod("sequences")
}

#' @rdname sequences
#' @noRd
#' @export
sequences.tokens <- function(x, features, valuetype = c("glob", "regex", "fixed"),
                      case_insensitive = TRUE, min_count = 2, max_length= 5, nested=TRUE, ordered=FALSE) {
    
    valuetype <- match.arg(valuetype)
    names_org <- names(x)
    attrs_org <- attributes(x)
    
    types <- types(x)
    features <- unlist(features, use.names = FALSE) # this funciton does not accpet list
    features_id <- unlist(regex2id(features, types, valuetype, case_insensitive, FALSE), use.names = FALSE)

    seqs <- qatd_cpp_sequences(x, features_id, min_count, max_length, nested, ordered)
    seqs$length <- lengths(seqs$sequence)
    seqs$sequence <- stringi::stri_c_list(lapply(seqs$sequence, function(y) types[y]), sep=' ')
    
    df <- as.data.frame(seqs, stringsAsFactors = FALSE)
    df <- df[df$count >= min_count,]
    df$z <- df$lambda / df$sigma
    df$p <- 1 - stats::pnorm(df$z)
    df <- df[order(df$z, decreasing = TRUE),]
    class(df) <- c("sequences", class(df))
    rownames(df) <- NULL
    return(df)
}

