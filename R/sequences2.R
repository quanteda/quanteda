#' Find variable-length collocations 
#' 
#' This function automatically identifies contiguous collocations consisting of 
#' variable-length term sequences whose frequency is unlikely to have occurred 
#' by chance.  The algorithm is based on Blaheta and Johnson's (2001) 
#' "Unsupervised Learning of Multi-Word Verbs".
#' @param x a \link{tokens} object
#' @param min_count minimum frequency of sequences for which parameters are 
#'   estimated
#' @param min_size minimum length of sequences which are collected  
#' @param max_size maximum length of sequences which are collected
#' @param method default is "unigram" and option is "all_subtuples"
#' @param nested if \code{TRUE}, collect all the subsequences of a longer
#'   sequence as separate entities. e.g. in a sequence of capitalized words
#'   "United States Congress", "States Congress" is considered as a subsequence.
#'   But "United States" is not a subsequence because it is followed by
#'   "Congress".
#' @keywords collocations internal
#' @author Kohei Watanabe
#' @references Blaheta, D., & Johnson, M. (2001). 
#'   \href{http://web.science.mq.edu.au/~mjohnson/papers/2001/dpb-colloc01.pdf}{Unsupervised
#'    learning of multi-word verbs}. Presented at the ACLEACL Workshop on the 
#'   Computational Extraction, Analysis and Exploitation of Collocations.
#' @examples 
#' toks <- tokens(corpus_segment(data_corpus_inaugural, what = "sentence"), remove_punct=TRUE)
#' toks <- tokens_select(toks, stopwords("english"), "remove", padding = TRUE)
#' # extracting multi-part proper nouns (capitalized terms)
#' toks <- tokens_select(toks, "^([A-Z][a-z\\-]{2,})", valuetype="regex", 
#'                      case_insensitive = FALSE, padding = TRUE)
#' 
#' seqs <- sequences2(toks)
#' head(seqs, 10)
#' # to return only trigrams
#' seqs <- sequences2(toks, min_size = 3, max_size = 3)
#' head(seqs, 10)
#' @export
sequences2 <- function(x, 
                       min_count = 2,
                       min_size = 2,
                       max_size = 5,
                       method = c("unigram", "all_subtuples"),
                       nested = TRUE) {
    
    # .Deprecated('textstat_collocations')
    UseMethod("sequences2")
}

#' @rdname sequences2
#' @noRd
#' @export
sequences2.tokens <- function(x,
                              min_count = 2,
                              min_size = 2,
                              max_size = 5,
                              method = c("unigram", "all_subtuples"),
                              nested = TRUE) {
    
    attrs_org <- attributes(x)
    methodtype = match.arg(method)
    types <- types(x)
    
    result <- qatd_cpp_sequences2(x, types, min_count, min_size, max_size, methodtype, nested)
    result <- result[result$count >= min_count,]
    result$z <- result$lambda / result$sigma
    result$p <- 1 - stats::pnorm(result$z)
    result <- result[order(result$z, decreasing = TRUE),]
    attr(result, 'types') <- types
    class(result) <- c("sequences2", 'data.frame')
    
    return(result)
}

#' @method "[" sequences2
#' @export
#' @noRd
"[.sequences2" <- function(x, i, ...) {
    x <- as.data.frame(x)[i,]
    attr(x, 'ids') <- attr(x, 'ids')[i]
    class(x) <- c("sequences2", 'data.frame')
    return(x)
}

#' @export
#' @method as.tokens sequences2
#' @noRd 
as.tokens.sequences2 <- function(x) {
    toks <- attr(x, 'tokens')
    attr(toks, 'types') <- attr(x, 'types')
    class(toks) <- c("tokens", "tokenizedTexts")
    return(toks)
}

#' @rdname sequences
#' @export
#' @return \code{sequences} returns \code{TRUE} if the object is of class
#'   sequences, \code{FALSE} otherwise.
is.sequences2 <- function(x) {
    ifelse("sequences2" %in% class(x), TRUE, FALSE)
}
