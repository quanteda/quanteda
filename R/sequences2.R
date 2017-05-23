#' find variable-length collocations with filtering
#' 
#' This function automatically identifies contiguous collocations consisting of 
#' variable-length term sequences whose frequency is unlikely to have occurred 
#' by chance.  The algorithm is based on Blaheta and Johnson's (2001) 
#' "Unsupervised Learning of Multi-Word Verbs".
#' @param x a \link{tokens} object
#' @param min_count minimum frequency of sequences for which parameters are 
#'   estimated
#' @param max_size maxium length of sequences which are collected
#' @param nested if \code{TRUE}, collect all the subsequences of a longer
#'   sequence as separate entities. e.g. in a sequence of capitalized words
#'   "United States Congress", "States Congress" is considered as a subsequence.
#'   But "United States" is not a subsequence because it is followed by
#'   "Congress".
#' @param ordered if true, use the Blaheta-Johnson method that distinguishes 
#'   between the order of words, and tends to promote rare sequences.
#' @keywords collocations internal
#' @author Kohei Watanabe
#' @references Blaheta, D., & Johnson, M. (2001). 
#'   \href{http://web.science.mq.edu.au/~mjohnson/papers/2001/dpb-colloc01.pdf}{Unsupervised
#'    learning of multi-word verbs}. Presented at the ACLEACL Workshop on the 
#'   Computational Extraction, Analysis and Exploitation of Collocations.
#' @examples 
#' toks <- tokens(corpus_segment(data_corpus_inaugural, what = "sentence"))
#' toks <- tokens_select(toks, stopwords("english"), "remove", padding = TRUE)
#' 
#' # extracting multi-part proper nouns (capitalized terms)
#' seqs <- sequences(toks)
#' head(seqs, 10)
#' 
#' # more efficient when applied to the same tokens object 
#' toks_comp <- tokens_compound(toks, seqs)
#' toks_comp_ir <- tokens_compound(tokens(data_corpus_irishbudget2010), seqs)
#' 
#' # types can be any words
#' seqs2 <- sequences(toks, min_count = 2, ordered = TRUE)
#'                    
#' head(seqs2, 10)
#' 
#' # convert to tokens object
#' as.tokens(seqs2)
#' 
#' @export
sequences2 <- function(x, 
                      min_count = 2, 
                      max_size = 5, 
                      nested = TRUE, ordered = FALSE) {
    
    # .Deprecated('textstat_collocations')
    UseMethod("sequences")
}

#' @rdname sequences
#' @noRd
#' @export
sequences2.tokens <- function(x,
                             min_count = 2, 
                             max_size= 5, 
                             nested=TRUE, ordered=FALSE) {
    
    attrs_org <- attributes(x)
    
    types <- types(x)
 
    result <- qatd_cpp_sequences(x, types, min_count, max_size, nested, ordered)
    result <- result[result$count >= min_count,]
    result$z <- result$lambda / result$sigma
    result$p <- 1 - stats::pnorm(result$z)
    result <- result[order(result$z, decreasing = TRUE),]
    attr(result, 'types') <- types
    class(result) <- c("sequences", 'data.frame')
    
    return(result)
}


#' @method "[" sequences
#' @export
#' @noRd
"[.sequences" <- function(x, i, ...) {
    x <- as.data.frame(x)[i,]
    attr(x, 'ids') <- attr(x, 'ids')[i]
    class(x) <- c("sequences", 'data.frame')
    return(x)
}

#' @export
#' @method as.tokens sequences
#' @noRd 
as.tokens.sequences <- function(x) {
    toks <- attr(x, 'tokens')
    attr(toks, 'types') <- attr(x, 'types')
    class(toks) <- c("tokens", "tokenizedTexts")
    return(toks)
}

#' @rdname sequences
#' @export
#' @return \code{sequences} returns \code{TRUE} if the object is of class
#'   sequences, \code{FALSE} otherwise.
is.sequences <- function(x) {
    ifelse("sequences" %in% class(x), TRUE, FALSE)
}



