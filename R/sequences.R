#' find variable-length collocations with filtering
#' 
#' This function automatically identifies contiguous collocations consisting of 
#' variable-length term sequences whose frequency is unlikely to have occurred 
#' by chance.  The algorithm is based on Blaheta and Johnson's (2001) 
#' "Unsupervised Learning of Multi-Word Verbs".
#' @param x a \link{tokens} object
#' @param features a regular expression for filtering the features to be located
#'   in sequences
#' @inheritParams valuetype
#' @param case_insensitive ignore case when matching, if \code{TRUE}
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
#' seqs <- sequences(toks, "^([A-Z][a-z\\-]{2,})", valuetype="regex", case_insensitive = FALSE)
#' head(seqs, 10)
#' 
#' # more efficient when applied to the same tokens object 
#' toks_comp <- tokens_compound(toks, seqs)
#' toks_comp_ir <- tokens_compound(tokens(data_corpus_irishbudget2010), seqs)
#' 
#' # types can be any words
#' seqs2 <- sequences(toks, "^([a-z]+)$", valuetype="regex", case_insensitive = FALSE, 
#'                    min_count = 2, ordered = TRUE)
#'                    
#' head(seqs2, 10)
#' 
#' # convert to tokens object
#' as.tokens(seqs2)
#' 
#' @export
sequences <- function(x, features = "*", 
                      valuetype = c("glob", "regex", "fixed"),
                      case_insensitive = TRUE, 
                      min_count = 2, 
                      max_size = 5, 
                      nested = TRUE, ordered = FALSE) {
    
    # .Deprecated('textstat_collocations')
    UseMethod("sequences")
}

#' @rdname sequences
#' @noRd
#' @export
sequences.tokens <- function(x, features = "*", 
                             valuetype = c("glob", "regex", "fixed"),
                             case_insensitive = TRUE, 
                             min_count = 2, 
                             max_size= 5, 
                             nested=TRUE, ordered=FALSE) {
    
    valuetype <- match.arg(valuetype)
    attrs_org <- attributes(x)
    
    types <- types(x)
    features <- features2vector(features)
    features_id <- unlist(regex2id(features, types, valuetype, case_insensitive, FALSE), use.names = FALSE)
    
    result <- qatd_cpp_sequences(x, features_id, types, min_count, max_size, nested, ordered)
    result <- result[result$count >= min_count,]
    result$z <- result$lambda / result$sigma
    result$p <- 1 - stats::pnorm(result$z)
    result <- result[order(result$z, decreasing = TRUE),]
    attr(result, 'types') <- types
    class(result) <- c("sequences", 'data.frame')
    
    return(result)
}


#' #' @method "[" sequences
#' #' @export
#' #' @noRd
#' "[.sequences" <- function(x, i, ...) {
#'     x <- as.data.frame(x)[i,]
#'     attr(x, 'ids') <- attr(x, 'ids')[i]
#'     class(x) <- c("sequences", 'data.frame')
#'     return(x)
#' }
#' 
#' #' @export
#' #' @method as.tokens sequences
#' #' @noRd 
#' as.tokens.sequences <- function(x) {
#'     toks <- attr(x, 'tokens')
#'     attr(toks, 'types') <- attr(x, 'types')
#'     class(toks) <- c("tokens", "tokenizedTexts")
#'     return(toks)
#' }
#' 
#' #' @rdname sequences
#' #' @export
#' #' @return \code{sequences} returns \code{TRUE} if the object is of class
#' #'   sequences, \code{FALSE} otherwise.
#' is.sequences <- function(x) {
#'     ifelse("sequences" %in% class(x), TRUE, FALSE)
#' }
#' 


