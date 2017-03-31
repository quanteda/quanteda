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
#' @keywords collocations
#' @author Kohei Watanabe
#' @references Blaheta, D., & Johnson, M. (2001). 
#'   \href{http://web.science.mq.edu.au/~mjohnson/papers/2001/dpb-colloc01.pdf}{Unsupervised
#'    learning of multi-word verbs}. Presented at the ACLEACL Workshop on the 
#'   Computational Extraction, Analysis and Exploitation of Collocations.
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
    features <- unlist(features, use.names = FALSE) # this funciton does not accpet list
    features_id <- unlist(regex2id(features, types, valuetype, case_insensitive, FALSE), use.names = FALSE)
    
    result <- qatd_cpp_sequences(x, features_id, min_count, max_size, nested, ordered)
    rownames(result) <- unlist(stringi::stri_c_list(lapply(attr(result, 'ids'), function(y) types[y]), sep=' '), use.names = FALSE)
    class(result) <- c("sequences", 'data.frame')
    
    result <- result[result$count >= min_count,]
    result$z <- result$lambda / result$sigma
    result$p <- 1 - stats::pnorm(result$z)
    result <- result[order(result$z, decreasing = TRUE),]
    attr(result, 'types') <- types
    
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
    toks <- attr(x, 'ids')
    attr(toks, 'types') <- attr(x, 'types')
    class(toks) <- c("tokens", "tokenizedTexts")
    return(toks)
}



