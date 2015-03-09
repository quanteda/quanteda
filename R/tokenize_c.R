#' @rdname tokenize
#' @details \code{tokenizeC} is an experimental C version of \code{\link{tokenize}}.
#' @author Kohei Watanabe
#' @importFrom Rcpp evalCpp
#' @useDynLib quanteda
#' @export
#' @export
tokenizeC <- function(x, sep=' ', simplify=FALSE,
                       minLength=1, toLower=TRUE, removeDigits=TRUE, removePunct=TRUE,
                       removeTwitter=TRUE, removeURL=TRUE, removeAdditional='') {
    
    if (simplify) {
        return(tokenizecpp(x, sep, minLength, toLower, removeDigits, removePunct, 
                           removeTwitter, removeURL, removeAdditional))
    } else {
        return(list(tokenizecpp(x, sep, minLength, toLower, removeDigits, removePunct, 
                                removeTwitter, removeURL, removeAdditional)))
    }
}
