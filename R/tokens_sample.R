#' Randomly sample documents or features from a tokens
#' 
#' Sample documents randomly from a tokens object.
#' @param x the tokens object whose documents or features will be sampled
#' @param size a positive number, the number of documents or features to select
#' @param replace logical; should sampling be with replacement?
#' @param prob a vector of probability weights for obtaining the elements of the
#'   vector being sampled.
#' @export
#' @return A tokens object with number of documents or features equal to \code{size}, drawn 
#'   from the tokens \code{x}.  
#' @seealso \link{sample}
#' @examples
#' set.seed(10)
#' toks <- tokens(data_corpus_inaugural[1:10])
#' head(toks)
#' head(tokens_sample(toks))
#' head(tokens_sample(toks, replace = TRUE))
tokens_sample <- function(x, size = ndoc(x), replace = FALSE, prob = NULL) {
    UseMethod("tokens_sample")
}

#' @export
tokens_sample.default <- function(x, size = ndoc(x), replace = FALSE, prob = NULL) {
    stop(friendly_class_undefined_message(class(x), "tokens_sample"))
}
    
#' @export
tokens_sample.tokens <- function(x, size = ndoc(x), replace = FALSE, prob = NULL) {
    
    if (size > ndoc(x) && !replace)
        stop("size cannot exceed the number of documents (", ndoc(x), ")")
    x[sample(ndoc(x), size, replace, prob)]
}
