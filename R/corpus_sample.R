#' randomly sample documents from a corpus
#' 
#' Takes a random sample or documents or features of the specified size from a 
#' corpus or document-feature matrix, with or without replacement.  Works just 
#' as \code{\link{sample}} works for the documents and their associated
#' document-level variables.
#' @param x a corpus object whose documents will be sampled
#' @param size a positive number, the number of documents to select
#' @param replace Should sampling be with replacement?
#' @param prob A vector of probability weights for obtaining the elements of the
#'   vector being sampled.
#' @param ... unused
#' @return A corpus object with number of documents equal to \code{size}, drawn 
#'   from the corpus \code{x}.  The returned corpus object will contain all of 
#'   the meta-data of the original corpus, and the same document variables for 
#'   the documents selected.
#' @export
#' @seealso \code{\link{sample}}
#' @examples
#' # sampling from a corpus
#' summary(corpus_sample(data_corpus_inaugural, 5)) 
#' summary(corpus_sample(data_corpus_inaugural, 10, replace=TRUE))
corpus_sample <- function(x, size = ndoc(x), replace = FALSE, prob = NULL, ...) {
    documents(x) <- documents(x)[base::sample(ndoc(x), size, replace, prob), , drop = FALSE]
    x
}

#' randomly sample documents or features
#' 
#' Deprecated function framework for randomly sampling from a corpus or dfm object.  Use
#' \code{\link{corpus_sample}} or \code{dfm_sample} instead
#' @param x a corpus or dfm object whose documents or features will be sampled
#' @param size a positive number, the number of documents to select
#' @param replace Should sampling be with replacement?
#' @param prob A vector of probability weights for obtaining the elements of the
#'   vector being sampled.
#' @param ... unused
#' @seealso corpus_sample
#' @keywords internal
#' @export
sample <- function(x, size, replace = FALSE, prob = NULL, ...) {
    UseMethod("sample")
}

#' @rdname sample
#' @export
sample.default <- function(x, size, replace = FALSE, prob = NULL, ...) {
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    base::sample(x, size, replace, prob)
}

#' @rdname sample
#' @export
sample.corpus <- function(x, ...) {
    .Deprecated("corpus_sample")
    corpus_sample(x, ...)
}

