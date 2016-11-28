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
#' @keywords internal deprecated
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
