#' randomly sample documents or features from a dfm
#' 
#' Sample randomly from a dfm object, from documents or features.
#' @param x the dfm object whose documents or features will be sampled
#' @param size a positive number, the number of documents or features to select
#' @param replace logical; should sampling be with replacement?
#' @param prob a vector of probability weights for obtaining the elements of the
#'   vector being sampled.
#' @param what dimension (of a \link{dfm}) to sample: can be \code{documents} or
#'   \code{features}
#' @export
#' @return A dfm object with number of documents or features equal to \code{size}, drawn 
#'   from the dfm \code{x}.  
#' @seealso \link{sample}
#' @examples
#' myDfm <- dfm(data_char_inaugural[1:10], verbose = FALSE)
#' dfm_sample(myDfm)[, 1:10]
#' dfm_sample(myDfm, replace = TRUE)[, 1:10]
#' dfm_sample(myDfm, what = "features")[1:10, ]
dfm_sample <- function(x, size = ndoc(x), replace = FALSE, prob = NULL, 
                       what = c("documents", "features")) {
    if (!is.dfm(x))
        stop("x must be a dfm object")
    what <- match.arg(what)
    if (what == "documents") {
        if (size > ndoc(x))
            stop("size cannot exceed the number of documents (", ndoc(x), ")")
        x <- x[sample(ndoc(x), size, replace, prob), ]
    } else if (what == "features") {
        if (size > nfeature(x))
            stop("size cannot exceed the number of features (", nfeature(x), ")")
        x <- x[, sample(nfeature(x), size, replace, prob)]
    } else {
        stop("only documents or features please")
    }
    x
}

