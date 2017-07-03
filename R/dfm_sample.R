#' randomly sample documents or features from a dfm
#' 
#' Sample randomly from a dfm object, from documents or features.
#' @param x the dfm object whose documents or features will be sampled
#' @param size a positive number, the number of documents or features to select
#' @param replace logical; should sampling be with replacement?
#' @param prob a vector of probability weights for obtaining the elements of the
#'   vector being sampled.
#' @param margin dimension (of a \link{dfm}) to sample: can be \code{documents} or
#'   \code{features}
#' @export
#' @return A dfm object with number of documents or features equal to \code{size}, drawn 
#'   from the dfm \code{x}.  
#' @seealso \link{sample}
#' @examples
#' set.seed(10)
#' myDfm <- dfm(data_corpus_inaugural[1:10])
#' head(myDfm)
#' head(dfm_sample(myDfm))
#' head(dfm_sample(myDfm, replace = TRUE))
#' head(dfm_sample(myDfm, margin = "features"))
dfm_sample <- function(x, size = ndoc(x), replace = FALSE, prob = NULL, 
                       margin = c("documents", "features")) {
    UseMethod("dfm_sample")
}

#' @noRd
#' @export
dfm_sample.dfm <- function(x, size = ndoc(x), replace = FALSE, prob = NULL, 
                       margin = c("documents", "features")) {
    margin <- match.arg(margin)
    if (margin == "documents") {
        if (size > ndoc(x))
            stop("size cannot exceed the number of documents (", ndoc(x), ")")
        x <- x[sample(ndoc(x), size, replace, prob), ]
    } else if (margin == "features") {
        if (size > nfeature(x))
            stop("size cannot exceed the number of features (", nfeature(x), ")")
        x <- x[, sample(nfeature(x), size, replace, prob)]
    } 
    x
}

