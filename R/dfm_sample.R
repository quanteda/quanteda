#' Randomly sample documents or features from a dfm
#'
#' Sample randomly from a dfm object, from documents or features.
#' @param x the [dfm] object whose documents or features will be sampled
#' @param size a positive number, the number of documents or features to select.
#'   The default is the number of documents or the number of features, for
#'   `margin = "documents"` and `margin = "features"` respectively.
#' @param replace logical; should sampling be with replacement?
#' @param prob a vector of probability weights for obtaining the elements of the
#'   vector being sampled.
#' @param margin dimension (of a [dfm]) to sample: can be `documents` or
#'   `features`
#' @export
#' @return A dfm object with number of documents or features equal to `size`, drawn
#'   from the dfm `x`.
#' @seealso [sample]
#' @keywords dfm
#' @examples
#' set.seed(10)
#' dfmat <- dfm(c("a b c c d", "a a c c d d d"))
#' head(dfmat)
#' head(dfm_sample(dfmat))
#' head(dfm_sample(dfmat, replace = TRUE))
#' head(dfm_sample(dfmat, margin = "features"))
#' head(dfm_sample(dfmat, margin = "features", replace = TRUE))
dfm_sample <- function(x, size = ifelse(margin == "documents", ndoc(x), nfeat(x)),
                       replace = FALSE, prob = NULL,
                       margin = c("documents", "features")) {
    UseMethod("dfm_sample")
}

#' @export
dfm_sample.default <- function(x, size = ifelse(margin == "documents", ndoc(x), nfeat(x)),
                       replace = FALSE, prob = NULL,
                       margin = c("documents", "features")) {
    stop(friendly_class_undefined_message(class(x), "dfm_sample"))
}
    
#' @export
dfm_sample.dfm <- function(x, size = ifelse(margin == "documents", ndoc(x), nfeat(x)),
                       replace = FALSE, prob = NULL,
                       margin = c("documents", "features")) {
    x <- as.dfm(x)
    margin <- match.arg(margin)
    if (margin == "documents") {
        if (size > ndoc(x) && !replace)
            stop("size cannot exceed the number of documents (", ndoc(x), ")")
        x <- x[sample(ndoc(x), size, replace, prob), ]
    } else if (margin == "features") {
        if (size > nfeat(x) && !replace)
            stop("size cannot exceed the number of features (", nfeat(x), ")")
        x <- x[, sample(nfeat(x), size, replace, prob)]
    }
    return(x)
}
