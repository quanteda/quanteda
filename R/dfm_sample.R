#' Randomly sample documents or features from a dfm
#'
#' Sample randomly from a dfm object, from documents or features.
#' @param x the [dfm] object whose documents or features will be sampled
#' @param size a positive number, the number of documents or features to select.
#'   The default is the number of documents or the number of features, for
#'   `margin = "documents"` and `margin = "features"` respectively.
#' @param margin dimension (of a [dfm]) to sample: can be `documents` or
#'   `features`. This argument is deprecated.
#' @inheritParams corpus_sample
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
dfm_sample <- function(x, size = NULL, replace = FALSE, prob = NULL, by = NULL,
                       margin = c("documents", "features")) {
    UseMethod("dfm_sample")
}

#' @export
dfm_sample.default <- function(x, size = NULL, replace = FALSE, prob = NULL, by = NULL,
                               margin = c("documents", "features")) {
    stop(check_class(class(x), "dfm_sample"))
}
    
#' @export
dfm_sample.dfm <- function(x, size = NULL, replace = FALSE, prob = NULL, by = NULL,
                           margin = c("documents", "features")) {
     
    x <- as.dfm(x)
    margin <- match.arg(margin)
    
    if (margin == "features")
        .Deprecated(msg = 'margin is deprecated')
    
    if (margin == "documents") {
        if (!is.null(by)) {
            if (by == "document") by <- "docid_"
            i <- resample(seq_len(ndoc(x)), size = size, replace = replace, prob = prob,
                          by = get_docvars(x, by, system = TRUE, drop = TRUE))
        } else {
            i <- resample(seq_len(ndoc(x)), size = size, replace = replace, prob = prob) 
        }
        x <- x[i,]
    } else if (margin == "features") {
        j <- resample(seq_len(nfeat(x)), size = size, replace = replace, prob = prob)
        x <- x[,j]
    }
    return(x)
}
