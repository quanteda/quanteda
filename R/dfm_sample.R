
#' @export
#' @param what dimension (of a \link{dfm}) to sample: can be \code{documents} or
#'   \code{features}
#' @return A dfm object with number of documents equal to \code{size}, drawn 
#'   from the corpus \code{x}.  The returned corpus object will contain all of 
#'   the meta-data of the original corpus, and the same document variables for 
#'   the documents selected.
#' @rdname sample
#' @examples
#' # sampling from a dfm
#' myDfm <- dfm(data_char_inaugural[1:10], verbose = FALSE)
#' dfm_sample(myDfm)[, 1:10]
#' dfm_sample(myDfm, replace = TRUE)[, 1:10]
#' dfm_sample(myDfm, what = "features")[1:10, ]
dfm_sample <- function(x, size = ndoc(x), replace = FALSE, prob = NULL, 
                       what = c("documents", "features"), ...) {
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

