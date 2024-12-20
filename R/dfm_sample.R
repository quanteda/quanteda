#' Randomly sample documents from a dfm
#'
#' Take a random sample of documents of the specified size from a dfm, with
#' or without replacement, optionally by grouping variables or with probability
#' weights.
#' @param x the [dfm] object whose documents will be sampled
#' @inheritParams corpus_sample
#' @inheritParams messages
#' @export
#' @return a [dfm] object (re)sampled on the documents, containing the document
#'   variables for the documents sampled.
#' @seealso [sample]
#' @keywords dfm
#' @examples
#' set.seed(10)
#' dfmat <- dfm(tokens(c("a b c c d", "a a c c d d d", "a b b c")))
#' dfmat
#' dfm_sample(dfmat)
#' dfm_sample(dfmat, replace = TRUE)
#'
#' # by groups
#' dfmat <- dfm(tokens(data_corpus_inaugural[50:58]))
#' dfm_sample(dfmat, by = Party, size = 2)
dfm_sample <- function(x, size = NULL, replace = FALSE, prob = NULL, by = NULL,
                       verbose = quanteda_options("verbose")) {
    UseMethod("dfm_sample")
}

#' @export
dfm_sample.default <- function(x, size = NULL, replace = FALSE, prob = NULL, by = NULL,
                               verbose = quanteda_options("verbose")) {
    check_class(class(x), "dfm_sample")
}
    
#' @export
dfm_sample.dfm <- function(x, size = NULL, replace = FALSE, prob = NULL, by = NULL,
                           verbose = quanteda_options("verbose")) {
    verbose <- check_logical(verbose)
    x <- as.dfm(x)

    if (!missing(by)) {
        by <- eval(substitute(by), get_docvars(x, user = TRUE, system = TRUE), parent.frame())
        if (is.factor(by)) by <- droplevels(by)
    }

    i <- resample(seq_len(ndoc(x)), size = size, replace = replace, prob = prob, by = by)

    if (verbose)
        before <- stats_dfm(x)
    result <- x[i, ]
    if (verbose)
        message_dfm("dfm_sample()", before, stats_dfm(result))
    return(result)    
}
