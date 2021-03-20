#' Randomly sample documents from a tokens object
#'
#' Take a random sample of documents of the specified size from a corpus, with
#' or without replacement, optionally by grouping variables or with probability
#' weights.
#' @param x a [tokens] object whose documents will be sampled
#' @inheritParams corpus_sample
#' @export
#' @return a [tokens] object (re)sampled on the documents, containing the document
#'   variables for the documents sampled.
#' @seealso [sample]
#' @keywords tokens
#' @examples
#' set.seed(123)
#' toks <- tokens(data_corpus_inaugural[1:6])
#' toks
#' tokens_sample(toks)
#' tokens_sample(toks, replace = TRUE) %>% docnames()
#' tokens_sample(toks, size = 3, replace = TRUE) %>% docnames()
#'
#' # sampling using by
#' docvars(toks)
#' tokens_sample(toks, size = 2, replace = TRUE, by = Party) %>% docnames()
#'
tokens_sample <- function(x, size = NULL, replace = FALSE, prob = NULL, by = NULL) {
    UseMethod("tokens_sample")
}

#' @export
tokens_sample.default <- function(x, size = NULL, replace = FALSE, prob = NULL, by = NULL) {
    check_class(class(x), "tokens_sample")
}
    
#' @export
tokens_sample.tokens <- function(x, size = NULL, replace = FALSE, prob = NULL, by = NULL) {
    x <- as.tokens(x)

    if (!missing(by)) {
        by <- eval(substitute(by), get_docvars(x, user = TRUE, system = TRUE), parent.frame())
        if (is.factor(by)) by <- droplevels(by)
    }

    i <- resample(seq_len(ndoc(x)), size = size, replace = replace, prob = prob, by = by)
    return(x[i])
}
