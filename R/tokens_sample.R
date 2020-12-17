#' Randomly sample documents from a tokens object
#' 
#' Sample tokenized documents randomly from a tokens object, with or without
#' replacement. Works just as [sample()] works, for document-level
#' units (and their associated document-level variables).
#' @param x a [tokens] object whose documents will be sampled
#' @inheritParams corpus_sample
#' @export
#' @return A [tokens] object with number of documents or features equal to
#'   `size`, drawn from the tokens `x`.
#' @seealso [sample]
#' @keywords tokens
#' @examples
#' set.seed(10)
#' toks <- tokens(data_corpus_inaugural[1:10])
#' head(toks)
#' head(tokens_sample(toks))
#' head(tokens_sample(toks, replace = TRUE))
tokens_sample <- function(x, size = NULL, replace = FALSE, prob = NULL, by = NULL) {
    UseMethod("tokens_sample")
}

#' @export
tokens_sample.default <- function(x, size = NULL, replace = FALSE, prob = NULL, by = NULL) {
    stop(friendly_class_undefined_message(class(x), "tokens_sample"))
}
    
#' @export
tokens_sample.tokens <- function(x, size = NULL, replace = FALSE, prob = NULL, by = NULL) {
    
    x <- as.tokens(x)
    
    if (!is.null(by)) {
        if (by == "document") by <- "docid_"
        i <- resample(seq_len(ndoc(x)), size = size, replace = replace, prob = prob,
                      by = get_docvars(x, by, system = TRUE, drop = TRUE))
    } else {
        i <- resample(seq_len(ndoc(x)), size = size, replace = replace, prob = prob) 
    }
    return(x[i])
}
