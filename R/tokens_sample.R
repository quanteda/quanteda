#' Randomly sample documents from a tokens object
#'
#' Take a random sample of documents of the specified size from a corpus, with
#' or without replacement, optionally by grouping variables or with probability
#' weights.
#' @param x a [tokens] object whose documents will be sampled
#' @param env an environment or a list object in which `x` is searched.
#' Passed to [substitute] for non-standard evaluation.
#' @inheritParams corpus_sample
#' @inheritParams messages
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
#' tokens_sample(toks, replace = TRUE) |> docnames()
#' tokens_sample(toks, size = 3, replace = TRUE) |> docnames()
#'
#' # sampling using by
#' docvars(toks)
#' tokens_sample(toks, size = 2, replace = TRUE, by = Party) |> docnames()
#'
tokens_sample <- function(x, size = NULL, replace = FALSE, prob = NULL, by = NULL, 
                          env = NULL, verbose = quanteda_options("verbose")) {
    UseMethod("tokens_sample")
}

#' @export
tokens_sample.default <- function(x, size = NULL, replace = FALSE, prob = NULL, 
                                  by = NULL, env = NULL, 
                                  verbose = quanteda_options("verbose")) {
    check_class(class(x), "tokens_sample")
}
    
#' @export
tokens_sample.tokens_xptr <- function(x, size = NULL, replace = FALSE, prob = NULL, 
                                      by = NULL, env = NULL, 
                                      verbose = quanteda_options("verbose")) {
    
    verbose <- check_logical(verbose)
    if (is.null(env))
        env <- parent.frame()
    if (!missing(by)) {
        by <- eval(substitute(by), get_docvars(x, user = TRUE, system = TRUE), env)
        if (is.factor(by)) by <- droplevels(by)
    }
    i <- resample(seq_len(ndoc(x)), size = size, replace = replace, prob = prob, by = by)
    if (verbose)
        before <- stats_tokens(x)
    result <- x[i]
    if (verbose)
        message_tokens("tokens_sample()", before, stats_tokens(result))
    return(result)
}

#' @export
tokens_sample.tokens <- function(x, ..., env = NULL) {
    if (is.null(env))
        env <- parent.frame()
    as.tokens(tokens_sample(as.tokens_xptr(x), ..., env = env))
}
