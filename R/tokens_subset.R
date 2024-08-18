#' Extract a subset of a tokens
#'
#' Returns document subsets of a tokens that meet certain conditions, including
#' direct logical operations on docvars (document-level variables).
#' `tokens_subset()` functions identically to [subset.data.frame()], using
#' non-standard evaluation to evaluate conditions based on the [docvars] in the
#' tokens.
#'
#' @param x [tokens] object to be subsetted.
#' @param min_ntoken,max_ntoken minimum and maximum lengths of the documents to extract.
#' @inheritParams messages
#' @inheritParams corpus_subset
#' @return [tokens] object, with a subset of documents (and docvars)
#'   selected according to arguments
#' @export
#' @seealso [subset.data.frame()]
#' @keywords tokens
#' @examples
#' corp <- corpus(c(d1 = "a b c d", d2 = "a a b e",
#'                  d3 = "b b c e", d4 = "e e f a b"),
#'                  docvars = data.frame(grp = c(1, 1, 2, 3)))
#' toks <- tokens(corp)
#' # selecting on a docvars condition
#' tokens_subset(toks, grp > 1)
#' # selecting on a supplied vector
#' tokens_subset(toks, c(TRUE, FALSE, TRUE, FALSE))
tokens_subset <- function(x, subset, min_ntoken = NULL, max_ntoken = NULL, 
                          drop_docid = TRUE, verbose = quanteda_options("verbose"), ...) {
    UseMethod("tokens_subset")
}
    
#' @export
tokens_subset.default <- function(x, subset, min_ntoken = NULL, max_ntoken = NULL, 
                                  drop_docid = TRUE, verbose = quanteda_options("verbose"), ...) {
    check_class(class(x), "tokens_subset")
}
    
#' @export
tokens_subset.tokens <- function(x, subset, min_ntoken = NULL, max_ntoken = NULL, 
                                 drop_docid = TRUE, verbose = quanteda_options("verbose"), ...) {
    
    x <- as.tokens(x)
    min_ntoken <- check_integer(min_ntoken, min = 0, allow_null = TRUE)
    max_ntoken <- check_integer(max_ntoken, min = 0, allow_null = TRUE)
    verbose <- check_logical(verbose)
    check_dots(...)
    
    attrs <- attributes(x)
    docvar <- get_docvars(x, user = TRUE, system = TRUE)
    r <- if (missing(subset)) {
        rep_len(TRUE, ndoc(x))
    } else {
        e <- substitute(subset)
        r <- eval(e, docvar, parent.frame())
        r & !is.na(r)
    }
    
    l <- if (is.null(min_ntoken) && is.null(max_ntoken)) {
        rep_len(TRUE, ndoc(x))
    } else {
        n <- ntoken(x)
        if (is.null(min_ntoken)) min_ntoken <- 0L
        if (is.null(max_ntoken)) max_ntoken <- max(n)
        min_ntoken <= n & n <= max_ntoken
    }
    if (verbose)
        before <- stats_tokens(x)
    x <- x[r & l, drop_docid = drop_docid]
    if (verbose)
        message_tokens("tokens_subset()", before, stats_tokens(x))
    return(x)
}
