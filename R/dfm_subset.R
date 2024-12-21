#' Extract a subset of a dfm
#' 
#' Returns document subsets of a dfm that meet certain conditions,
#' including direct logical operations on docvars (document-level variables). 
#' `dfm_subset` functions identically to [subset.data.frame()],
#' using non-standard evaluation to evaluate conditions based on the
#' [docvars] in the dfm.
#' 
#' To select or subset *features*, see [dfm_select()] instead.
#' @param x [dfm] object to be subsetted.
#' @inheritParams corpus_subset
#' @inheritParams tokens_subset
#' @inheritParams messages
#' @return [dfm] object, with a subset of documents (and docvars) selected
#'   according to arguments
#' @export
#' @keywords dfm
#' @examples
#' corp <- corpus(c(d1 = "a b c d", d2 = "a a b e",
#'                  d3 = "b b c e", d4 = "e e f a b"),
#'                docvars = data.frame(grp = c(1, 1, 2, 3)))
#' dfmat <- dfm(tokens(corp))
#' # selecting on a docvars condition
#' dfm_subset(dfmat, grp > 1)
#' # selecting on a supplied vector
#' dfm_subset(dfmat, c(TRUE, FALSE, TRUE, FALSE))
dfm_subset <- function(x, subset, min_ntoken = NULL, max_ntoken = NULL, 
                       drop_docid = TRUE, verbose = quanteda_options("verbose"), 
                       ...) {
    UseMethod("dfm_subset")
}
    
#' @export
dfm_subset.default <- function(x, subset, min_ntoken = NULL, max_ntoken = NULL, 
                               drop_docid = TRUE, 
                               verbose = quanteda_options("verbose"),
                               ...) {
    check_class(class(x), "dfm_subset")
}
    
#' @export
dfm_subset.dfm <- function(x, subset, min_ntoken = NULL, max_ntoken = NULL,
                           drop_docid = TRUE, 
                           verbose = quanteda_options("verbose"),
                           ...) {
    
    x <- as.dfm(x)
    min_ntoken <- check_integer(min_ntoken, min = 0, allow_null = TRUE)
    max_ntoken <- check_integer(max_ntoken, min = 0, allow_null = TRUE)
    check_dots(...)
    
    #sys <- select_docvars(x@docvars, system = TRUE)
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
        before <- stats_dfm(x)
    x <- x[r & l,,drop_docid = drop_docid]
    if (verbose)
        message_dfm("dfm_subset()", before, stats_dfm(x))
    return(x)
}
