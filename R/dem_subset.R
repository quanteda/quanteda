#' Extract a subset of a dem
#' 
#' Returns document subsets of a dem that meet certain conditions,
#' including direct logical operations on docvars (document-level variables). 
#' `dem_subset` functions identically to [subset.data.frame()],
#' using non-standard evaluation to evaluate conditions based on the
#' [docvars] in the dem.
#' 
#' To select or subset *features*, see [dem_select()] instead.
#' @param x [dem] object to be subsetted.
#' @inheritParams corpus_subset
#' @inheritParams tokens_subset
#' @inheritParams messages
#' @return [dem] object, with a subset of documents (and docvars) selected
#'   according to arguments
#' @export
#' @keywords dem
#' @examples
#' corp <- corpus(c(d1 = "a b c d", d2 = "a a b e",
#'                  d3 = "b b c e", d4 = "e e f a b"),
#'                docvars = data.frame(grp = c(1, 1, 2, 3)))
#' demat <- dem(tokens(corp))
#' # selecting on a docvars condition
#' dem_subset(demat, grp > 1)
#' # selecting on a supplied vector
#' dem_subset(demat, c(TRUE, FALSE, TRUE, FALSE))
dem_subset <- function(x, subset, #min_ntoken = NULL, max_ntoken = NULL, 
                       drop_docid = TRUE, verbose = quanteda_options("verbose"), 
                       ...) {
    UseMethod("dem_subset")
}
    
#' @export
dem_subset.default <- function(x, subset, #min_ntoken = NULL, max_ntoken = NULL, 
                               drop_docid = TRUE, 
                               verbose = quanteda_options("verbose"),
                               ...) {
    check_class(class(x), "dem_subset")
}
    
#' @export
dem_subset.dem <- function(x, subset, #min_ntoken = NULL, max_ntoken = NULL,
                           drop_docid = TRUE, 
                           verbose = quanteda_options("verbose"),
                           ...) {
    
    x <- as.dem(x)
    #min_ntoken <- check_integer(min_ntoken, min = 0, allow_null = TRUE)
    #max_ntoken <- check_integer(max_ntoken, min = 0, allow_null = TRUE)
    check_dots(...)
    
    docvar <- get_docvars(x, user = TRUE, system = TRUE)
    r <- if (missing(subset)) {
        rep_len(TRUE, ndoc(x))
    } else {
        e <- substitute(subset)
        r <- eval(e, docvar, parent.frame())
        r & !is.na(r)
    }
    
    # l <- if (is.null(min_ntoken) && is.null(max_ntoken)) {
    #     rep_len(TRUE, ndoc(x))
    # } else {
    #     n <- ntoken(x)
    #     if (is.null(min_ntoken)) min_ntoken <- 0L
    #     if (is.null(max_ntoken)) max_ntoken <- max(n)
    #     min_ntoken <= n & n <= max_ntoken
    # }
    #x <- x[r & l,,drop_docid = drop_docid]
    x <- x[r,drop_docid = drop_docid]
    if (verbose)
       message_dem("dem_subset()", stats_dem(x))
    return(x)
}
