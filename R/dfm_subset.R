#' Extract a subset of a dfm
#' 
#' Returns document subsets of a dfm that meet certain conditions,
#' including direct logical operations on docvars (document-level variables). 
#' `dfm_subset` functions identically to [subset.data.frame()],
#' using non-standard evaluation to evaluate conditions based on the
#' [docvars] in the dfm.
#' 
#' To select or subset *features*, see [dfm_select()] instead.
#' @param x [dfm] object to be subsetted
#' @inheritParams corpus_subset
# @param select expression, indicating the docvars to select from the dfm; or a
#   \link{dfm} object, in which case the returned dfm will contain the same
#   documents as the original dfm, even if these are empty.  See Details.
#' @return [dfm] object, with a subset of documents (and docvars) selected
#'   according to arguments
#' @details When `select` is a dfm, then the returned dfm will be equal in
#'   document dimension and order to the dfm used for selection.  This is the
#'   document-level version of using [dfm_select()] where
#'   `pattern` is a dfm: that function matches features, while
#'   `dfm_subset` will match documents.
#' @export
#' @seealso [subset.data.frame()]
#' @keywords dfm
#' @examples
#' corp <- corpus(c(d1 = "a b c d", d2 = "a a b e",
#'                  d3 = "b b c e", d4 = "e e f a b"),
#'                docvars = data.frame(grp = c(1, 1, 2, 3)))
#' dfmat <- dfm(corp)
#' # selecting on a docvars condition
#' dfm_subset(dfmat, grp > 1)
#' # selecting on a supplied vector
#' dfm_subset(dfmat, c(TRUE, FALSE, TRUE, FALSE))
dfm_subset <- function(x, subset, ...) {
    UseMethod("dfm_subset")
}
    
#' @export
dfm_subset.default <- function(x, subset, ...) {
    stop(friendly_class_undefined_message(class(x), "dfm_subset"))
}
    
#' @export
dfm_subset.dfm <- function(x, subset, ...) {
    
    unused_dots(...)
    
    x <- as.dfm(x)
    #sys <- select_docvars(x@docvars, system = TRUE)
    docvar <- get_docvars(x, user = TRUE, system = TRUE)
    r <- if (missing(subset)) {
        rep_len(TRUE, ndoc(x))
    } else {
        e <- substitute(subset)
        r <- eval(e, docvar, parent.frame())
        r & !is.na(r)
    }
    # vars <- if (missing(select)) 
    #     rep_len(TRUE, ncol(usr))
    # else {
    #     nl <- as.list(seq_along(usr))
    #     names(nl) <- names(usr)
    #     eval(substitute(select), nl, parent.frame())
    # }
    return(x[r,])
}
