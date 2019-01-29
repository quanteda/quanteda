#' Extract a subset of a dfm
#' 
#' Returns document subsets of a dfm that meet certain conditions,
#' including direct logical operations on docvars (document-level variables). 
#' \code{dfm_subset} functions identically to \code{\link{subset.data.frame}},
#' using non-standard evaluation to evaluate conditions based on the
#' \link{docvars} in the dfm.
#' 
#' To select or subset \emph{features}, see \code{\link{dfm_select}} instead.
#' @param x \link{dfm} object to be subsetted
#' @inheritParams corpus_subset
#' @param select expression, indicating the docvars to select from the dfm; or a
#'   \link{dfm} object, in which case the returned dfm will contain the same
#'   documents as the original dfm, even if these are empty.  See Details.
#' @return \link{dfm} object, with a subset of documents (and docvars) selected
#'   according to arguments
#' @details When \code{select} is a dfm, then the returned dfm will be equal in
#'   document dimension and order to the dfm used for selection.  This is the
#'   document-level version of using \code{\link{dfm_select}} where
#'   \code{pattern} is a dfm: that function matches features, while
#'   \code{dfm_subset} will match documents.
#' @export
#' @seealso \code{\link{subset.data.frame}}
#' @keywords dfm
#' @examples
#' corp <- corpus(c(d1 = "a b c d", d2 = "a a b e",
#'                      d3 = "b b c e", d4 = "e e f a b"),
#'                    docvars = data.frame(grp = c(1, 1, 2, 3)))
#' dfmat <- dfm(corp)
#' # selecting on a docvars condition
#' dfm_subset(dfmat, grp > 1)
#' # selecting on a supplied vector
#' dfm_subset(dfmat, c(TRUE, FALSE, TRUE, FALSE))
#' 
#' # selecting on a dfm
#' dfmat1 <- dfm(c(d1 = "a b b c", d2 = "b b c d"))
#' dfmat2 <- dfm(c(d1 = "x y z", d2 = "a b c c d", d3 = "x x x"))
#' dfm_subset(dfmat1, subset = dfmat2)
#' dfm_subset(dfmat1, subset = dfmat2[c(3,1,2), ])
dfm_subset <- function(x, subset, select, ...) {
    UseMethod("dfm_subset")
}
    
#' @export
dfm_subset.default <- function(x, subset, select, ...) {
    stop(friendly_class_undefined_message(class(x), "dfm_subset"))
}
    
#' @export
dfm_subset.dfm <- function(x, subset, select, ...) {
    
    x <- as.dfm(x)
    unused_dots(...)

    r <- if (missing(subset)) {
        rep(TRUE, ndoc(x))
    } else {
        e <- substitute(subset)
        r <- eval(e, docvars(x), parent.frame())
        if (is.dfm(r)) {
            if (!missing(select)) stop("cannot select docvars if subset is a dfm")
            x <- x[docnames(x) %in% docnames(r), ]
            return(dfm_group(dfm_trim(x, min_termfreq = 1, min_docfreq = 1, 
                                      verbose = FALSE), 
                             groups = factor(docnames(x), levels = docnames(r)),
                             fill = TRUE))
        } else {
            r & !is.na(r)
        }
    }
    
    s <- if (missing(select)) {
        rep(TRUE, ncol(docvars(x)))
    } else {
        nl <- as.list(seq_along(docvars(x)))
        names(nl) <- names(docvars(x))
        eval(substitute(select), nl, parent.frame())
    }
    if (ncol(docvars(x)))
        docvars(x) <- docvars(x)[, s, drop = FALSE]
    
    x[r, ]
}
