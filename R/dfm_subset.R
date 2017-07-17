#' extract a subset of a dfm
#' 
#' Returns \emph{document} subsets of a dfm that meet certain conditions, including direct 
#' logical operations on docvars (document-level variables).  \code{dfm_subset}
#' functions identically to \code{\link{subset.data.frame}}, using non-standard
#' evaluation to evaluate conditions based on the \link{docvars} in the dfm.
#' 
#' To select or subset \emph{features}, see \code{\link{dfm_select}} instead.
#' @param x \link{dfm} object to be subsetted
#' @param subset logical expression indicating the documents to keep: missing
#'   values are taken as false
#' @param select expression, indicating the docvars to select from the dfm
#' @param ... not used
#' @return \link{dfm} object, with a subset of documents (and docvars) selected according to arguments
#' @export
#' @seealso \code{\link{subset.data.frame}}
#' @keywords corpus
#' @examples
#' testcorp <- corpus(c(d1 = "a b c d", d2 = "a a b e",
#'                      d3 = "b b c e", d4 = "e e f a b"),
#'                    docvars = data.frame(grp = c(1, 1, 2, 3)))
#' testdfm <- dfm(testcorp)
#' dfm_subset(testdfm, grp > 1)
#' dfm_subset(testdfm, c(TRUE, FALSE, TRUE, FALSE))
dfm_subset <- function(x, subset, select, ...) {
    UseMethod("dfm_subset")
}
    
#' @rdname dfm_subset
#' @noRd    
#' @export
dfm_subset.dfm <- function(x, subset, select, ...) {

    if (length(addedArgs <- list(...)))
        warning("Argument", if (length(addedArgs) > 1L) "s " else " ", names(addedArgs), " not used.", sep = "")
    r <- if (missing(subset)) {
        rep_len(TRUE, nrow(docvars(x)))
    } else {
        e <- substitute(subset)
        r <- eval(e, docvars(x), parent.frame())
        r & !is.na(r)
    }
    vars <- if (missing(select)) {
        TRUE
    } else {
        nl <- as.list(seq_along(docvars(x)))
        names(nl) <- names(docvars(x))
        c(1, eval(substitute(select), nl, parent.frame()))
    }
    
    docvars(x) <- docvars(x)[, vars, drop = FALSE]
    x[r, ]
}

