#' Extract a subset of a tokens
#'
#' Returns document subsets of a tokens that meet certain conditions, including
#' direct logical operations on docvars (document-level variables).
#' \code{tokens_subset} functions identically to
#' \code{\link{subset.data.frame}}, using non-standard evaluation to evaluate
#' conditions based on the \link{docvars} in the tokens.
#'
#' @param x \link{tokens} object to be subsetted
#' @inheritParams corpus_subset
#' @param select expression, indicating the \link{docvars} to keep
#' @return \link{tokens} object, with a subset of documents (and docvars)
#'   selected according to arguments
#' @export
#' @seealso \code{\link{subset.data.frame}}
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
tokens_subset <- function(x, subset, select, ...) {
    UseMethod("tokens_subset")
}
    
#' @export
tokens_subset.default <- function(x, subset, select, ...) {
    stop(friendly_class_undefined_message(class(x), "tokens_subset"))
}
    
#' @export
tokens_subset.tokens <- function(x, subset, select, ...) {
    
    unused_dots(...)
    
    x <- as.tokens(x)
    sys <- get_docvars(attr(x, "docvars"), system = TRUE)
    usr <- get_docvars(attr(x, "docvars"), system = FALSE)
    r <- if (missing(subset)) {
        rep_len(TRUE, ndoc(x))
    } else {
        e <- substitute(subset)
        r <- eval(e, usr, parent.frame())
        r & !is.na(r)
    }
    vars <- if (missing(select)) 
        rep_len(TRUE, ncol(usr))
    else {
        nl <- as.list(seq_along(usr))
        names(nl) <- names(usr)
        eval(substitute(select), nl, parent.frame())
    }
    x <- unclass(x)[r]
    attr(x, "docvars") <- cbind(reshape_docvars(sys, r),
                                reshape_docvars(usr, r, vars))
    return(x)
}
