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
#' @param select expression, indicating the docvars to select from the tokens;
#'   or a \link{tokens} object, in which case the returned tokens will contain the same
#'   documents in the same order as the original tokens, even if these are empty.
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
#'
#' # selecting on a tokens
#' toks1 <- tokens(c(d1 = "a b b c", d2 = "b b c d"))
#' toks2 <- tokens(c(d1 = "x y z", d2 = "a b c c d", d3 = "x x x"))
#' tokens_subset(toks1, subset = toks2)
#' tokens_subset(toks1, subset = toks2[c(3,1,2)])
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
    
    r <- if (missing(subset)) {
        rep_len(TRUE, nrow(docvars(x)))
    } else {
        e <- substitute(subset)
        r <- eval(e, docvars(x), parent.frame())
        if (is.tokens(r)) {
            if (!missing(select)) stop("cannot select docvars if subset is a tokens")
            x <- x[which(docnames(x) %in% docnames(r)), ]
            return(tokens_group(x, groups = factor(docnames(x), levels = docnames(r))))
        } else {
            r & !is.na(r)
        }
    }
    
    vars <- if (missing(select)) {
        TRUE
    } else {
        nl <- as.list(seq_along(docvars(x)))
        names(nl) <- names(docvars(x))
        eval(substitute(select), nl, parent.frame())
    }
    if (ncol(docvars(x)))
        docvars(x) <- docvars(x)[, vars, drop = FALSE]
    
    x[which(r)]
}
