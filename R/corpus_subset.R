#' Extract a subset of a corpus
#' 
#' Returns subsets of a corpus that meet certain conditions, including direct 
#' logical operations on docvars (document-level variables).  \code{corpus_subset}
#' functions identically to \code{\link{subset.data.frame}}, using non-standard
#' evaluation to evaluate conditions based on the \link{docvars} in the corpus.
#' 
#' @param x \link{corpus} object to be subsetted
#' @param subset logical expression indicating the documents to keep: missing
#'   values are taken as false
# @param select expression, indicating the \link{docvars} to keep
#' @param ... not used
#' @return corpus object, with a subset of documents (and docvars) selected according to arguments
#' @export
#' @seealso \code{\link{subset.data.frame}}
#' @keywords corpus
#' @examples
#' summary(corpus_subset(data_corpus_inaugural, Year > 1980))
#' summary(corpus_subset(data_corpus_inaugural, Year > 1930 & President == "Roosevelt"))
corpus_subset <- function(x, subset, ...) {
    UseMethod("corpus_subset")
}
    
#' @export
corpus_subset.default <- function(x, subset, ...) {
    stop(friendly_class_undefined_message(class(x), "corpus_subset"))
}

#' @export
corpus_subset.corpus <- function(x, subset, ...) {
    
    unused_dots(...)

    x <- as.corpus(x)
    attrs <- attributes(x)
    #sys <- select_docvars(attr(x, "docvars"), system = TRUE)
    usr <- select_docvars(attr(x, "docvars"), system = FALSE)
    r <- if (missing(subset)) {
        rep_len(TRUE, ndoc(x))
    } else {
        e <- substitute(subset)
        r <- eval(e, usr, parent.frame())
        r & !is.na(r)
    }
    # vars <- if (missing(select)) 
    #     rep_len(TRUE, ncol(usr))
    # else {
    #     nl <- as.list(seq_along(usr))
    #     names(nl) <- names(usr)
    #     eval(substitute(select), nl, parent.frame())
    # }
    x <- x[r]
    #attr(x, "docvars") <- cbind(reshape_docvars(sys, r),
    #                            reshape_docvars(usr, r, vars))
    #attributes(x, FALSE) <- attrs
    return(x)
}

