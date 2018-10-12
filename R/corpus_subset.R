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
#' @param select expression, indicating the \link{docvars} to keep
#' @param ... not used
#' @return corpus object, with a subset of documents (and docvars) selected according to arguments
#' @export
#' @seealso \code{\link{subset.data.frame}}
#' @keywords corpus
#' @examples
#' summary(corpus_subset(data_corpus_inaugural, Year > 1980))
#' summary(corpus_subset(data_corpus_inaugural, Year > 1930 & President == "Roosevelt", 
#'                       select = Year))
corpus_subset <- function(x, subset, select, ...) {
    UseMethod("corpus_subset")
}
    
#' @export
corpus_subset.default <- function(x, subset, select, ...) {
    stop(friendly_class_undefined_message(class(x), "corpus_subset"))
}

#' @export
corpus_subset.corpus <- function(x, subset, select, ...) {
    
    unused_dots(...)
    x <- as.corpus(x)
    attrs <- attributes(x)
    
    r <- if (missing(subset)) {
        rep_len(TRUE, nrow(attrs$docvars))
    } else {
        e <- substitute(subset)
        r <- eval(e, attrs$docvars, parent.frame())
        r & !is.na(r)
    }
    vars <- if (missing(select)) 
        TRUE
    else {
        nl <- as.list(seq_along(attrs$docvars))
        names(nl) <- names(attrs$docvars)
        c(1, eval(substitute(select), nl, parent.frame()))
    }
    
    x <- as.character(unclass(x))[r]
    attrs$docvars <- attrs$docvars[r, vars, drop = FALSE]
    attributes(x) <- attrs
    return(x)
}

