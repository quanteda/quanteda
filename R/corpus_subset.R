#' extract a subset of a corpus
#' 
#' Returns subsets of a corpus that meet certain conditions, including direct 
#' logical operations on docvars (document-level variables).  `corpus_subset` 
#' functions identically to \link{`subset`} where non-standard evaluation is
#' used to evaluate conditions based on the \link{docvars} in the corpus.
#' 
#' @param x corpus object to be subsetted
#' @param subset logical expression indicating elements or rows to keep: missing
#'   values are taken as false
#' @param select expression, indicating the attributes to select from the corpus
#' @param ... not used
#' @return corpus object
#' @export
#' @seealso \code{\link{subset}}
#' @examples
#' summary(corpus_subset(data_corpus_inaugural, Year>1980))
#' summary(corpus_subset(data_corpus_inaugural, Year>1930 & President=="Roosevelt", select=Year))
corpus_subset <- function(x, subset, select, ...) {
    
    if (as.character(match.call()[[1]]) == "subset.corpus")
        .Deprecated("corpus_subset")
    
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    r <- if (missing(subset)) {
        rep_len(TRUE, nrow(documents(x)))
    } else {
        e <- substitute(subset)
        r <- eval(e, documents(x), parent.frame())
        r & !is.na(r)
    }
    vars <- if (missing(select)) 
        TRUE
    else {
        nl <- as.list(seq_along(documents(x)))
        names(nl) <- names(documents(x))
        c(1, eval(substitute(select), nl, parent.frame()))
    }
    documents(x) <- documents(x)[r, vars, drop = FALSE]
    x
}

#' @rdname corpus_subset
#' @noRd
#' @export
subset.corpus <- corpus_subset
