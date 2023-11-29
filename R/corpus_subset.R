#' Extract a subset of a corpus
#' 
#' Returns subsets of a corpus that meet certain conditions, including direct 
#' logical operations on docvars (document-level variables).  `corpus_subset`
#' functions identically to [subset.data.frame()], using non-standard
#' evaluation to evaluate conditions based on the [docvars] in the corpus.
#' 
#' @param x [corpus] object to be subsetted.
#' @param subset logical expression indicating the documents to keep: missing
#'   values are taken as false.
#' @param drop_docid if `TRUE`, `docid` for documents are removed as the result 
#'   of subsetting.
#' @param ... not used
#' @return corpus object, with a subset of documents (and docvars) selected according to arguments
#' @export
#' @seealso [subset.data.frame()]
#' @keywords corpus
#' @examples
#' summary(corpus_subset(data_corpus_inaugural, Year > 1980))
#' summary(corpus_subset(data_corpus_inaugural, Year > 1930 & President == "Roosevelt"))
corpus_subset <- function(x, subset, drop_docid = TRUE, ...) {
    UseMethod("corpus_subset")
}
    
#' @export
corpus_subset.default <- function(x, subset, drop_docid = TRUE, ...) {
    check_class(class(x), "corpus_subset")
}

#' @export
corpus_subset.corpus <- function(x, subset, drop_docid = TRUE, ...) {
    
    x <- as.corpus(x)
    check_dots(...)

    attrs <- attributes(x)
    docvar <- get_docvars(x, user = TRUE, system = TRUE)
    r <- if (missing(subset)) {
        rep_len(TRUE, ndoc(x))
    } else {
        e <- substitute(subset)
        r <- eval(e, docvar, parent.frame())
        r & !is.na(r)
    }
    return(x[r, drop_docid = drop_docid])
}
