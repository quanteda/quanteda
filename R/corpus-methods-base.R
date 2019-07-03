#' Base method extensions for corpus objects
#' 
#' Extensions of base R functions for corpus objects.
#' @name corpus-class
#' @param x a corpus object
#' @keywords internal corpus
#' @seealso \code{\link{summary.corpus}}
NULL

#' @export
#' @rdname corpus-class
#' @method print corpus
print.corpus <- function(x, ...) {
    x <- as.corpus(x)
    cat("Corpus consisting of ", format(ndoc(x), big.mark=","), " document",
        if (ndoc(x) > 1L) "s" else "", sep = "")
    if (ncol(docvars(x)))
        cat(" and ", format(ncol(docvars(x)), big.mark=","), " docvar",
            if (ncol(docvars(x)) == 1L) "" else "s", sep="")
    cat(".\n")
    
    # development mode
    # cat("\n\n")
    # print(stri_sub(x, 0, 100))
    # cat("\n")
    # cat("docvars:\n")
    # print(attr(x, "docvar"))
    # cat("\n")
    # cat("meta:\n")
    # print(attr(x, "meta"))
    
}

#' @return \code{is.corpus} returns \code{TRUE} if the object is a corpus
#' @rdname corpus-class
#' @export
is.corpus <- function(x) {
    "corpus" %in% class(x)
}

#' Summarize a corpus
#' 
#' Displays information about a corpus, including attributes and metadata such
#' as date of number of texts, creation and source.
#' 
#' @param object corpus to be summarized
#' @param n maximum number of texts to describe, default=100
#' @param showmeta set to \code{TRUE} to include document-level
#'   meta-data
#' @param tolower convert texts to lower case before counting types
#' @param ... additional arguments passed through to \code{\link{tokens}}
#' @export
#' @method summary corpus
#' @keywords internal corpus
#' @examples
#' summary(data_corpus_inaugural)
#' summary(data_corpus_inaugural, n = 10)
#' corp <- corpus(data_char_ukimmig2010, 
#'                    docvars = data.frame(party=names(data_char_ukimmig2010))) 
#' summary(corp, showmeta = TRUE) # show the meta-data
#' sumcorp <- summary(corp) # (quietly) assign the results
#' sumcorp$Types / sumcorp$Tokens # crude type-token ratio
summary.corpus <- function(object, n = 100, tolower = FALSE, ...) {
    object <- as.corpus(object)
    result <- summarize_texts(texts(object), n = n, tolower = tolower, ...)
    attr(result, "ndoc_show") <- n
    attr(result, "ndoc_all") <- ndoc(object)
    class(result) <- c("summary.corpus", "data.frame")
    return(result)
}

#' @export
#' @rdname corpus-class
#' @method print summary.corpus
print.summary.corpus <- function(x, ...) {
    
    ndoc_all <- attr(x, "ndoc_all")
    ndoc_show <- attr(x, "ndoc_show")

    cat("Corpus consisting of ", ndoc_all, " document", if (ndoc_all > 1) "s" else "", sep = "")
    if (!is.null(ndoc_show)) 
        cat(", showing ", ndoc_show, " document", if (ndoc_show > 1) "s" else "", sep = "")
    cat(":\n\n")
    print.data.frame(x, row.names = FALSE)
    cat("\n")
}

#' @noRd
#' @export
#' @method [ summary.corpus
`[.summary.corpus` <- function(x, i, j, ...) {
    class(x) <- "data.frame"
    row.names(x) <- NULL
    NextMethod("[")
}

#' Return the first or last part of a corpus
#' 
#' For a \link{corpus} object, returns the first or last \code{n} documents.
#' @param x a dfm object
#' @param n a single integer.  If positive, the number of documents for the
#'   resulting object: number of first/last documents for the dfm.  If negative,
#'   all but the n last/first number of documents of x.
#' @param ... additional arguments passed to other functions
#' @return A \link{corpus} class object corresponding to the subset defined 
#'   by \code{n}.
#' @export
#' @name head.corpus
#' @method head corpus
#' @keywords corpus
#' @examples
#' head(data_corpus_irishbudget2010, 3) %>% summary()
#' 
head.corpus <- function(x, n = 6L, ...) {
    x <- as.corpus(x)
    stopifnot(length(n) == 1L)
    n <- if (n < 0L) max(ndoc(x) + n, 0L) else min(n, ndoc(x))
    corpus_subset(x, seq_len(ndoc(x)) %in% seq_len(n))
}

#' @rdname head.corpus
#' @method tail corpus
#' @export
#' @examples
#' tail(data_corpus_irishbudget2010, 3) %>% summary()
tail.corpus <- function(x, n = 6L, ...) {
    x <- as.corpus(x)
    stopifnot(length(n) == 1L)
    nrx <- ndoc(x)
    n <- if (n < 0L) max(nrx + n, 0L) else min(n, nrx)
    sel <- as.integer(seq.int(to = nrx, length.out = n))
    corpus_subset(x, seq_len(ndoc(x)) %in% sel)
}
    

#' @rdname corpus-class
#' @param c1 corpus one to be added
#' @param c2 corpus two to be added
#' @details The \code{+} operator for a corpus object will combine two corpus
#'   objects, resolving any non-matching \code{\link{docvars}} by making them
#'   into \code{NA} values for the corpus lacking that field. Corpus-level meta
#'   data is concatenated, except for \code{source} and \code{notes}, which are
#'   stamped with information pertaining to the creation of the new joined
#'   corpus.
#'
#'   The `c()` operator is also defined for corpus class objects, and provides
#'   an easy way to combine multiple corpus objects.
#'
#'   There are some issues that need to be addressed in future revisions of
#'   quanteda concerning the use of factors to store document variables and
#'   meta-data.  Currently most or all of these are not recorded as factors,
#'   because we use \code{stringsAsFactors=FALSE} in the
#'   \code{\link{data.frame}} calls that are used to create and store the
#'   document-level information, because the texts should always be stored as
#'   character vectors and never as factors.
#' @export
`+.corpus` <- function(c1, c2) {
    c1 <- as.corpus(c1)
    c2 <- as.corpus(c2)
    result <- corpus(c(as.character(unclass(c1)), as.character(unclass(c2))),
                     docvars = rbind_fill(get_docvars(c1), get_docvars(c2)),
                     meta = meta(c1, type = "user"))
    meta_system(result) <- meta_system_defaults("corpus+")
    return(result)
}

#' @rdname corpus-class
#' @param recursive logical used by `c()` method, always set to `FALSE`
#' @examples 
#' 
#' # concatenate corpus objects
#' corpus1 <- corpus(data_char_ukimmig2010[1:2])
#' corpus2 <- corpus(data_char_ukimmig2010[3:4])
#' corpus3 <- corpus(data_char_ukimmig2010[5:6])
#' summary(c(corpus1, corpus2, corpus3))
#' @export
c.corpus <- function(..., recursive = FALSE) {
    x <- list(...)
    if (length(x) == 1) return(x[[1]])
    result <- x[[1]] + x[[2]]
    if (length(x) == 2) return(result)
    for (i in seq(3, length(x)))
        result <- result + x[[i]]
    return(result)
}

#' @rdname corpus-class
#' @method [ corpus
#' @export
#' @param i index for documents or rows of document variables
#' @param j index for column of document variables
#' @param drop if \code{TRUE}, return a vector if extracting a single document
#'   variable; if \code{FALSE}, return it as a single-column data.frame.  See
#'   \code{\link{drop}} for further details.
#' @examples 
#' 
#' # two ways to index corpus elements
#' data_corpus_inaugural["1793-Washington"]
#' data_corpus_inaugural[2] 
#' 
`[.corpus` <- function(x, i) {
    x <- as.corpus(x)
    attrs <- attributes(x)
    if (is.character(i)) {
        index <- match(i, docnames(x))
    } else if (is.numeric(i)) {
        index <- match(i, seq_len(length(x)))
    } else {
        index <- which(i)
    }
    is_na <- is.na(index)
    if (any(is_na))
        stop("Subscript out of bounds")
    index <- index[!is_na]
    
    x <- unclass(x)[index]
    attrs$docvars <- subset_docvars(attrs$docvars, index)
    attributes(x, FALSE) <- attrs
    return(x)
}
