#' Base method extensions for corpus objects
#'
#' Extensions of base R functions for corpus objects.
#' @name corpus-class
#' @param x a corpus object
#' @keywords internal corpus
#' @seealso [summary.corpus()]
NULL

#' @export
#' @rdname corpus-class
#' @param ndoc integer; number of documents to show text from; 0 shows none, and -1 shows all
#' @param nchar integer; number of characters to use when showing the first part of the
#'   of text; 0 shows none, and -1 shows all
#' @param show.summary print a brief summary indicating the number of documents
#'   and docvars
#' @method print corpus
print.corpus <- function(x, ndoc = quanteda_options("print_corpus_max_ndoc"), 
                         nchar = quanteda_options("print_corpus_max_nchar"), 
                         show.summary = quanteda_options("print_corpus_summary"), 
                         ...) {
    x <- as.corpus(x)

    if (ndoc < 0) ndoc <- ndoc(x)
    if (nchar < 0) nchar <- -1L

    ndoc <- as.integer(min(ndoc, ndoc(x)))
    nchar <- as.integer(nchar)

    if (show.summary) {
        cat("Corpus consisting of ", format(ndoc(x), big.mark = ","), " document",
            if (ndoc(x) > 1L) "s" else "", sep = "")
        if (ncol(docvars(x)))
            cat(" and ", format(ncol(docvars(x)), big.mark = ","), " docvar",
                if (ncol(docvars(x)) == 1L) "" else "s", sep = "")
        cat(".\n")
    }

    if (ndoc > 0) {
        temp <- head(texts(x), ndoc)
        label <- paste0("[", names(temp), "]")
        temp <- stri_replace_all_regex(temp, "[\\p{C}]+", " ")
        temp <- paste0(stri_sub(temp, 1, nchar), if (nchar > stri_length(nchar)) " ..." else "")
        cat(paste0(format(label, justify = "right"), " ", temp, "\n"), sep = "")
    }
    
    ndoc_rem <- ndoc(x) - ndoc
    if (show.summary && ndoc_rem > 0 && ndoc != 0)
        cat("and ", ndoc_rem, " more document", 
            if (ndoc_rem > 1) "s", ".\n", sep = "")
}
    
#' @return `is.corpus` returns `TRUE` if the object is a corpus
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
#' @param showmeta set to `TRUE` to include document-level
#'   meta-data
#' @param tolower convert texts to lower case before counting types
#' @param ... additional arguments passed through to [tokens()]
#' @export
#' @method summary corpus
#' @keywords internal corpus
#' @examples
#' summary(data_corpus_inaugural)
#' summary(data_corpus_inaugural, n = 10)
#' corp <- corpus(data_char_ukimmig2010,
#'                docvars = data.frame(party=names(data_char_ukimmig2010)))
#' summary(corp, showmeta = TRUE) # show the meta-data
#' sumcorp <- summary(corp) # (quietly) assign the results
#' sumcorp$Types / sumcorp$Tokens # crude type-token ratio
summary.corpus <- function(object, n = 100, tolower = FALSE, showmeta = TRUE, ...) {
    object <- as.corpus(object)
    ndoc_all <- ndoc(object)
    object <- head(object, n)
    ndoc_show <- ndoc(object)
    result <- if (!is.null(meta(object, "summary", type = "system"))) {
        get_summary_metadata(object, ...)
    } else {
        summarize_texts(texts(object), tolower = tolower, ...)
    }
    if (showmeta)
        result <- cbind(result, docvars(object))
    attr(result, "ndoc_all") <- ndoc_all
    attr(result, "ndoc_show") <- ndoc_show
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
#' For a [corpus] object, returns the first or last `n` documents.
#' @param x a dfm object
#' @param n a single integer.  If positive, the number of documents for the
#'   resulting object: number of first/last documents for the dfm.  If negative,
#'   all but the n last/first number of documents of x.
#' @param ... additional arguments passed to other functions
#' @return A [corpus] class object corresponding to the subset defined
#'   by `n`.
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
#' @details The `+` operator for a corpus object will combine two corpus
#'   objects, resolving any non-matching [docvars()] by making them
#'   into `NA` values for the corpus lacking that field. Corpus-level meta
#'   data is concatenated, except for `source` and `notes`, which are
#'   stamped with information pertaining to the creation of the new joined
#'   corpus.
#'
#'   The `c()` operator is also defined for corpus class objects, and provides
#'   an easy way to combine multiple corpus objects.
#'
#'   There are some issues that need to be addressed in future revisions of
#'   quanteda concerning the use of factors to store document variables and
#'   meta-data.  Currently most or all of these are not recorded as factors,
#'   because we use `stringsAsFactors=FALSE` in the
#'   [data.frame()] calls that are used to create and store the
#'   document-level information, because the texts should always be stored as
#'   character vectors and never as factors.
#' @return
#' The `+` and `c()` operators return a [corpus()] object.
#' @export
`+.corpus` <- function(c1, c2) {
    c1 <- as.corpus(c1)
    c2 <- as.corpus(c2)
    
    if (length(intersect(docnames(c1), docnames(c2))))
        stop("Cannot combine corpora with duplicated document names", 
             call. = FALSE)
    #if (!identical(attr(c1, "unit"), attr(c2, "unit")))
    #    stop("Cannot combine corpora in different units")
    
    docvar <- rbind_fill(get_docvars(c1, user = TRUE, system = TRUE), 
                         get_docvars(c2, user = TRUE, system = TRUE))
    result <- compile_corpus(
        c(as.character(c1), as.character(c2)), 
        names = c(docnames(c1), docnames(c2)),
        unit = attr(c1, "unit"),
        source = "corpus+",
        docvars = docvar,
        meta = meta(c1, type = "user")
    )
    return(result)
}

#' @rdname corpus-class
#' @param recursive logical used by `c()` method, always set to `FALSE`
#' @examples
#' # concatenate corpus objects
#' corp1 <- corpus(data_char_ukimmig2010[1:2])
#' corp2 <- corpus(data_char_ukimmig2010[3:4])
#' corp3 <- corpus(data_char_ukimmig2010[5:6])
#' summary(c(corp1, corp2, corp3))
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
#' @param drop if `TRUE`, return a vector if extracting a single document
#'   variable; if `FALSE`, return it as a single-column data.frame.  See
#'   [drop()] for further details.
#' @return
#' Indexing a corpus works in three ways, as of v2.x.x:
#' * `[` returns a subsetted corpus
#' * `[[` returns the textual contents of a subsetted corpus (similar to [texts()])
#' * `$` returns a vector containing the single named [docvars]
#' @examples
#'
#' # two ways to index corpus elements
#' data_corpus_inaugural["1793-Washington"]
#' data_corpus_inaugural[2]
#'
#' # return the text itself
#' data_corpus_inaugural[["1793-Washington"]]
`[.corpus` <- function(x, i) {
    x <- as.corpus(x)
    attrs <- attributes(x)
    
    index <- seq_along(docnames(x))
    names(index) <- docnames(x)
    index <- index[i]
    if (any(is.na(index)))
        stop("Subscript out of bounds")
    x <- unclass(x)[index]
    attrs$docvars <- subset_docvars(attrs$docvars, index)
    attrs$names <- attrs$docvars[["docname_"]]
    attributes(x) <- attrs
    return(x)
}
