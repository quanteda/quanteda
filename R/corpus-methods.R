#' Base method extensions for corpus objects
#'
#' Extensions of base R functions for corpus objects.
#' @name corpus-class
#' @param x a corpus object
#' @keywords internal corpus
#' @seealso [summary.corpus()]
NULL

#' @rdname print-methods
#' @param max_nchar max number of tokens to print; default is from the
#'   `print_corpus_max_nchar` setting of [quanteda_options()]
#' @method print corpus
#' @export
print.corpus <- function(x, max_ndoc = quanteda_options("print_corpus_max_ndoc"),
                         max_nchar = quanteda_options("print_corpus_max_nchar"),
                         show_summary = quanteda_options("print_corpus_summary"),
                         ...) {
    
    x <- as.corpus(x)
    max_ndoc <- check_integer(max_ndoc, min = -1)
    max_nchar <- check_integer(max_nchar, min = -1)
    show_summary <- check_logical(show_summary)
    check_dots(...)
    
    docvars <- docvars(x)
    ndoc <- ndoc(x)
    if (max_ndoc < 0)
        max_ndoc <- ndoc(x)

    if (show_summary) {
        cat("Corpus consisting of ", format(ndoc, big.mark = ","), " document",
            if (ndoc != 1L) "s" else "", sep = "")
        if (ncol(docvars))
            cat(" and ", format(ncol(docvars), big.mark = ","), " docvar",
                if (ncol(docvars) != 1L) "s" else "", sep = "")
        cat(".\n")
    }

    if (max_ndoc > 0 && ndoc(x) > 0) {
        x <- head(as.character(x), max_ndoc)
        label <- paste0(names(x), " :")
        x <- stri_replace_all_regex(x, "[\\p{C}]+", " ")
        len <- stri_length(x)
        if (max_nchar < 0)
            max_nchar <- max(len)
        for (i in seq_along(label)) {
            cat(label[i], "\n", sep = "")
            cat('"', stri_sub(x[i], 1, max_nchar), sep = "")
            if (len[i] > max_nchar) {
                cat('..."\n\n')
            } else {
                cat('"\n\n')
            }
        }
        ndoc_rem <- ndoc - max_ndoc
        if (ndoc_rem > 0)
            cat("[ reached max_ndoc ... ", format(ndoc_rem, big.mark = ","), " more document",
                if (ndoc_rem > 1) "s", " ]\n", sep = "")
    }

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
    attrs1 <- attributes(c1)
    attrs2 <- attributes(c2)

    if (length(intersect(docnames(c1), docnames(c2))))
        stop("Cannot combine corpora with duplicated document names",
             call. = FALSE)

    docvars <- rbind_fill(get_docvars(c1, user = TRUE, system = TRUE),
                          get_docvars(c2, user = TRUE, system = TRUE))
    build_corpus(
        c(as.character(c1), as.character(c2)),
        unit = field_object(attrs1, "unit"),
        docvars = docvars,
        meta = field_user(attrs1),
        class = attrs1[["class"]]
    )
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
    recursive <- check_logical(recursive)
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
#' @param i document names or indices for documents to extract.
#' @param drop_docid if `TRUE`, drop `docid` for documents removed as the result of extraction.
#' @return
#' Indexing a corpus works in three ways, as of v2.x.x:
#' * `[` returns a subsetted corpus
#' * `[[` returns the textual contents of a subsetted corpus (similar to [as.character()])
#' * `$` returns a vector containing the single named [docvars]
#' @examples
#'
#' # two ways to index corpus elements
#' data_corpus_inaugural["1793-Washington"]
#' data_corpus_inaugural[2]
#'
#' # return the text itself
#' data_corpus_inaugural[["1793-Washington"]]
`[.corpus` <- function(x, i, drop_docid = TRUE) {

    if (missing(i)) return(x)
    x <- as.corpus(x)
    attrs <- attributes(x)

    index <- seq_along(docnames(x))
    names(index) <- docnames(x)
    index <- index[i]
    if (any(is.na(index)))
        stop("Subscript out of bounds")

    build_corpus(
        unclass(x)[index],
        docvars = reshape_docvars(attrs[["docvars"]], index, drop_docid = drop_docid),
        meta = attrs[["meta"]],
        class = attrs[["class"]]
    )
}


# coercion and checking functions -----------

#' Coercion and checking methods for corpus objects
#'
#' Coercion functions to and from [corpus] objects, including conversion to a
#' plain [character] object; and checks for whether an object is a corpus.
#' @param x object to be coerced or checked
#' @param use.names logical; preserve (document) names if `TRUE`
#' @param ... additional arguments used by specific methods
#' @note `as.character(x)` where `x` is a corpus is equivalent to
#' calling the deprecated `texts(x)`.
#' @return `as.character()` returns the corpus as a plain character vector, with
#'   or without named elements.
#' @method as.character corpus
#' @export
as.character.corpus <- function(x, use.names = TRUE, ...) {
    use.names <- check_logical(use.names)
    dnames <- if (use.names) docnames(x) else NULL
    attributes(x) <- NULL
    return(structure(x, names = dnames))
}

#' @rdname as.character.corpus
#' @return `is.corpus` returns `TRUE` if the object is a corpus.
#' @export
is.corpus <- function(x) {
    "corpus" %in% class(x)
}

#' @rdname as.character.corpus
#' @return `as.corpus()` upgrades a corpus object to the newest format.
#' object.
#' @export
as.corpus <- function(x) {
    UseMethod("as.corpus")
}

#' @export
as.corpus.default <- function(x) {
    check_class(class(x), "as.corpus")
}

#' @export
#' @method as.corpus corpus
as.corpus.corpus <- function(x) {
    upgrade_corpus(x)
}
