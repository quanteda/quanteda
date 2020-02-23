# featnames -----------

#' Get the feature labels from a dfm
#'
#' Get the features from a document-feature matrix, which are stored as the
#' column names of the [dfm] object.
#' @param x the dfm whose features will be extracted
#' @return character vector of the feature labels
#' @examples
#' dfmat <- dfm(data_corpus_inaugural)
#'
#' # first 50 features (in original text order)
#' head(featnames(dfmat), 50)
#'
#' # first 50 features alphabetically
#' head(sort(featnames(dfmat)), 50)
#'
#' # contrast with descending total frequency order from topfeatures()
#' names(topfeatures(dfmat, 50))
#' @export
featnames <- function(x) {
    UseMethod("featnames")
}

#' @export
#' @noRd
featnames.dfm <- function(x) {
    x <- as.dfm(x)
    if (is.null(colnames(x))) {
        character()
    } else {
        colnames(x)
    }
}

# docnames -----------

#' @noRd
#' @export
docnames.dfm <- function(x) {
    rownames(x)
}

# as.dfm -----------

#' Coercion and checking functions for dfm objects
#'
#' Convert an eligible input object into a dfm, or check whether an object is a
#' dfm.  Current eligible inputs for coercion to a dfm are: [matrix],
#' (sparse) [Matrix][Matrix::Matrix],
#' \link[tm:matrix]{TermDocumentMatrix} and \link[tm:matrix]{DocumentTermMatrix}
#' (from the \pkg{tm} package), [data.frame], and other [dfm] objects.
#' @param x a candidate object for checking or coercion to [dfm]
#' @return `as.dfm` converts an input object into a [dfm].  Row names
#'   are used for docnames, and column names for featnames, of the resulting
#'   dfm.
#' @seealso [as.data.frame.dfm()], [as.matrix.dfm()],
#'   [convert()]
#' @export
as.dfm <- function(x) {
    UseMethod("as.dfm")
}

#' @export
as.dfm.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "as.dfm"))
}

#' @noRd
#' @method as.dfm dfm
#' @export
as.dfm.dfm <- function(x) {
    upgrade_dfm(x)
}

#' @noRd
#' @method as.dfm matrix
#' @export
as.dfm.matrix <- function(x) {
    matrix2dfm(x)
}

#' @noRd
#' @method as.dfm Matrix
#' @export
as.dfm.Matrix <- function(x) {
    matrix2dfm(x)
}

#' @noRd
#' @method as.dfm data.frame
#' @export
as.dfm.data.frame <- function(x) {
    matrix2dfm(as.matrix(x, rownames.force = TRUE))
}

#' @noRd
#' @method as.dfm dfmSparse
#' @export
as.dfm.dfmSparse <- function(x) {
    as.dfm(as(x, "dgCMatrix"))
}

#' @noRd
#' @method as.dfm DocumentTermMatrix
#' @export
as.dfm.DocumentTermMatrix <- function(x) {
    as.dfm(
        sparseMatrix(i = x$i, j = x$j, x = x$v,
                     dims = dim(x),
                     dimnames = list(docs = rownames(x),
                                     features = colnames(x))))
}

#' @noRd
#' @method as.dfm TermDocumentMatrix
#' @export
as.dfm.TermDocumentMatrix <- function(x) {
    as.dfm(
        sparseMatrix(i = x$j, j = x$i, x = x$v,
                     dims = rev(dim(x)),
                     dimnames = list(docs = colnames(x), 
                                     features = rownames(x))))
}

#' Converts a Matrix to a dfm
#' @param x a Matrix
#' @param meta a list of values to be assigned to slots
#' @keywords internal
matrix2dfm <- function(x, docvars = NULL, meta = NULL) {

    docname <- rownames(x)
    if (nrow(x) > length(docname))
        docname <- paste0(quanteda_options("base_docname"), seq_len(nrow(x)))

    featname <- colnames(x)
    if (ncol(x) > length(featname))
        featname <- paste0(quanteda_options("base_featname"), seq_len(ncol(x)))

    if (is.null(docvars))
        docvars <- make_docvars(nrow(x), docname, FALSE)
    if (is.null(meta))
        meta <- make_meta("dfm")

    build_dfm(
        as(Matrix(x, sparse = TRUE), "dgCMatrix"),
        featname,
        docvars = docvars,
        meta = meta
    )
}

#' Set values to a dfm's S4 slots
#' @param x a dfm
#' @param exceptions names of slots to be ignored
#' @param value a list of values extracted using `attributes` and to be assigned to slots
#' @keywords internal
"set_dfm_slots<-" <- function(x, exceptions = NULL, value) {
    if (is.null(value)) return(x)
    sname <- setdiff(slotNames("dfm"), c(slotNames("dgCMatrix"), exceptions))
    for (s in sname) {
        try({
            slot(x, s) <- value[[s]]
        }, silent = TRUE)
    }
    return(x)
}

#' @rdname set_dfm_slots-set
get_dfm_slots <- function(x) {
    sname <- setdiff(slotNames("dfm"), c(slotNames("dgCMatrix")))
    attributes(x)[sname]
}

#' @rdname as.dfm
#' @return
#' `is.dfm` returns `TRUE` if and only if its argument is a [dfm].
#' @export
is.dfm <- function(x) {
    is(x, "dfm")
}

# topfeatures -----------

#' Identify the most frequent features in a dfm
#'
#' List the most (or least) frequently occurring features in a [dfm], either
#' as a whole or separated by document.
#' @name topfeatures
#' @param x the object whose features will be returned
#' @param n how many top features should be returned
#' @param decreasing If `TRUE`, return the `n` most frequent features;
#'   otherwise return the `n` least frequent features
#' @param scheme one of `count` for total feature frequency (within
#'   `group` if applicable), or `docfreq` for the document frequencies
#'   of features
#' @inheritParams groups
#' @return A named numeric vector of feature counts, where the names are the
#'   feature labels, or a list of these if `groups` is given.
#' @examples
#' dfmat1 <- corpus_subset(data_corpus_inaugural, Year > 1980) %>%
#'     dfm(remove_punct = TRUE)
#' dfmat2 <- dfm_remove(dfmat1, stopwords("english"))
#'
#' # most frequent features
#' topfeatures(dfmat1)
#' topfeatures(dfmat2)
#'
#' # least frequent features
#' topfeatures(dfmat2, decreasing = FALSE)
#'
#' # top features of individual documents
#' topfeatures(dfmat2, n = 5, groups = docnames(dfmat2))
#'
#' # grouping by president last name
#' topfeatures(dfmat2, n = 5, groups = "President")
#'
#' # features by document frequencies
#' tail(topfeatures(dfmat1, scheme = "docfreq", n = 200))
#' @export
topfeatures <- function(x, n = 10, decreasing = TRUE,
                        scheme = c("count", "docfreq"), groups = NULL) {
    UseMethod("topfeatures")
}

#' @export
topfeatures.default <- function(x, n = 10, decreasing = TRUE,
                                scheme = c("count", "docfreq"), groups = NULL) {
    stop(friendly_class_undefined_message(class(x), "topfeatures"))
}

#' @export
#' @noRd
#' @importFrom stats quantile
topfeatures.dfm <- function(x, n = 10, decreasing = TRUE,
                            scheme = c("count", "docfreq"), groups = NULL) {

    x <- as.dfm(x)
    if (!nfeat(x) || !ndoc(x)) return(numeric())
    if (!is.numeric(n)) stop("n must be a number")
    scheme <- match.arg(scheme)

    if (!is.null(groups)) {
        result <- list()
        x <- dfm_group(x, groups)
        for (i in seq_len(ndoc(x))) {
            result[[i]] <- topfeatures(x[i, ], n = n, scheme = scheme,
                                       decreasing = decreasing, groups = NULL)
        }
        names(result) <- docnames(x)
        return(result)
    }

    if (n > nfeat(x)) n <- nfeat(x)

    if (scheme == "count") {
        wght <- colSums(x)
    } else if (scheme == "docfreq") {
        wght <- docfreq(x)
    }
    result <- sort(wght, decreasing)
    return(head(result, n))
}

# sparsity -----------

#' Compute the sparsity of a document-feature matrix
#'
#' Return the proportion of sparseness of a document-feature matrix, equal
#' to the proportion of cells that have zero counts.
#' @param x the document-feature matrix
#' @examples
#' dfmat <- dfm(data_corpus_inaugural)
#' sparsity(dfmat)
#' sparsity(dfm_trim(dfmat, min_termfreq = 5))
#' @export
sparsity <- function(x) {
    UseMethod("sparsity")
}

#' @export
sparsity.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "sparsity"))
}

#' @export
sparsity.dfm <- function(x) {
    (1 - length(x@x) / prod(dim(x)))
}

#  Internal --------

#' Internal functions for dfm objects
#'
#' Internal function documentation for [dfm] objects.
#' @name dfm-internal
#' @keywords dfm internal
NULL

#' The `Compare` methods enable relational operators to be use with dfm.
#' Relational operations on a dfm with a numeric will return a
#' [dgCMatrix-class][Matrix::dgCMatrix-class] object.
#' @rdname dfm-internal
#' @param e1 a [dfm]
#' @param e2 a numeric value to compare with values in a dfm
#' @export
#' @seealso [Comparison] operators
setMethod("Compare", c("dfm", "numeric"), function(e1, e2) {
    as(callGeneric(as(e1, "dgCMatrix"), e2), "lgCMatrix")
})
