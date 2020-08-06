# convert generics ---------

#' Convert quanteda objects to non-quanteda formats
#'
#' Convert a quanteda [dfm] or [corpus] object to a format useable by other
#' packages. The general function `convert` provides easy conversion from a dfm
#' to the document-term representations used in all other text analysis packages
#' for which conversions are defined.  For [corpus] objects, `convert` provides
#' an easy way to make a corpus and its document variables into a data.frame.
#' @param x a [dfm] or [corpus] to be converted
#' @param to target conversion format, one of:
#'   \describe{ \item{`"lda"`}{a list with components "documents" and "vocab" as
#'   needed by the function
#'   [lda.collapsed.gibbs.sampler][lda::lda.collapsed.gibbs.sampler] from the
#'   \pkg{lda} package}
#'   \item{`"tm"`}{a \link[tm:matrix]{DocumentTermMatrix} from the \pkg{tm}
#'   package}
#'   \item{`"stm"`}{the  format for the \pkg{stm} package} \item{`"austin"`}{the
#'   `wfm` format from the **austin** package}
#'   \item{`"topicmodels"`}{the "dtm" format as used by the \pkg{topicmodels}
#'   package}
#'   \item{`"lsa"`}{the "textmatrix" format as
#'   used by the \pkg{lsa} package}
#'   \item{`"data.frame"`}{a data.frame of without row.names, in which documents
#'   are rows, and each feature is a variable (for a dfm),
#'    or each text and its document variables form a row (for a corpus)}
#'   \item{`"json"`}{(corpus only) convert a corpus and its document variables
#'   into JSON format, using the format described in
#'   \link[jsonlite:fromJSON]{jsonlite::toJSON()}}
#'   \item{`"tripletlist"`}{a named "triplet" format list consisting of
#'   `document`, `feature`, and `frequency`}
#'   }
#' @param docvars optional data.frame of document variables used as the
#'   `meta` information in conversion to the \pkg{stm} package format.
#'   This aids in selecting the document variables only corresponding to the
#'   documents with non-zero counts.  Only affects the "stm" format.
#' @param omit_empty logical; if `TRUE`, omit empty documents and features
#'   from the converted dfm. This is required for some formats (such as STM)
#'   that do not accept empty documents.  Only used when `to = "lda"` or
#'   `to = "topicmodels"`.  For `to = "stm"` format, `omit_empty`` is
#'   always `TRUE`.
#' @param docid_field character; the name of the column containing document
#'   names used when `to = "data.frame"`.  Unused for other conversions.
#' @param ... unused directly
#' @return A converted object determined by the value of `to` (see above).
#'   See conversion target package documentation for more detailed descriptions
#'   of the return formats.
#' @export
#' @examples
#' ## convert a dfm
#'
#' corp <- corpus_subset(data_corpus_inaugural, Year > 1970)
#' dfmat1 <- dfm(corp)
#'
#' # austin's wfm format
#' identical(dim(dfmat1), dim(convert(dfmat1, to = "austin")))
#'
#' # stm package format
#' stmmat <- convert(dfmat1, to = "stm")
#' str(stmmat)
#'
#' # triplet
#' tripletmat <- convert(dfmat1, to = "tripletlist")
#' str(tripletmat)
#'
#' \dontrun{
#' # tm's DocumentTermMatrix format
#' tmdfm <- convert(dfmat1, to = "tm")
#' str(tmdfm)
#'
#' # topicmodels package format
#' str(convert(dfmat1, to = "topicmodels"))
#'
#' # lda package format
#' str(convert(dfmat1, to = "lda"))
#' }
convert <- function(x, to, ...) {
    UseMethod("convert")
}

#' @noRd
#' @export
convert.default <- function(x, to, ...) {
    stop(friendly_class_undefined_message(class(x), "convert"))
}

#' @rdname convert
#' @export
convert.dfm <- function(x, to = c("lda", "tm", "stm", "austin", "topicmodels",
                                  "lsa", "matrix", "data.frame", "tripletlist"),
                        docvars = NULL, omit_empty = TRUE, docid_field = "doc_id", 
                        ...) {
    unused_dots(...)
    x <- as.dfm(x)
    to <- match.arg(to)
    attrs <- attributes(x)

    if (!is.null(docvars)) {
        if (!is.data.frame(docvars))
            stop("docvars must be a data.frame")
        if (nrow(docvars) != ndoc(x))
            stop("docvars must have the same number of rows as ndoc(x)")
    }

    if ((to %in% c("stm", "lda", "topicmodels")) &&
        (field_object(attrs, "weight_tf")$scheme != "count" || field_object(attrs, "weight_df")$scheme != "unary")) {
        stop("cannot convert a non-count dfm to a topic model format")
    }

    if (!to %in% c("lda", "topicmodels") && !missing(omit_empty) && omit_empty) {
        warning("omit_empty not used for 'to = \"", to, "\"'")
    }

    if (to == "tm")
        return(dfm2tm(x))
    else if (to == "lda")
        return(dfm2lda(x, omit_empty = omit_empty))
    else if (to == "stm")
        return(dfm2stm(x, docvars, omit_empty = TRUE))
    else if (to == "austin")
        return(dfm2austin(x))
    else if (to == "topicmodels")
        return(dfm2dtm(x, omit_empty = omit_empty))
    else if (to == "lsa")
        return(dfm2lsa(x))
    else if (to == "data.frame")
        return(dfm2dataframe(x, docid_field = docid_field))
    else if (to == "matrix")
        return(as.matrix(x))
    else if (to == "tripletlist")
        return(dfm2tripletlist(x))
    else
        stop("invalid \"to\" format")

}

#' @rdname convert
#' @inheritParams jsonlite::toJSON
#' @export
#' @examples
#'
#' ## convert a corpus into a data.frame
#'
#' corp <- corpus(c(d1 = "Text one.", d2 = "Text two."),
#'                docvars = data.frame(dvar1 = 1:2, dvar2 = c("one", "two"),
#'                                     stringsAsFactors = FALSE))
#' convert(corp, to = "data.frame")
#' convert(corp, to = "json")
convert.corpus <- function(x, to = c("data.frame", "json"), pretty = FALSE, ...) {
    unused_dots(...)
    to <- match.arg(to)

    if (to == "data.frame") {
        df <- data.frame(doc_id = docnames(x),
                         text = texts(x),
                         stringsAsFactors = FALSE,
                         row.names = NULL)
        df <- cbind(df, docvars(x))
        return(df)
    } else if (to == "json") {
        return(jsonlite::toJSON(convert(x, to = "data.frame"), pretty = pretty))
    } else {
        stop("invalid \"to\" format")
    }
}

# convert.dfm internal --------

#' Convenience wrappers for dfm convert
#'
#' To make the usage as consistent as possible with other packages, quanteda
#' also provides shortcut wrappers to [convert()], designed to be
#' similar in syntax to analogous commands in the packages to whose format they
#' are converting.
#' @param x the dfm to be converted
#' @inheritParams convert
#' @param ... additional arguments used only by `as.DocumentTermMatrix`
#' @return A converted object determined by the value of `to` (see above).
#'   See conversion target package documentation for more detailed descriptions
#'   of the return formats.
#' @note  Additional coercion methods to base R objects are also available:
#'   \describe{ \item{`[as.data.frame](x)`}{converts a [dfm] into
#'   a [data.frame]}
#'
#'   \item{`[as.matrix](x)`}{converts a [dfm] into a
#'   [matrix]} }
#' @name convert-wrappers
#' @keywords internal
#' @examples
#' corp <- corpus_subset(data_corpus_inaugural, Year > 1970)
#' dfmat <- dfm(corp)
#'
NULL

#' @rdname convert-wrappers
#' @details `as.wfm` converts a quanteda [dfm] into the
#' `wfm` format used by the `austin` package.
#' @export
#' @examples
#' # shortcut conversion to austin package's wfm format
#' identical(as.wfm(dfmat), convert(dfmat, to = "austin"))
#'
as.wfm <- function(x) {
    UseMethod("as.wfm")
}

#' @rdname convert-wrappers
#' @method as.wfm dfm
#' @export
as.wfm.dfm <- function(x) {
    convert(as.dfm(x), to = "austin")
}

#' @export
#' @rdname convert-wrappers
#' @details `as.DocumentTermMatrix` will convert a quanteda [dfm] into the
#'   \pkg{tm} package's \link[tm:matrix]{DocumentTermMatrix} format. Note: The
#'   \pkg{tm} package version of `as.TermDocumentMatrix()` allows a `weighting`
#'   argument, which supplies a weighting function for
#'   \code{\link[tm:matrix]{TermDocumentMatrix()}}.  Here the default is for
#'   term frequency weighting. If you want a different weighting, apply the
#'   weights after converting using one of the \pkg{tm} functions. For other
#'   available weighting functions from the \pkg{tm} package, see
#'   \link[tm:matrix]{TermDocumentMatrix}.
#' @examples
#' \dontrun{
#' # shortcut conversion to tm package's DocumentTermMatrix format
#' identical(as.DocumentTermMatrix(dfmat), convert(dfmat, to = "tm"))
#' }
#'
as.DocumentTermMatrix <- function(x) {
    UseMethod("as.DocumentTermMatrix")
}

#' @rdname convert-wrappers
#' @method as.DocumentTermMatrix dfm
#' @export
as.DocumentTermMatrix.dfm <- function(x) {
    convert(as.dfm(x), to = "tm")
}

#' @rdname convert-wrappers
dfm2austin <- function(x) {
    result <- as.matrix(as(x, "dgeMatrix"))
    names(dimnames(result))[2] <- "words"
    class(result) <- c("wfm", "matrix")
    result
}

#' @rdname convert-wrappers
#' @param weighting a \pkg{tm} weight, see [tm::weightTf()]
dfm2tm <- function(x, weighting = tm::weightTf) {
    attrs <- attributes(x)
    if (!requireNamespace("tm", quietly = TRUE))
        stop("You must install the tm package for this conversion.")
    if (!requireNamespace("slam", quietly = TRUE))
        stop("You must install the slam package for this conversion.")

    if (!(field_object(attrs, "weight_tf")$scheme == "count" && field_object(attrs, "weight_df")$scheme == "unary")) {
        warning("converted DocumentTermMatrix will not have weight attributes set correctly")
    }
    tm::as.DocumentTermMatrix(slam::as.simple_triplet_matrix(x),
                              weighting = weighting)
}

## TODO:
## Implement weight recordings for
## weightTfIdf
## - attr(*, "weighting")= chr [1:2] "term frequency - inverse document frequency" "tf-idf"
## - attr(*, "weighting")= chr [1:2] "term frequency - inverse document frequency (normalized)" "tf-idf"
## weightTf
## - attr(*, "weighting")= chr [1:2] "term frequency" "tf"
## weightSMART
## - attr(*, "weighting")= chr [1:2] "SMART ntc" "SMART"  (e.g.)

#' @rdname convert-wrappers
#' @details
#' `dfm2lda` provides converts a [dfm] into the list representation
#' of terms in documents used by the \pkg{lda} package (a list with components
#' "documents" and "vocab" as needed by
#'   [lda::lda.collapsed.gibbs.sampler()]).
#' @examples
#' \dontrun{
#' # shortcut conversion to lda package list format
#' identical(quanteda:::dfm2lda(dfmat), convert(dfmat, to = "lda"))
#' }
#'
#' @keywords internal
dfm2lda <- function(x, omit_empty = TRUE) {
    x <- as.dfm(x)
    if (!requireNamespace("tm", quietly = TRUE))
        stop("You must install the slam package for this conversion.")
    dtm2lda(dfm2dtm(x, omit_empty = omit_empty), omit_empty = omit_empty)
}

#' @rdname convert-wrappers
#' @details
#' `dfm2ldaformat` provides converts a [dfm] into the list
#' representation of terms in documents used by the \pkg{lda} package (a list
#' with components "documents" and "vocab" as needed by
#' [lda::lda.collapsed.gibbs.sampler()]).
#' @examples
#' \dontrun{
#' # shortcut conversion to lda package list format
#' identical(dfm2ldaformat(dfmat), convert(dfmat, to = "lda"))
#' }
#' @keywords internal
dtm2lda <- function(x, omit_empty = TRUE) {
    if (!requireNamespace("slam", quietly = TRUE))
        stop("You must install the slam package for this conversion.")

    docs <- vector(mode = "list", length = nrow(x))
    names(docs) <- rownames(x)

    docs[slam::row_sums(x) > 0] <- split.matrix(rbind(as.integer(x$j) - 1L,
                                                      as.integer(x$v)),
                                                as.integer(x$i))
    if (omit_empty) {
        docs[slam::row_sums(x) == 0] <- NULL
    } else {
        docs[slam::row_sums(x) == 0] <- rep(list(matrix(integer(), ncol = 0, nrow = 2)),
                                             sum(slam::row_sums(x) == 0))
    }
    list(documents = docs, vocab = colnames(x))
}

# internal function for dtm2lda
split.matrix <- function(x, f, drop = FALSE, ...) {
    lapply(split(seq_len(ncol(x)),
                 f, drop = drop, ...), function(ind) x[, ind, drop = FALSE])
}

#' @rdname convert-wrappers
dfm2dtm <- function(x, omit_empty = TRUE) {
    if (!requireNamespace("tm", quietly = TRUE))
        stop("You must install the tm package for this conversion.")
    if (!requireNamespace("slam", quietly = TRUE))
        stop("You must install the slam package for this conversion.")

    x <- as.dfm(x)
    x <- as(x, "dgTMatrix")
    if (omit_empty)
        x <- x[rowSums(x) > 0, ]
    tm::as.DocumentTermMatrix(slam::as.simple_triplet_matrix(x), tm::weightTf)
}

#' @rdname convert-wrappers
dfm2stm <- function(x, docvars = NULL, omit_empty = TRUE) {
    # get docvars (if any)
    if (is.null(docvars))
        docvars <- docvars(x)

    # sort features into alphabetical order
    x <- x[, order(featnames(x))]

    # deal with empty documents
    empty_docs <- rowSums(x) == 0
    if (omit_empty) {
        if (sum(empty_docs) > 0)
            warning("Dropped empty document(s): ",
                    paste0(docnames(x)[empty_docs], collapse = ", "))

        empty_feats <- colSums(x) == 0
        if (sum(empty_feats) > 0)
            warning("zero-count features: ",
                    paste0(featnames(x)[empty_feats], collapse = ", "))

        x <- x[!empty_docs, !empty_feats]
        docvars <- docvars[!empty_docs, , drop = FALSE]
    } else {
        stop("omit_empty = FALSE not implemented for STM format")
    }

    # convert counts to STM documents format
    x <- as(x, "dgTMatrix")
    docs <- ijv.to.doc(x@i + 1, x@j + 1, x@x)
    names(docs) <- rownames(x)
    list(documents = docs, vocab = colnames(x), meta = docvars)
}

# internal function for dfm2stm
ijv.to.doc <- function(i, j, v) {
    index <- split(j, i)
    index <- lapply(index, as.integer)
    count <- split(v, i)
    count <- lapply(count, as.integer)
    mapply(rbind, index, count, SIMPLIFY = FALSE)
}

#' Convert a dfm to an lsa "textmatrix"
#'
#' Converts a dfm to a textmatrix for use with the lsa package.
#' @param x dfm to be converted
#' @examples
#' \dontrun{
#' (dfmat <- dfm(c(d1 = "this is a first matrix",
#'                 d2 = "this is second matrix as example")))
#' lsa::lsa(convert(dfmat, to = "lsa"))
#' }
#' @keywords internal
dfm2lsa <- function(x) {
    if (!requireNamespace("lsa", quietly = TRUE))
        stop("You must install the lsa package for this conversion.")
    x <- t(as.matrix(x))
    names(dimnames(x))[1] <- "terms"
    lsa::as.textmatrix(x)
}

dfm2tripletlist <- function(x) {
    feat <- featnames(x)
    doc <- docnames(x)
    x <- as(x, "dgTMatrix")
    list(
        document = doc[x@i + 1],
        feature = feat[x@j + 1],
        frequency = x@x
    )
}

dfm2dataframe <- function(x, row.names = NULL, ..., document = docnames(x),
                          docid_field = "doc_id", check.names = FALSE) {
    if (!(is.character(document) || is.null(document)))
        stop("document must be character or NULL")
    df <- data.frame(as.matrix(x), row.names = row.names,
                     check.names = check.names)
    if (!is.null(document)) {
        if (docid_field %in% names(df)) {
            stop("'", docid_field, "' matches a feature in the dfm; use a different docid_field value")
        }
        df <- cbind(document, df, stringsAsFactors = FALSE)
        names(df)[1] <- docid_field
    }
    df
}
