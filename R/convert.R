#' Convert a dfm to a non-quanteda format
#' 
#' Convert a quanteda \link{dfm} object to a format useable by other text
#' analysis packages.  The general function \code{convert} provides easy
#' conversion from a dfm to the document-term representations used in all other
#' text analysis packages for which conversions are defined.
#' @param x a \link{dfm} to be converted
#' @param to target conversion format, consisting of the name of the package 
#'   into whose document-term matrix representation the dfm will be converted: 
#'   \describe{ \item{\code{"lda"}}{a list with components "documents" and 
#'   "vocab" as needed by the function \link[lda]{lda.collapsed.gibbs.sampler} from the 
#'   \pkg{lda} package} \item{\code{"tm"}}{a \link[tm]{DocumentTermMatrix} from 
#'   the \pkg{tm} package} \item{\code{"stm"}}{the  format for the \pkg{stm} 
#'   package} \item{\code{"austin"}}{the \code{wfm} format from the 
#'   \strong{austin} package} \item{\code{"topicmodels"}}{the "dtm" format as 
#'   used by the \pkg{topicmodels} package} 
#'   \item{\code{"lsa"}}{the "textmatrix" format as 
#'   used by the \pkg{lsa} package}
#'   \item{\code{"data.frame"}}{a data.frame where each feature is a variable} 
#'   \item{\code{"tripletlist"}}{a named "triplet" format list consisting of 
#'   \code{document}, \code{feature}, and \code{frequency}} 
#'   }
#' @param docvars optional data.frame of document variables used as the
#'   \code{meta} information in conversion to the \pkg{stm} package format.
#'   This aids in selecting the document variables only corresponding to the
#'   documents with non-zero counts.
#' @return A converted object determined by the value of \code{to} (see above). 
#'   See conversion target package documentation for more detailed descriptions 
#'   of the return formats.
#' @export
#' @examples
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
#' #' # triplet
#' tripletmat <- convert(dfmat1, to = "tripletlist")
#' str(tripletmat)
#' 
#' # illustrate what happens with zero-length documents
#' dfmat2 <- dfm(c(punctOnly = "!!!", corp[-1]))
#' rowSums(dfmat2)
#' str(convert(dfmat2, to = "stm", docvars = docvars(corp)))
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
#' 
#' }
convert <- function(x, to = c("lda", "tm", "stm", "austin", "topicmodels", 
                              "lsa", "matrix", "data.frame", "tripletlist"), docvars = NULL) {
    UseMethod("convert")
}

#' @export
convert.default <- function(x, ...) {
    stop(friendly_class_undefined_message(class(x), "convert"))
}

#' @noRd
#' @export
convert.dfm <- function(x, to = c("lda", "tm", "stm", "austin", "topicmodels", 
                                  "lsa", "matrix", "data.frame", "tripletlist"), 
                        docvars = NULL) {
    x <- as.dfm(x)
    to <- match.arg(to)

    if (!is.null(docvars)) {
        if (!is.data.frame(docvars)) 
            stop("docvars must be a data.frame")
        if (nrow(docvars) != ndoc(x))
            stop("docvars must have the same number of rows as ndoc(x)")
    }
    
    if ((to %in% c("stm", "lda", "topicmodels")) &&
        (x@weightTf$scheme != "count" || x@weightDf$scheme != "unary")) {
        stop("cannot convert a non-count dfm to a topic model format")
    }
    
    if (to == "tm") 
        return(dfm2tm(x))
    else if (to == "lda")
        return(dfm2lda(x))
    else if (to == "stm")
        return(dfm2stm(x, docvars))
    else if (to == "austin")
        return(dfm2austin(x))
    else if (to == "topicmodels")
        return(dfm2dtm(x))
    else if (to == "lsa")
        return(dfm2lsa(x))
    else if (to == "data.frame")
        return(dfm2dataframe(x))
    else if (to == "matrix")
        return(as.matrix(x))
    else if (to == "tripletlist")
        return(dfm2tripletlist(x))
    else
        stop("invalid \"to\" format")
        
}


#' Convenience wrappers for dfm convert
#' 
#' To make the usage as consistent as possible with other packages, quanteda
#' also provides shortcut wrappers to \code{\link{convert}}, designed to be
#' similar in syntax to analogous commands in the packages to whose format they
#' are converting.
#' @param x the dfm to be converted
#' @param ... additional arguments used only by \code{as.DocumentTermMatrix}
#' @return A converted object determined by the value of \code{to} (see above). 
#'   See conversion target package documentation for more detailed descriptions 
#'   of the return formats.
#' @note  Additional coercion methods to base R objects are also available: 
#'   \describe{ \item{\code{\link{as.data.frame}(x)}}{converts a \link{dfm} into
#'   a \link{data.frame}}
#'   
#'   \item{\code{\link{as.matrix}(x)}}{converts a \link{dfm} into a
#'   \link{matrix}} }
#' @name convert-wrappers
#' @keywords internal
#' @examples 
#' corp <- corpus_subset(data_corpus_inaugural, Year > 1970)
#' dfmat <- dfm(corp)
#' 
NULL


#' @rdname convert-wrappers
#' @details \code{as.wfm} converts a quanteda \link{dfm} into the
#' \code{wfm} format used by the \code{austin} package.
#' @export
#' @examples 
#' # shortcut conversion to austin package's wfm format
#' identical(as.wfm(dfmat), convert(dfmat, to = "austin"))
#' 
as.wfm <- function(x) {
    UseMethod("as.wfm")
}

#' @noRd
#' @method as.wfm dfm
#' @export
as.wfm.dfm <- function(x) {
    convert(as.dfm(x), to = "austin")
}

#' @export
#' @rdname convert-wrappers
#' @details \code{as.DocumentTermMatrix} will convert a quanteda \link{dfm} into
#'   the \pkg{tm} package's \link[tm]{DocumentTermMatrix} format. Note: The
#'   \pkg{tm} package version of \code{as.TermDocumentMatrix} allows a
#'   \code{weighting} argument, which supplies a weighting function for 
#'   \link[tm]{TermDocumentMatrix}.  Here the default is for term frequency
#'   weighting. If you want a different weighting, apply the weights after
#'   converting using one of the \pkg{tm} functions. For other available
#'   weighting functions from the \pkg{tm} package, see 
#'   \code{\link[tm]{TermDocumentMatrix}}.
#' @examples
#' \dontrun{
#' # shortcut conversion to tm package's DocumentTermMatrix format
#' identical(as.DocumentTermMatrix(dfmat), convert(dfmat, to = "tm"))
#' }
#' 
as.DocumentTermMatrix <- function(x) {
    UseMethod("as.DocumentTermMatrix")
}

#' @noRd
#' @method as.DocumentTermMatrix dfm
#' @export
as.DocumentTermMatrix.dfm <- function(x) {
    convert(as.dfm(x), to = "tm")
}

#' @keywords internal
dfm2austin <- function(x) {
    result <- as.matrix(as(x, 'dgeMatrix'))
    names(dimnames(result))[2] <- "words"
    class(result) <- c("wfm", "matrix")
    result
}

#' @keywords internal
dfm2tm <- function(x, weighting = tm::weightTf) {
    if (!requireNamespace("tm", quietly = TRUE)) 
        stop("You must install the tm package installed for this conversion.")
    if (!requireNamespace("slam", quietly = TRUE))
        stop("You must install the slam package installed for this conversion.")
    
    if (!(x@weightTf$scheme == "count" && x@weightDf$scheme == "unary")) {
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
#' \code{dfm2lda} provides converts a \link{dfm} into the list representation
#' of terms in documents used by the \pkg{lda} package (a list with components 
#' "documents" and "vocab" as needed by 
#'   \code{\link[lda]{lda.collapsed.gibbs.sampler}}).
#' @examples
#' \dontrun{
#' # shortcut conversion to lda package list format
#' identical(quanteda:::dfm2lda(dfmat), convert(dfmat, to = "lda")) 
#' }
#' 
#' @keywords internal
dfm2lda <- function(x) {
    x <- as.dfm(x)
    if (!requireNamespace("tm", quietly = TRUE))
        stop("You must install the slam package installed for this conversion.")
    dtm2lda(dfm2dtm(x))
}

#' @noRd
#' @details
#' \code{dfm2ldaformat} provides converts a \link{dfm} into the list
#' representation of terms in documents used by the \pkg{lda} package (a list
#' with components "documents" and "vocab" as needed by
#' \code{\link[lda]{lda.collapsed.gibbs.sampler}}).
#' @examples
#' \dontrun{
#' # shortcut conversion to lda package list format
#' identical(dfm2ldaformat(dfmat), convert(dfmat, to = "lda")) 
#' }
#' @keywords internal
dtm2lda <- function (x, omit_empty = TRUE) {
    if (!requireNamespace("slam", quietly = TRUE))
        stop("You must install the slam package installed for this conversion.")
    
    docs <- vector(mode = "list", length = nrow(x))
    names(docs) <- rownames(x)

    docs[slam::row_sums(x) > 0] <- split.matrix(rbind(as.integer(x$j) - 1L, 
                                                      as.integer(x$v)), 
                                                as.integer(x$i))
    if (omit_empty) 
        docs[slam::row_sums(x) == 0] <- NULL
    else docs[slam::row_sums(x) == 0] <- rep(list(matrix(integer(), 
                                                         ncol = 0, nrow = 2)), 
                                             sum(slam::row_sums(x) == 0))
    list(documents = docs, vocab = colnames(x))
}

# internal function for dtm2lda
split.matrix <- function(x, f, drop = FALSE, ...) {
    lapply(split(seq_len(ncol(x)), 
                 f, drop = drop, ...), function(ind) x[, ind, drop = FALSE])
}

#' @keywords internal
dfm2dtm <- function(x, omit_empty = TRUE) {

    if (!requireNamespace("tm", quietly = TRUE))
        stop("You must install the tm package installed for this conversion.")
    if (!requireNamespace("slam", quietly = TRUE))
        stop("You must install the slam package installed for this conversion.")
    
    x <- as.dfm(x)
    x <- as(x, 'dgTMatrix')
    if (omit_empty) 
        x <- x[rowSums(x) > 0, ]
    tm::as.DocumentTermMatrix(slam::as.simple_triplet_matrix(x), tm::weightTf)
}


#' @keywords internal
dfm2stm <- function(x, docvars, omit_empty = TRUE) {
    # get docvars (if any)
    if (is.null(docvars))
        docvars <- docvars(x)
    
    # sort features into alphabetical order
    x <- x[, order(featnames(x))]
    if (omit_empty) {
        
        empty_docs <- rowSums(x) == 0
        if (sum(empty_docs) > 0) 
            warning("Dropped empty document(s): ", 
                    paste0(docnames(x)[empty_docs], collapse = ", "))
        
        empty_feats <- colSums(x) == 0
        if (sum(empty_feats) > 0) 
            warning("zero-count features: ", 
                    paste0(featnames(x)[empty_feats], collapse = ", "))
        
        x <- x[!empty_docs, !empty_feats]
        docvars <- docvars[!empty_docs,, drop = FALSE]
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
    x <- as.matrix(x)
    result <- apply(x, c(1,2), as.integer) # convert to integer
    names(dimnames(result)) <- c("docs", "terms") 
    class(result) <- "textmatrix"
    t(result)
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
                          check.names = FALSE) {
    if (!(is.character(document) || is.null(document)))
        stop("document must be character or NULL")
    df <- data.frame(as.matrix(x), row.names = row.names, 
                     check.names = check.names)
    if (!is.null(document)) df <- cbind(document, df, stringsAsFactors = FALSE)
    df    
}
