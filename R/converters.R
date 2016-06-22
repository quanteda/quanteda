#' convert a dfm to a non-quanteda format
#' 
#' Convert a quanteda \link{dfm-class} object to a format useable by other text
#' analysis packages.  The general function \code{convert} provides easy
#' conversion from a dfm to the document-term representations used in all other
#' text analysis packages for which conversions are defined.  To make the usage
#' as consistent as possible with other packages, however, quanteda also
#' provides direct conversion functions in the idiom of the foreign packages,
#' for example  \code{as.wfm} to coerce a dfm into the \code{wfm} format from
#' the \strong{austin} package, and \code{quantedaformat2dtm} for using a dfm
#' with the \pkg{topicmodels} package.
#' 
#' We recommend using \code{convert()} rather than the specific functions.  In
#' fact, it's worth considering whether we should simply remove all of them and
#' \strong{only} support calling these through `convert()`.
#' 
#' We may also use this function, eventually, for converting other classes of
#' objects such as a `corpus` or `tokenizedList`.
#' 
#' @param x dfm to be converted
#' @param to target conversion format, consisting of the name of the package
#'   into whose document-term matrix representation the dfm will be converted: 
#'   \describe{ \item{\code{"lda"}}{a list with components "documents" and
#'   "vocab" as needed by \link[lda]{lda.collapsed.gibbs.sampler} from the
#'   \pkg{lda} package} \item{\code{"tm"}}{a \link[tm]{DocumentTermMatrix} from
#'   the \pkg{tm} package} \item{\code{"stm"}}{the  format for the \pkg{stm}
#'   package} \item{\code{"austin"}}{the \code{wfm} format from the
#'   \strong{austin} package} \item{\code{"topicmodels"}}{the "dtm" format as
#'   used by the \pkg{topicmodels} package} }
#' @param docvars optional data.frame of document variables used as the
#'   \code{meta} information in conversion to the STM package format.  This aids
#'   in selecting the document variables only corresponding to the documents
#'   with non-zero counts.
#' @return A converted object determined by the value of \code{to} (see above). 
#'   See conversion target package documentation for more detailed descriptions 
#'   of the return formats.
#' @importFrom utils installed.packages
#' @export
#' @examples
#' mycorpus <- subset(inaugCorpus, Year > 1970)
#' quantdfm <- dfm(mycorpus, verbose = FALSE)
#' 
#' # austin's wfm format
#' austindfm <- as.wfm(quantdfm)
#' identical(austindfm, convert(quantdfm, to = "austin"))
#' 
#' # tm's DocumentTermMatrix format
#' tmdfm <- as.DocumentTermMatrix(quantdfm)
#' str(tmdfm)
#' 
#' # stm package format
#' stmdfm <- convert(quantdfm, to = "stm")
#' str(stmdfm)
#' # illustrate what happens with zero-length documents
#' quantdfm2 <- dfm(c(punctOnly = "!!!", mycorpus[-1]), verbose = FALSE)
#' rowSums(quantdfm2)
#' stmdfm2 <- convert(quantdfm2, to = "stm", docvars = docvars(mycorpus))
#' str(stmdfm2)
#'  
#' # topicmodels package format
#' topicmodelsdfm <- quantedaformat2dtm(quantdfm)
#' identical(topicmodelsdfm, convert(quantdfm, to = "topicmodels"))
#' 
#' # lda package format
#' ldadfm <- convert(quantdfm, to = "lda")
#' str(ldadfm)
#' identical(ldadfm[1], stmdfm[1])
#' 
convert <- function(x, to, ...) {
    UseMethod("convert")
}

#' @export
#' @rdname convert
# @importFrom topicmodels dtm2ldaformat
convert.dfm <- function(x, to = c("lda", "tm", "stm", "austin", "topicmodels"), docvars = NULL, ...) {
    to <- match.arg(to)
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    
    if (!is.null(docvars)) {
        if (!is.data.frame(docvars))
            stop("docvars must be a data.frame")
        if (nrow(docvars) != ndoc(x))
            stop("docvars must have the same number of rows as ndoc(x)")
    }
    
    if (to=="tm") 
        return(dfm2tmformat(x, ...))
    else if (to=="lda")
        return(dfm2ldaformat(x))
    else if (to=="stm")
        return(dfm2stmformat(x, docvars))
    else if (to=="austin")
        return(dfm2austinformat(x))
    else if (to=="topicmodels")
        return(quantedaformat2dtm(x))
}

#' @export
#' @rdname convert
as.wfm <- function(x) {
    UseMethod("as.wfm")
}


#' @export
#' @rdname convert
#' @details \code{as.wfm} converts a quanteda \link{dfm} into the
#' \code{wfm} format used by the \code{austin} package.
#' 
as.wfm.dfm <- function(x) {
    convert(x, to = "austin")
}

dfm2austinformat <- function(d) {
    d <- as.matrix(d)
    names(dimnames(d))[2] <- "words"
    class(d) <- c("wfm", "matrix")
    d
}

## convert to tm format
dfm2tmformat <- function(x, weighting=tm::weightTf) {
    if (!("tm" %in% installed.packages()[, "Package"])) 
        stop("You must install the tm package installed for this conversion.")
    if (!("slam" %in% installed.packages()[, "Package"]))
        stop("You must install the slam package installed for this conversion.")
    sl <- slam::as.simple_triplet_matrix(x)
    td <- tm::as.DocumentTermMatrix(sl, weighting=weighting)
    return(td)
}

#' @export
#' @rdname convert
as.DocumentTermMatrix <- function(x, ...) {
    UseMethod("as.DocumentTermMatrix")
}

#' @export
#' @rdname convert
#' @details \code{as.DocumentTermMatrix} will convert a quanteda \link{dfm} into
#'   the \pkg{tm} package's \link[tm]{DocumentTermMatrix} format.
#' @note The \pkg{tm} package version of \code{as.TermDocumentMatrix} allows a \code{weighting} argument,
#' which supplies a weighting function for 
#'   \link[tm]{TermDocumentMatrix}.  Here the default is for term frequency weighting.
#'   If you want a different weighting, apply the weights after converting using one of the \pkg{tm}
#'   functions.  
#   For other available weighting functions from the \pkg{tm} package, see 
#   \code{\link[tm]{TermDocumentMatrix}}.
#' @param ... not used here
as.DocumentTermMatrix.dfm <- function(x, ...) {
    convert(x, to="tm", ...)
}

#' @export
#' @rdname convert
dfm2ldaformat <- function(x) {
    UseMethod("dfm2ldaformat")
}

#' @rdname convert
#' @details
#' \code{dfm2ldaformat} provides converts a \link{dfm} into the list representation
#' of terms in documents used by tghe \pkg{lda} package.  
#' @return \code{dfm2ldaformat} returns a list with components "documents" and "vocab" as needed by 
#'   \code{\link[lda]{lda.collapsed.gibbs.sampler}}.
#' @export
#' @examples
#' # calling dfm2ldaformat directly
#' ldadfm <- dfm2ldaformat(quantdfm)
#' str(ldadfm)
dfm2ldaformat.dfm <- function(x) {
    if (!("tm" %in% installed.packages()[, "Package"]))
        stop("You must install the slam package installed for this conversion.")
    tmDTM <- dfm2tmformat(x)
    return(dtm2ldaformat(tmDTM))
}

## from the package topicmodels
dtm2ldaformat <- function (x, omit_empty = TRUE) 
{
    if (!("slam" %in% installed.packages()[, "Package"]))
        stop("You must install the slam package installed for this conversion.")
    
    split.matrix <- function(x, f, drop = FALSE, ...) lapply(split(seq_len(ncol(x)), 
                                                                   f, drop = drop, ...), function(ind) x[, ind, drop = FALSE])
    documents <- vector(mode = "list", length = nrow(x))
    names(documents) <- rownames(x)
    documents[slam::row_sums(x) > 0] <- split(rbind(as.integer(x$j) - 
                                                        1L, as.integer(x$v)), as.integer(x$i))
    if (omit_empty) 
        documents[slam::row_sums(x) == 0] <- NULL
    else documents[slam::row_sums(x) == 0] <- rep(list(matrix(integer(), 
                                                              ncol = 0, nrow = 2)), sum(slam::row_sums(x) == 0))
    list(documents = documents, vocab = colnames(x))
}

#' @export
#' @rdname convert
quantedaformat2dtm <- function(x) {
    UseMethod("quantedaformat2dtm")
}

#' @rdname convert
#' @export
#' @details
#' \code{quantedaformat2dtm} provides converts a \link{dfm} into the sparse simple triplet matrix
#'  representation
#' of terms in documents used by the \pkg{topicmodels} package.  
#' @return \code{quantedaformat2dtm} returns a "dtm" sparse matrix object for use with the 
#' \pkg{topicmodels} package.
quantedaformat2dtm.dfm <- function(x) {
    d_lda <- convert(x, to="lda")
    ldaformat2dtm(d_lda$documents, d_lda$vocab)
}

ldaformat2dtm <- function (documents, vocab, omit_empty = TRUE) 
{
    if (!("tm" %in% installed.packages()[, "Package"]))
        stop("You must install the tm package installed for this conversion.")
    if (!("slam" %in% installed.packages()[, "Package"]))
        stop("You must install the slam package installed for this conversion.")
    
    stm <- slam::simple_triplet_matrix(i = rep(seq_along(documents), 
                                               sapply(documents, ncol)), j = as.integer(unlist(lapply(documents, 
                                                                                                      "[", 1, )) + 1L), v = as.integer(unlist(lapply(documents, 
                                                                                                                                                     "[", 2, ))), nrow = length(documents), ncol = length(vocab), 
                                       dimnames = list(names(documents), vocab))
    dtm <- tm::as.DocumentTermMatrix(stm, tm::weightTf)
    if (omit_empty) 
        dtm <- dtm[slam::row_sums(dtm) > 0, ]
    dtm
}

# convert a dfm to stm's input document format
#
# Convert a quanteda dfm object into the indexed format needed for estimating
# a structural topic model from the \pkg{stm} package using \link[stm]{stm}.
# @param data dfm object to be converted
# @note
# Meta-data will need to be passed separately to \link[stm]{stm} as this 
# information is not included in a dfm object.
# @return A list containing the following elements:
# \item{documents}{A list containing the documents in the stm format.}
# \item{vocab}{Character vector of vocabulary.}
# \item{meta}{NULL} 
# @examples
# mydfm <- dfm(inaugTexts)
# mydfmStm <- dfm2stmformat(mydfm)
# str(mydfmStm)
# @export

dfm2stmformat <- function(data, meta) {
    # sort features into alphabetical order
    data <- data[, order(features(data))]
    data <- as(data, "dgTMatrix")
    
    # find out which documents are not empty
    non_empty_docs <- which(rowSums(data) != 0)
    
    # convert counts to STM documents format
    documents <- ijv.to.doc(data[non_empty_docs, ]@i+1, data[non_empty_docs, ]@j+1, data[non_empty_docs, ]@x) 
    names(documents) <- rownames(data)[non_empty_docs]
    
    # select docvars for non-empty docs
    if (!is.null(meta))
        meta <- meta[non_empty_docs, , drop = FALSE]
    
    # return the object
    list(documents = documents, vocab = colnames(data), meta = meta)
}


# borrowed from the STM package
ijv.to.doc <- function(i, j, v) {
    index <- split(j, i)
    index <- lapply(index, as.integer)
    count <- split(v, i)
    count <- lapply(count, as.integer)
    mapply(rbind, index, count)
}


