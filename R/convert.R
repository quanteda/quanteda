#' convert a dfm to a non-quanteda format
#' 
#' Convert a quanteda \link{dfm} object to a format useable by other text 
#' analysis packages.  The general function \code{convert} provides easy 
#' conversion from a dfm to the document-term representations used in all other 
#' text analysis packages for which conversions are defined.    See also 
#' \link{convert-wrappers} for convenience functions for specific package converters.
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
#'   used by the \pkg{topicmodels} package} 
#'   \item{\code{"lsa"}}{the "textmatrix" format as 
#'   used by the \pkg{lsa} package} }
#' @param docvars optional data.frame of document variables used as the 
#'   \code{meta} information in conversion to the STM package format.  This aids
#'   in selecting the document variables only corresponding to the documents 
#'   with non-zero counts.
#' @param ... unused
#' @return A converted object determined by the value of \code{to} (see above). 
#'   See conversion target package documentation for more detailed descriptions 
#'   of the return formats.
#' @note There also exist a variety of converter shortcut commands, designed to 
#' mimic the idioms of the packages into whose format they convert.  
#' See \link{convert-wrappers} for details.
#' @export
#' @examples
#' mycorpus <- corpus_subset(data_corpus_inaugural, Year > 1970)
#' quantdfm <- dfm(mycorpus, verbose = FALSE)
#' 
#' # austin's wfm format
#' identical(dim(quantdfm), dim(convert(quantdfm, to = "austin")))
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
#' \dontrun{
#' #' # tm's DocumentTermMatrix format
#' tmdfm <- convert(quantdfm, to = "tm")
#' str(tmdfm)
#' 
#' # topicmodels package format
#' str(convert(quantdfm, to = "topicmodels"))
#' 
#' # lda package format
#' ldadfm <- convert(quantdfm, to = "lda")
#' str(ldadfm)
#' }
convert <- function(x, to = c("lda", "tm", "stm", "austin", "topicmodels", "lsa",
                         "matrix", "data.frame"), docvars = NULL, ...) {
    UseMethod("convert")
}

#' @noRd
#' @export
convert.dfm <- function(x, to = c("lda", "tm", "stm", "austin", "topicmodels", "lsa",
                                  "matrix", "data.frame"), docvars = NULL, ...) {
    to <- match.arg(to)
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    
    if (!is.null(docvars)) {
        if (!is.data.frame(docvars))
            stop("docvars must be a data.frame")
        if (nrow(docvars) != ndoc(x))
            stop("docvars must have the same number of rows as ndoc(x)")
    }
    
    if (to == "tm") 
        return(dfm2tmformat(x, ...))
    else if (to == "lda")
        return(dfm2ldaformat(x))
    else if (to == "stm")
        return(dfm2stmformat(x, docvars))
    else if (to == "austin")
        return(dfm2austinformat(x))
    else if (to == "topicmodels")
        return(quantedaformat2dtm(x))
    else if (to == "lsa")
        return(dfm2lsa(x))
    else if (to == "data.frame")
        return(as.data.frame(x))
    else if (to == "matrix")
        return(as.matrix(x))
    else
        stop("invalid \"to\" format")
        
}


#' convenience wrappers for dfm convert
#' 
#' To make the usage as consistent as possible with other packages, quanteda
#' also provides shortcut wrappers to \code{\link{convert}}, designed to be
#' similar in syntax to analagous commands in the packages to whose format they
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
#' mycorpus <- corpus_subset(data_corpus_inaugural, Year > 1970)
#' quantdfm <- dfm(mycorpus, verbose = FALSE)
#' 
NULL


#' @rdname convert-wrappers
#' @details \code{as.wfm} converts a quanteda \link{dfm} into the
#' \code{wfm} format used by the \code{austin} package.
#' @export
#' @examples 
#' # shortcut conversion to austin package's wfm format
#' identical(as.wfm(quantdfm), convert(quantdfm, to = "austin"))
#' 
as.wfm <- function(x) {
    if (!is.dfm(x))
        stop("x must be a dfm class object")
    convert(x, to = "austin")
}

dfm2austinformat <- function(d) {
    d <- as.matrix(d)
    names(dimnames(d))[2] <- "words"
    class(d) <- c("wfm", "matrix")
    d
}

## convert to tm format
dfm2tmformat <- function(x, weighting = tm::weightTf) {
    if (!requireNamespace("tm")) 
        stop("You must install the tm package installed for this conversion.")
    if (!requireNamespace("slam"))
        stop("You must install the slam package installed for this conversion.")
    sl <- slam::as.simple_triplet_matrix(x)
    td <- tm::as.DocumentTermMatrix(sl, weighting = weighting)
    return(td)
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
#' identical(as.DocumentTermMatrix(quantdfm), convert(quantdfm, to = "tm"))
#' }
#' 
as.DocumentTermMatrix <- function(x, ...) {
    if (!is.dfm(x))
        stop("x must be a dfm class object")
    convert(x, to = "tm", ...)
}

#' @rdname convert-wrappers
#' @details
#' \code{dfm2ldaformat} provides converts a \link{dfm} into the list representation
#' of terms in documents used by tghe \pkg{lda} package (a list with components 
#' "documents" and "vocab" as needed by 
#'   \code{\link[lda]{lda.collapsed.gibbs.sampler}}).
#' @export
#' @examples
#' \dontrun{
#' # shortcut conversion to lda package list format
#' identical(dfm2ldaformat(quantdfm), convert(quantdfm, to = "lda")) 
#' }
#' 
dfm2ldaformat <- function(x) {
    if (!is.dfm(x))
        stop("x must be a dfm class object")
    if (!requireNamespace("tm"))
        stop("You must install the slam package installed for this conversion.")
    tmDTM <- dfm2tmformat(x)
    return(dtm2ldaformat(tmDTM))
}


## from the package topicmodels
dtm2ldaformat <- function (x, omit_empty = TRUE) {
    if (!requireNamespace("slam"))
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

#' @rdname convert-wrappers
#' @export
#' @details \code{quantedaformat2dtm} provides converts a \link{dfm} into the
#' sparse simple triplet matrix representation of terms in documents used by the
#' \pkg{topicmodels} package.
#' @examples 
#' # shortcut conversion to topicmodels package format
#' \dontrun{
#' identical(quantedaformat2dtm(quantdfm), 
#'           convert(quantdfm, to = "topicmodels")) 
#' }
#' 
quantedaformat2dtm <- function(x) {
    if (!is.dfm(x))
        stop("x must be a dfm class object")
    d_lda <- convert(x, to = "lda")
    ldaformat2dtm(d_lda$documents, d_lda$vocab)
}

ldaformat2dtm <- function (documents, vocab, omit_empty = TRUE) {
    if (!requireNamespace("tm"))
        stop("You must install the tm package installed for this conversion.")
    if (!requireNamespace("slam"))
        stop("You must install the slam package installed for this conversion.")
    
    stm <- slam::simple_triplet_matrix(i = rep(seq_along(documents), vapply(documents, ncol, integer(1))), 
                                       j = as.integer(unlist(lapply(documents, "[", 1, )) + 1L),
                                       v = as.integer(unlist(lapply(documents, "[", 2, ))), nrow = length(documents), ncol = length(vocab),
                                       dimnames = list(names(documents), vocab))
    dtm <- tm::as.DocumentTermMatrix(stm, tm::weightTf)
    if (omit_empty) 
        dtm <- dtm[slam::row_sums(dtm) > 0, ]
    dtm
}

dfm2stmformat <- function(data, meta) {
    # sort features into alphabetical order
    data <- data[, order(featnames(data))]
    data <- as(data, "dgTMatrix")
    
    # find out which documents are not empty
    non_empty_docs <- which(rowSums(data) != 0)
    
    # find out which documents are empty
    empty_docs <- which(rowSums(data) == 0)
    if (length(empty_docs) > 0) warning("Dropped empty document(s): ", paste0(names(empty_docs), collapse=", "))
    
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

#' convert a dfm to an lsa "textmatrix"
#' 
#' Converts a dfm to a textmatrix for use with the lsa package.
#' @param x dfm to be converted
#' @examples
#' \dontrun{
#' (mydfm <- dfm(c(d1 = "this is a first matrix", d2 = "this is second matrix as example")))
#' lsa::lsa(convert(mydfm, to = "lsa"))
#' }
#' @keywords internal
dfm2lsa <- function(x) {
    x <- as.matrix(x)
    # convert to integer
    x <- apply(x, c(1,2), as.integer)
    class(x) <- "textmatrix"
    names(dimnames(x)) <- c("docs", "terms") 
    t(x)
}
    

