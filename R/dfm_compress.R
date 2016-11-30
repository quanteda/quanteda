#' compress a dfm or fcm by combining identical dimension elements
#' 
#' "Compresses" a dfm (or fcm) whose dimension names are the same, for either
#' documents or features.  This may happen, for instance, if features are made
#' equivalent through application of a thesaurus.  It may also occur after
#' lower-casing or stemming the features of a dfm, but this should only be done
#' in very rare cases (approaching never: it's better to do this \emph{before}
#' constructing the dfm.)  It could also be needed after a
#' \code{\link{cbind.dfm}} or \code{\link{rbind.dfm}} operation.
#' 
#' @param x input object, a \link{dfm} or \link{fcm}
#' @param margin character indicating on which margin to compress a dfm, either 
#'   \code{"documents"}, \code{"features"}, or \code{"both"} (default).  For fcm
#'   objects, \code{"documents"} has an effect.
#' @param ... additional arguments passed from generic to specific methods
#' @export
#' @examples 
#' mat <- rbind(dfm(c("b A A", "C C a b B"), toLower = FALSE, verbose = FALSE),
#'              dfm("A C C C C C", toLower = FALSE, verbose = FALSE))
#' colnames(mat) <- toLower(features(mat))
#' mat
#' dfm_compress(mat, margin = "documents")
#' dfm_compress(mat, margin = "features")
#' dfm_compress(mat)
#' 
#' # no effect if no compression needed
#' compactdfm <- dfm(data_char_inaugural[1:5], verbose = FALSE)
#' dim(compactdfm)
#' dim(dfm_compress(compactdfm))
#' 
dfm_compress <- function(x, margin = c("both", "documents", "features")) {
    if (!is.dfm(x))
        stop("compress_dfm only works on a dfm object")

    margin <- match.arg(margin)
    
    uniquednames <- unique(rownames(x))
    uniquefnames <- unique(colnames(x))
    if (length(uniquednames) == nrow(x) & length(uniquefnames) == ncol(x)) 
        return(x)
    
    # add 1 since stored from 0, but constructor requires indexing from 1
    new_i <- x@i + 1
    new_j <- as(x, "dgTMatrix")@j + 1
    
    allZeroFeatures <- match(names(which(colSums(x)==0)), uniquefnames)
    
    # combine documents
    if (margin %in% c("both", "documents") & length(uniquednames) < nrow(x))
        new_i <- match(rownames(x), uniquednames)[new_i]
    else
        uniquednames <- rownames(x)
    
    # combine features
    if (margin %in% c("both", "features") & length(uniquefnames) < ncol(x))
        new_j <- match(colnames(x), uniquefnames)[new_j]
    else
        uniquefnames <- colnames(x)
    
    if (nf <- length(allZeroFeatures)) {
        new_i <- c(new_i, rep(1, nf))
        new_j <- c(new_j, allZeroFeatures)
    }
    
    new("dfmSparse", sparseMatrix(i = new_i, j = new_j, 
                                  x = c(x@x, rep(0, length(allZeroFeatures))),
                                  dimnames = list(docs = uniquednames, features = uniquefnames)),
        settings = x@settings,
        weightTf = x@weightTf,
        weightDf = x@weightDf,
        smooth = x@smooth,
        ngrams = x@ngrams,
        concatenator = x@concatenator)
} 

#' convert the case of the features of a dfm and combine
#' 
#' \code{dfm_tolower} and \code{dfm_toupper} convert the features of the dfm to
#' lower and upper case, respectively, and then recombine the counts.
#' @param x the \link{dfm} or \link{fcm} object
#' @importFrom stringi stri_trans_tolower
#' @export
#' @examples
#' # for a document-feature matrix
#' mydfm <- dfm(c("b A A", "C C a b B"), 
#'              toLower = FALSE, verbose = FALSE)
#' mydfm
#' dfm_tolower(mydfm) 
#' dfm_toupper(mydfm)
#'    
dfm_tolower <- function(x) {
    if (!is.dfm(x))
        stop("dfm_tolower requires x to be a dfm object")
    colnames(x) <- stringi::stri_trans_tolower(colnames(x))
    dfm_compress(x)
}

#' @rdname dfm_tolower
#' @importFrom stringi stri_trans_toupper
#' @export
dfm_toupper <- function(x) {
    if (!is.dfm(x))
        stop("dfm_toupper requires x to be a dfm object")
    colnames(x) <- stringi::stri_trans_toupper(colnames(x))
    dfm_compress(x)
}


#' sort a dfm by frequency of one or more margins
#' 
#' Sorts a \link{dfm} by descending frequency of total features, total features
#' in documents, or both.
#' 
#' @param x Document-feature matrix created by \code{\link{dfm}}
#' @param margin which margin to sort on \code{features} to sort by frequency of
#'   features, \code{documents} to sort by total feature counts in documents,
#'   and \code{both} to sort by both
#' @param decreasing TRUE (default) if sort will be in descending order, 
#'   otherwise sort in increasing order
#' @return A sorted \link{dfm} matrix object
#' @export
#' @author Ken Benoit
#' @examples
#' dtm <- dfm(data_corpus_inaugural)
#' dtm[1:10, 1:5]
#' dfm_sort(dtm)[1:10, 1:5]
#' dfm_sort(dtm, decreasing = FALSE, "both")[1:10, 1:5]  
dfm_sort <- function(x, decreasing = TRUE, 
                     margin = c("features", "documents", "both")) {
    if (!is.dfm(x))
        stop("dfm_sort requires x to be a dfm object")
    margin <- match.arg(margin)
    class_xorig <- class(x)
    if (margin=="features") {
        x <- x[, order(colSums(x), decreasing=decreasing)]
    } else if (margin=="documents") {
        x <- x[order(rowSums(x), decreasing=decreasing), ]
    } else if (margin=="both") {
        x <- x[order(rowSums(x), decreasing=decreasing),
               order(colSums(x), decreasing=decreasing)]
    }
    class(x) <- class_xorig
    return(x)
}

