#' recombine a dfm or fcm by combining identical dimension elements
#' 
#' "Compresses" or groups a \link{dfm} or \link{fcm} whose dimension names are
#' the same, for either documents or features.  This may happen, for instance,
#' if features are made equivalent through application of a thesaurus.  It could also be needed after a 
#' \code{\link{cbind.dfm}} or \code{\link{rbind.dfm}} operation.  In most cases, you will not
#' need to call `dfm_compress`, since it is called automatically by functions that change the 
#' dimensions of the dfm, e.g. \code{\link{dfm_tolower}}.
#' 
#' @param x input object, a \link{dfm} or \link{fcm}
#' @param margin character indicating on which margin to compress a dfm, either 
#'   \code{"documents"}, \code{"features"}, or \code{"both"} (default).  For fcm
#'   objects, \code{"documents"} has no effect.
#' @param ... additional arguments passed from generic to specific methods
#' @return \code{dfm_compress} returns a \link{dfm} whose dimensions have been
#'   recombined by summing the cells across identical dimension names
#'   (\link{docnames} or \link{featnames}).  The \link{docvars} will be
#'   preserved for combining by features but not when documents are combined.
#' @export
#' @examples 
#' # dfm_compress examples
#' mat <- rbind(dfm(c("b A A", "C C a b B"), tolower = FALSE),
#'              dfm("A C C C C C", tolower = FALSE))
#' colnames(mat) <- char_tolower(featnames(mat))
#' mat
#' dfm_compress(mat, margin = "documents")
#' dfm_compress(mat, margin = "features")
#' dfm_compress(mat)
#' 
#' # no effect if no compression needed
#' compactdfm <- dfm(data_corpus_inaugural[1:5])
#' dim(compactdfm)
#' dim(dfm_compress(compactdfm))
#' 
dfm_compress <- function(x, margin = c("both", "documents", "features")) {
    UseMethod("dfm_compress")
}
    
#' @noRd
#' @export
dfm_compress.dfm <- function(x, margin = c("both", "documents", "features")) {
    
    x <- as.dfm(x)
    margin <- match.arg(margin)
    if (margin == 'documents') {
        result <- group_dfm(x, NULL, docnames(x))
    } else if (margin == 'features') {
        result <- group_dfm(x, featnames(x), NULL)
    } else {
        result <- group_dfm(x, featnames(x), docnames(x))
    }
    return(result)
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
#' @param decreasing logical; if \code{TRUE}, the sort will be in descending order,
#'   otherwise sort in increasing order
#' @return A sorted \link{dfm} matrix object
#' @export
#' @author Ken Benoit
#' @examples
#' dtm <- dfm(data_corpus_inaugural)
#' head(dtm)
#' head(dfm_sort(dtm))
#' head(dfm_sort(dtm, decreasing = FALSE, "both"))
dfm_sort <- function(x, decreasing = TRUE, 
                     margin = c("features", "documents", "both")) {
    UseMethod("dfm_sort")
}

#' @noRd
#' @export
dfm_sort.dfm <- function(x, decreasing = TRUE, 
                         margin = c("features", "documents", "both")) {
    
    x <- as.dfm(x)
    margin <- match.arg(margin)
    class_org <- class(x)
    if (margin=="features") {
        x <- x[, order(colSums(x), decreasing=decreasing)]
    } else if (margin=="documents") {
        x <- x[order(rowSums(x), decreasing=decreasing), ]
    } else if (margin=="both") {
        x <- x[order(rowSums(x), decreasing=decreasing),
               order(colSums(x), decreasing=decreasing)]
    }
    class(x) <- class_org
    return(x)
}

