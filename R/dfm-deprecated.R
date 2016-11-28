#' compress a dfm by combining similarly named dimensions
#' 
#' "Compresses" a dfm whose dimension names are the same, for either documents 
#' or features.  This may happen, for instance, if features are made equivalent 
#' through application of a thesaurus.  It may also occur after lower-casing or 
#' stemming the features of a dfm, but this should only be done in very rare 
#' cases (approaching never: it's better to do this \emph{before} constructing 
#' the dfm.)  It could also be needed , after a \code{\link{cbind.dfm}} or 
#' \code{\link{rbind.dfm}} operation.
#' 
#' @param x input object, a \link{dfm}
#' @param margin character indicating which margin to compress on, either
#'   \code{"documents"}, \code{"features"}, or \code{"both"} (default)
#' @param ... additional arguments passed from generic to specific methods
#' @note This function is deprecated: use \code{\link{dfm_compress}} instead.
#' @export
#' @keywords internal deprecated
compress <- function(x, ...)  {
    UseMethod("compress")
}

#' @rdname compress 
#' @examples 
#' \donttest{
#' mat <- rbind(dfm(c("b A A", "C C a b B"), toLower = FALSE, verbose = FALSE),
#'              dfm("A C C C C C", toLower = FALSE, verbose = FALSE))
#' colnames(mat) <- toLower(features(mat))
#' mat
#' compress(mat, margin = "documents")
#' compress(mat, margin = "features")
#' compress(mat)
#' 
#' # no effect if no compression needed
#' compress(dfm(data_char_inaugural, verbose = FALSE))
#' }
#' @export
compress.dfm <- function(x, margin = c("both", "documents", "features"), ...) {
    .Deprecated("dfm_compress")
    
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
