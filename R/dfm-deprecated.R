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
#' \dontrun{
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


#' sort a dfm by one or more margins
#'
#' Sorts a \link{dfm} by frequency of total features, total features in
#' documents, or both
#'
#' @param x Document-feature matrix created by \code{\link{dfm}}
#' @param margin which margin to sort on \code{features} to sort by frequency of
#'   features, \code{docs} to sort by total feature counts in documents, and
#'   \code{both} to sort by both
#' @param decreasing TRUE (default) if sort will be in descending order,
#'   otherwise sort in increasing order
#' @param ... additional arguments passed to base method \code{sort.int}
#' @note Deprecated; use \code{\link{dfm_sort}} instead.
#' @return A sorted \link{dfm} matrix object
#' @keywords internal deprecated
#' @seealso dfm_sort
#' @export
sort.dfm <- function(x, decreasing=TRUE, margin = c("features", "docs", "both"), ...) {
    .Deprecated("dfm_sort")
    margin <- match.arg(margin)
    class_xorig <- class(x)
    if (margin=="features") {
        x <- x[, order(colSums(x), decreasing=decreasing)]
    } else if (margin=="docs") {
        x <- x[order(rowSums(x), decreasing=decreasing), ]
    } else if (margin=="both") {
        x <- x[order(rowSums(x), decreasing=decreasing),
               order(colSums(x), decreasing=decreasing)]
    }
    class(x) <- class_xorig
    return(x)
}

#' deprecated name for dfm_trim
#' 
#' Deprecated function name for \code{\link{dfm_trim}}.
#' @param x dfm whose features or documents will be trimmed
#' @param ... arguments passed to \code{dfm_trim}

#' @note Deprecated; use \code{\link{dfm_trim}} instead.
#' @keywords internal deprecated
#' @export
trim <- function(x, ...) {
    UseMethod("trim")
}

#' @rdname trim
#' @export
trim.dfm <- function(x, ...) {
    .Deprecated("dfm_trim")
    dfm_trim(x, ...)
}

#' @rdname trim
#' @export
trimdfm <- function(x, ...) {
    catm("note: trimdfm deprecated: use trim instead.\n")
    UseMethod("trim")
}

#' @rdname sample
#' @export
sample.dfm <- function(x, ...) {
    .Deprecated("dfm_sample")
    dfm_sample(x, ...)
}

#' weight or smooth a dfm
#' 
#' Deprecated weighting and smoothing functions for dfm objects.  See instead
#' \code{\link{dfm_weight}} and \code{\link{dfm_smooth}}.
#' @param x document-feature matrix created by \link{dfm}
#' @param ... arguments passed to \code{\link{dfm_weight}} and and \code{\link{dfm_smooth}}
#' @keywords internal deprecated
#' @export
weight <- function(x, ...) {
    .Deprecated("dfm_weight")
    dfm_weight(x, ...)
}
    

#' @rdname weight
#' @export
smoother <- function(x, ...) {
    .Deprecated("dfm_smooth")
    dfm_smooth(x, ...)
}

#' @rdname applyDictionary
#' @details 
#' \code{applyDictionary.dfm} is the deprecated function name for \code{\link{dfm_lookup}}.
#' @export
applyDictionary.dfm <- function(x, ...) {
    .Deprecated("dfm_lookup")
    dfm_lookup(x, ...)
}

