
#' compute similarities between documents and/or features
#' 
#' Compute similarities between documents and/or features from a 
#' \code{\link{dfm}}. Uses the similarity measures defined in 
#' \link[proxy]{simil}.  See \code{\link[proxy]{pr_DB}} for available distance 
#' measures, or how to create your own.
#' @param x a \link{dfm} object
#' @param selection character or character vector of document names or feature 
#'   labels from the dfm
#' @param n the top \code{n} most similar items will be returned, sorted in 
#'   descending order.  If n is \code{NULL}, return all items.
#' @param margin identifies the margin of the dfm on which similarity will be 
#'   computed:  \code{documents} for documents or \code{features} for word/term
#'   features.
#' @param method a valid method for computing similarity from 
#'   \code{\link[proxy]{pr_DB}}
#' @param sorted sort results in descending order if \code{TRUE}
#' @param normalize a deprecated argument retained (temporarily) for legacy 
#'   reasons.  If you want to compute similarity on a "normalized" dfm objects 
#'   (e.g. \code{x}), wrap it in \code{\link{weight}(x, "relFreq")}.
#' @return a named list of the selection labels, with a sorted named vector of 
#'   similarity measures.
#' @note The method for computing feature similarities can be quite slow when 
#'   there are large numbers of feature types.  Future implementations will 
#'   hopefully speed this up.
#' @examples
#' \donttest{
#' # create a dfm from inaugural addresses from Reagan onwards
#' presDfm <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980), stem = TRUE,
#'                remove = stopwords("english"))
#' 
#' # compute some document similarities
#' (tmp <- similarity(presDfm, margin = "documents"))
#' # output as a matrix
#' as.matrix(tmp)
#' # for specific comparisons
#' similarity(presDfm, "1985-Reagan", n = 5, margin = "documents")
#' similarity(presDfm, c("2009-Obama" , "2013-Obama"), n = 5, margin = "documents")
#' similarity(presDfm, c("2009-Obama" , "2013-Obama"), margin = "documents")
#' similarity(presDfm, c("2009-Obama" , "2013-Obama"), margin = "documents", method = "cosine")
#' similarity(presDfm, "2005-Bush", margin = "documents", method = "eJaccard", sorted = FALSE)
#' 
#' # compute some term similarities
#' similarity(presDfm, c("fair", "health", "terror"), method="cosine", margin = "features", 20)
#' }
#' @keywords internal deprecated
#' @export
setGeneric("similarity", 
           signature = c("x", "selection", "n", "margin", "sorted", "normalize"),
           def = function(x, selection = NULL, n = NULL, 
                          margin = c("documents", "features"),
                          method = "correlation",
                          sorted = TRUE, normalize = FALSE)
               standardGeneric("similarity"))

#' @rdname similarity
#' @export
setMethod("similarity", 
           signature = signature("dfm", "ANY"),
           def = function(x, selection = NULL, n = NULL, 
                          margin = c("documents", "features"),
                          method = "correlation",
                          sorted = TRUE, normalize = FALSE) {

               .Deprecated("textstat_simil")
               
               # value <- match.arg(value)

               if (normalize) {
                   warning("normalize is deprecated, not applied - use weight() instead")
                   # x <- weight(x, "relFreq")  # normalize by term freq.
               }
               
               margin <- match.arg(margin)
               if (margin == "features") {
                   items <- featnames(x)
               } else {
                   items <- docnames(x)
               }
               
               if (is.null(n) || n >= length(items))
                   n <- length(items) - 1 # choose all features/docs if n is NULL
               
               if (!is.null(selection)) {
                   # retain only existing features or documents
                   selectIndex <- which(items %in% selection)
                   if (length(selectIndex)==0)
                       stop("no such documents or feature labels exist.")
                   
                   if (margin=="features") {
                       xSelect <- x[, selectIndex, drop=FALSE]
                   } else {
                       xSelect <- x[selectIndex, , drop=FALSE]
                   }
               } else xSelect <- NULL

               if (method == "cosine") {
                   similmatrix <- cosine_sparse(x, xSelect, margin = ifelse(margin == "documents", 1, 2))
               } else if (method == "correlation") {
                   similmatrix <- correlation_sparse(x, xSelect, margin = ifelse(margin == "documents", 1, 2))
               } else {
                   # use proxy::simil() for all other methods
                   similmatrix <- as.matrix(proxy::simil(as.matrix(x), as.matrix(xSelect), method = method, 
                                                         by_rows = ifelse(margin=="features", FALSE, TRUE)), diag = 1)
               }
               
               # rounding
               # if (!is.null(digits)) similmatrix <- round(similmatrix, digits)
               
               # convert NaNs to NA
               # similmatrix[is.nan(similmatrix)] <- NA
               
               # return the matrix if this was requested
               # return(similmatrix)
               
               # convert the matrix to a list of similarities
               result <- lapply(seq_len(ncol(similmatrix)), function(i) similmatrix[, i])
               names(result) <- if (!is.null(xSelect)) items[selectIndex] else if (margin == "documents") docnames(x) else featnames(x)
               
               # remove the element of each similarity vector equal to the item itself
               tempseq <- seq_along(result)
               names(tempseq) <- names(result)
               result <- lapply( tempseq, function(i)
                   result[[i]] <- result[[i]][-which(names(result[[i]]) == names(result)[i])] )

               # sort each element of the list and return only first n results if n not NULL
               if (sorted == TRUE)
                   result <- lapply(result, sort, decreasing=TRUE, na.last = TRUE)
               
               # truncate to n if n is not NULL
               if (!is.null(n))
                   result <- lapply(result, "[", 1:n)

               # return a vector if list of length 1, otherwise return a named list
               # if (length(result)==1) result <- result[[1]]
               
               class(result) <- c("similMatrix", class(result))
               result
           })


#' @rdname similarity
#' @param ... unused
#' @export
#' @method as.matrix similMatrix
as.matrix.similMatrix <- function(x, ...) {
    itemNames <- names(x)
    # add back the identity similarity for each list element
    x <- lapply(seq_along(x), function(i) {
        result <- c(1.0, x[[i]])
        names(result)[1] <- names(x)[i]
        result
    })
    names(x) <- itemNames

    # now combine the lists taking into account that features may differ
    tmpdf <- do.call(rbind, lapply(lapply(x, unlist), "[",
                                   unique(unlist(c(sapply(x, names))))))
    tmpdf <- as.data.frame(tmpdf)
    names(tmpdf) <- unique(unlist(c(sapply(x, names))))
    
    as.matrix(t(tmpdf))
}



#' @noRd
#' @method print similMatrix
#' @export
print.similMatrix <- function(x, ...) {
    # x <- lapply(x, round, digits)
    cat("similarity Matrix:\n")
    print(unclass(x), ...)
}

