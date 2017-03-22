####################################################################
## methods for dfm objects
##
## Ken Benoit
####################################################################

#' get the feature labels from a dfm
#' 
#' Get the features from a document-feature matrix, which are stored as the
#' column names of the \link{dfm} object.
#' @param x the dfm whose features will be extracted
#' @return character vector of the features
#' @examples
#' inaugDfm <- dfm(data_char_inaugural, verbose = FALSE)
#' 
#' # first 50 features (in original text order)
#' head(featnames(inaugDfm), 50)
#' 
#' # first 50 features alphabetically
#' head(sort(featnames(inaugDfm)), 50)
#' 
#' # contrast with descending total frequency order from topfeatures()
#' names(topfeatures(inaugDfm, 50))
#' @export
featnames <- function(x) {
    UseMethod("featnames")
}

#' @export
#' @noRd
featnames.NULL <- function(x) {
    NULL
}

#' @export
#' @noRd
featnames.dfm <- function(x) {
    colnames(x)
}

#' deprecated function name for featnames
#' 
#' Please use \code{\link{featnames}} instead.
#' @keywords internal deprecated
#' @export
features <- function(x) {
    .Deprecated("featnames")
    featnames(x)
}

#' @noRd
#' @export
docnames.dfm <- function(x) {
    rownames(x)
}

#' @noRd
#' @export
docnames.NULL <- function(x) {
    NULL
}

#' coercion and checking functions for dfm objects
#' 
#' Check for a dfm, or convert
#' a matrix into a dfm.
#' @param x a \link{dfm} object
#' @return 
#' \code{is.dfm} returns \code{TRUE} if and only if its argument is a \link{dfm}.
#' @seealso \code{\link{as.data.frame.dfm}}, \code{\link{as.matrix.dfm}}
#' @export
is.dfm <- function(x) {
    is(x, "dfm")
    # "dfm" %in% class(x)
}

#' @rdname is.dfm
#' @return \code{as.dfm} coerces a matrix or data.frame to a dfm
#' @export
as.dfm <- function(x) {
    if (!any((c("matrix", "data.frame") %in% class(x))))
        stop("as.dfm only applicable to matrix(-like) objects.")
    new("dfmSparse", Matrix(as.matrix(x), 
                            sparse = TRUE,
                            dimnames = list(docs = if (is.null(rownames(x))) paste0("doc", seq_len(nrow(x))) else rownames(x),
                                            features = if (is.null(colnames(x))) paste0("feat", seq_len(ncol(x))) else colnames(x)) 
                            ) 
        )
}



#' list the most frequent features
#' 
#' List the most (or least) frequently occuring features in a \link{dfm}.
#' @name topfeatures
#' @param x the object whose features will be returned
#' @param n how many top features should be returned
#' @param decreasing If TRUE, return the \code{n} most frequent features, if 
#'   FALSE, return the \code{n} least frequent features
#' @param ci confidence interval from 0-1.0 for use if dfm is resampled
#' @return A named numeric vector of feature counts, where the names are the
#'   feature labels.
#' @examples
#' # most frequent features
#' topfeatures(dfm(corpus_subset(data_corpus_inaugural, Year > 1980), verbose = FALSE))
#' topfeatures(dfm(corpus_subset(data_corpus_inaugural, Year > 1980), 
#'             remove = stopwords("english"), verbose = FALSE))
#'             
#' # least frequent features
#' topfeatures(dfm(corpus_subset(data_corpus_inaugural, Year > 1980), verbose = FALSE), 
#'             decreasing = FALSE)
#' @export
topfeatures <- function(x, n = 10, decreasing = TRUE, ci = .95) {
    UseMethod("topfeatures")
}

#' @export
#' @noRd
#' @importFrom stats quantile
topfeatures.dfm <- function(x, n = 10, decreasing = TRUE, ci = .95) {
    if (n > nfeature(x)) n <- nfeature(x)
    if (is.resampled(x)) {
        subdfm <- x[, order(colSums(x[,,1]), decreasing=decreasing), ]
        subdfm <- subdfm[, 1:n, ]   # only top n need to be computed
        return(data.frame(#features=colnames(subdfm),
            freq=colSums(subdfm[,,1]),
            cilo = apply(colSums(subdfm), 1, stats::quantile, (1-ci)/2),
            cihi = apply(colSums(subdfm), 1, stats::quantile, 1-(1-ci)/2)))
    } else {
        subdfm <- sort(colSums(x), decreasing)
        return(subdfm[1:n])
    }
}

#' @export
#' @noRd
topfeatures.dgCMatrix <- function(x, n = 10, decreasing = TRUE, ci = .95) {
    if (is.null(n)) n <- ncol(x)
    #     if (is.resampled(x)) {
    #         subdfm <- x[, order(colSums(x[,,1]), decreasing=decreasing), ]
    #         subdfm <- subdfm[, 1:n, ]   # only top n need to be computed
    #         return(data.frame(#features=colnames(subdfm),
    #             freq=colSums(subdfm[,,1]),
    #             cilo=apply(colSums(subdfm), 1, quantile, (1-ci)/2),
    #             cihi=apply(colSums(subdfm), 1, quantile, 1-(1-ci)/2)))
    #     } else {
    
    csums <- colSums(x)
    names(csums) <- x@Dimnames$features
    subdfm <- sort(csums, decreasing)
    return(subdfm[1:n])
    #    }
}



#' compute the sparsity of a document-feature matrix
#'
#' Return the proportion of sparseness of a document-feature matrix, equal
#' to the proportion of cells that have zero counts.
#' @param x the document-feature matrix
#' @examples 
#' inaug_dfm <- dfm(data_corpus_inaugural, verbose = FALSE)
#' sparsity(inaug_dfm)
#' sparsity(dfm_trim(inaug_dfm, min_count = 5))
#' @export
sparsity <- function(x) {
    if (!is.dfm(x))
        stop("sparsity is only defined for dfm objects")
    (1 - length(x@x) / prod(dim(x)))
}

