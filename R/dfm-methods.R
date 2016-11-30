####################################################################
## methods for dfm objects
##
## Ken Benoit
####################################################################

#' @export
#' @rdname ndoc
ndoc.dfm <- function(x) {
    nrow(x)
}


#' extract the feature labels from a dfm
#'
#' Extract the features from a document-feature matrix, which are stored as the column names
#' of the \link{dfm} object.
#' @param x the object (dfm) whose features will be extracted
#' @return Character vector of the features
#' @examples
#' inaugDfm <- dfm(data_char_inaugural, verbose = FALSE)
#' 
#' # first 50 features (in original text order)
#' head(features(inaugDfm), 50)
#' 
#' # first 50 features alphabetically
#' head(sort(features(inaugDfm)), 50)
#' 
#' # contrast with descending total frequency order from topfeatures()
#' names(topfeatures(inaugDfm, 50))
#' @export
features <- function(x) {
    UseMethod("features")
}

#' @export
#' @rdname features
features.dfm <- function(x) {
    colnames(x)
}

#' @rdname docnames
#' @examples
#' # query the document names of a dfm
#' docnames(dfm(data_char_inaugural[1:5]))
#' @export
docnames.dfm <- function(x) {
    rownames(x)
}

#' @details \code{is.dfm} returns \code{TRUE} if and only if its argument is a \link{dfm}.
#' @rdname dfm-class
#' @export
is.dfm <- function(x) {
    is(x, "dfm")
    # "dfm" %in% class(x)
}

#' @details \code{as.dfm} coerces a matrix or data.frame to a dfm
#' @rdname dfm-class
#' @export
as.dfm <- function(x) {
    if (!any((c("matrix", "data.frame") %in% class(x))))
        stop("as.dfm only applicable to matrix(-like) objects.")
    new("dfmSparse", Matrix(as.matrix(x), sparse=TRUE))
    #     
    #     m <- as.matrix(x)
    #     attr(m, "settings") <- attr(x, "settings")
    #     attr(m, "weighting") <- attr(x, "weighting")
    #     class(m) <- class(x)
    #     m
}

#' @rdname ndoc
#' @export
nfeature <- function(x) {
    UseMethod("nfeature")
}

#' @rdname ndoc
#' @export
nfeature.corpus <- function(x) {
    stop("nfeature not yet implemented for corpus objects.")
}

#' @rdname ndoc
#' @description \code{nfeature} is an alias for \code{ntype} when applied to dfm
#'   objects.  For a corpus or set of texts, "features" are only defined through
#'   tokenization, so you need to use \code{\link{ntoken}} to count these.
#' @export
#' @examples
#' nfeature(dfm(data_corpus_inaugural))
#' nfeature(dfm_trim(dfm(data_corpus_inaugural), min_docfreq = 5, min_count = 10))
nfeature.dfm <- function(x) {
    ncol(x)
}


#' list the most frequent features
#'
#' List the most frequently occuring features in a \link{dfm}
#' @name topfeatures
#' @aliases topFeatures
#' @param x the object whose features will be returned
#' @param n how many top features should be returned
#' @param decreasing If TRUE, return the \code{n} most frequent features, if
#'   FALSE, return the \code{n} least frequent features
#' @param ci confidence interval from 0-1.0 for use if dfm is resampled
#' @param ... additional arguments passed to other methods
#' @export
topfeatures <- function(x, ...) {
    UseMethod("topfeatures")
}

#' @return A named numeric vector of feature counts, where the names are the feature labels.
#' @examples
#' topfeatures(dfm(corpus_subset(data_corpus_inaugural, Year > 1980), verbose = FALSE))
#' topfeatures(dfm(corpus_subset(data_corpus_inaugural, Year > 1980), 
#'             remove = stopwords("english"), verbose = FALSE))
#' # least frequent features
#' topfeatures(dfm(corpus_subset(data_corpus_inaugural, Year > 1980), verbose = FALSE), 
#'             decreasing = FALSE)
#' @export
#' @rdname topfeatures
#' @importFrom stats quantile
topfeatures.dfm <- function(x, n = 10, decreasing = TRUE, ci = .95, ...) {
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
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
#' @rdname topfeatures
topfeatures.dgCMatrix <- function(x, n=10, decreasing=TRUE, ...) {
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


# rnames <- colnames(x)
# dnames <- rownames(x)
# microbenchmark::microbenchmark(m1 = t(crossprod(x, Matrix(sapply(unique(dnames),"==", dnames)))),
#                                m2 = t(t(x) %*% Matrix(sapply(unique(dnames),"==", dnames))), 
#                                times = 100)
 
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

