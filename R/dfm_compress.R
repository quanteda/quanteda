#' recombine a dfm or fcm by combining identical dimension elements
#' 
#' "Compresses" or groups a \link{dfm} or \link{fcm} whose dimension names are
#' the same, for either documents or features.  This may happen, for instance,
#' if features are made equivalent through application of a thesaurus.  It may
#' also occur after lower-casing or stemming the features of a dfm, but this
#' should only be done in very rare cases (approaching never: it's better to do
#' this \emph{before} constructing the dfm.)  It could also be needed after a 
#' \code{\link{cbind.dfm}} or \code{\link{rbind.dfm}} operation.
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
dfm_compress.dfmSparse <- function(x, margin = c("both", "documents", "features")) {
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

#' @rdname dfm_compress
#' @description \code{dfm_group} allows combining dfm documents by a grouping 
#'   variable, which can also be one of the \link{docvars} attached to the dfm.  This is
#'   identical in functionality to using the \code{"groups"} argument in
#'   \code{\link{dfm}}.
#' @param groups either: a character vector containing the names of document 
#'   variables to be used for grouping; or a factor or object that can be 
#'   coerced into a factor equal in length or rows to the number of documents
#' @return \code{dfm_group} returns a \link{dfm} whose documents are equal to
#'   the unique group combinations, and whose cell values are the sums of the
#'   previous values summed by group.  This currently erases any docvars in the dfm.
#' @export
#' @examples
#' # dfm_group examples
#' mycorpus <- corpus(c("a a b", "a b c c", "a c d d", "a c c d"), 
#'                    docvars = data.frame(grp = c("grp1", "grp1", "grp2", "grp2")))
#' mydfm <- dfm(mycorpus)
#' dfm_group(mydfm, groups = "grp")
#' dfm_group(mydfm, groups = c(1, 1, 2, 2))
#' 
#' # equivalent
#' dfm(mydfm, groups = "grp")
#' dfm(mydfm, groups = c(1, 1, 2, 2))
dfm_group <- function(x, groups = NULL) {
    dfm(x, groups = groups, tolower = FALSE)
}


#
# internal code to perform dfm compression and grouping
# on features and/or documents
group_dfm <- function(x, features = NULL, documents = NULL) {
    
    if (is.null(features) && is.null(documents)) {
        return(x)
    }
    
    temp <- as(x, "dgTMatrix")
    
    if (is.null(features)) {
        features_unique <- temp@Dimnames[[2]]
        j_new <- temp@j + 1
    } else {
        features_unique <- unique(features)
        features_index <- match(features, features_unique)
        j_new <- features_index[temp@j + 1]
        docvars_new <- docvars(x)
    }
    if (is.null(documents)) {
        documents_unique <- temp@Dimnames[[1]]
        i_new <- temp@i + 1
    } else {
        documents_unique <- unique(documents)
        documents_index <- match(documents, documents_unique)
        i_new <- documents_index[temp@i + 1]
        docvars_new <- data.frame()
    }
    x_new <- temp@x
    dims <- c(length(documents_unique), length(features_unique))
    dimnames <- list(docs = as.character(documents_unique), 
                     features = as.character(features_unique))
    
    result <- new("dfmSparse", 
                  sparseMatrix(i = i_new, j = j_new, x = x_new, dims = dims, dimnames = dimnames),
                  settings = x@settings,
                  weightTf = x@weightTf,
                  weightDf = x@weightDf,
                  smooth = x@smooth,
                  ngrams = x@ngrams,
                  skip = x@skip,
                  concatenator = x@concatenator,
                  docvars = docvars_new)
    
    return(result)
}

#' @noRd
#' @export
#' @examples 
#' # for dfmDense
#' mat <- rbind(dfm(c("b A A", "C C a b B"), tolower = FALSE, verbose = FALSE),
#'              dfm("A C C C C C", tolower = FALSE, verbose = FALSE))
#' matd <- dfm_smooth(mat)
#' colnames(matd) <- char_tolower(featnames(mat))
#' matd
#' dfm_compress(matd, margin = "documents")
#' dfm_compress(matd, margin = "features")
#' dfm_compress(matd)
dfm_compress.dfmDense <- function(x, ...) {
    dfm_compress(new("dfmSparse", Matrix::Matrix(as.matrix(x), sparse = TRUE),
                     settings = x@settings,
                     weightTf = x@weightTf,
                     weightDf = x@weightDf,
                     smooth = x@smooth,
                     ngrams = x@ngrams,
                     skip = x@skip,
                     concatenator = x@concatenator,
                     docvars = x@docvars),
                 ...)
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
    colnames(x) <- stri_trans_tolower(colnames(x))
    dfm_compress(x, margin = "features")
}

#' @rdname dfm_tolower
#' @importFrom stringi stri_trans_toupper
#' @export
dfm_toupper <- function(x) {
    if (!is.dfm(x))
        stop("dfm_toupper requires x to be a dfm object")
    colnames(x) <- stri_trans_toupper(colnames(x))
    dfm_compress(x, margin = "features")
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

