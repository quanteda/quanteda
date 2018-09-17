#' Similarity and distance computation between documents or features
#' 
#' These functions compute matrixes of distances and similarities between 
#' documents or features from a \code{\link{dfm}} and return a 
#' \code{\link[stats]{dist}} object (or a matrix if specific targets are
#' selected).  They are fast and robust because they operate directly on the sparse
#' \link{dfm} objects.
#' @param x a \link{dfm} object
#' @param selection a valid index for document or feature names from \code{x},
#'   to be selected for comparison
#' @param margin identifies the margin of the dfm on which similarity or
#'   difference will be computed:  \code{"documents"} for documents or 
#'   \code{"features"} for word/term features
#' @param method method the similarity or distance measure to be used; see
#'   Details
#' @param upper  whether the upper triangle of the symmetric \eqn{V \times V} 
#'   matrix is recorded
#' @param diag whether the diagonal of the distance matrix should be recorded
#' @details \code{textstat_simil2} options are: \code{"correlation"} (default), 
#'   \code{"cosine"}, \code{"jaccard"}, \code{"ejaccard"}, \code{"dice"},
#'   \code{"edice"}, \code{"simple matching"}, \code{"hamann"}, and 
#'   \code{"faith"}.
#' @note If you want to compute similarity on a "normalized" dfm object 
#'   (controlling for variable document lengths, for methods such as correlation
#'   for which different document lengths matter), then wrap the input dfm in 
#'   \code{\link{dfm_weight}(x, "prop")}.
#' @return \code{textstat_simil2} and \code{textstat_dist} return
#'   \code{\link{dist}} class objects if selection is \code{NULL}, otherwise, a
#'   matrix is returned matching distances to the documents or features
#'   identified in the selection.
#' @export
#' @seealso \code{\link{textstat_dist}}, \code{\link{as.list.dist}},
#'   \code{\link{dist}}
#' @examples
#' # similarities for documents
#' pres_dfm <- dfm(data_corpus_inaugural, remove_punct = TRUE, remove = stopwords("english"))
#' (s1 <- textstat_simil2(pres_dfm, method = "cosine", margin = "documents"))
#' as.matrix(s1)
#' as.list(s1)
#' 
#' # similarities for for specific documents
#' textstat_simil2(pres_dfm, "2017-Trump", margin = "documents")
#' textstat_simil2(pres_dfm, "2017-Trump", method = "cosine", margin = "documents")
#' textstat_simil2(pres_dfm, c("2009-Obama" , "2013-Obama"), margin = "documents")
#' 
#' # compute some term similarities
#' s2 <- textstat_simil2(pres_dfm, c("fair", "health", "terror"), method = "cosine", 
#'                       margin = "features")
#' head(as.matrix(s2), 10)
#' as.list(s2, n = 8)
#' 
textstat_simil2 <- function(x, selection = NULL,
                           margin = c("documents", "features"),
                           method = c("cosine", "correlation"), 
                           upper  = FALSE, diag = FALSE, min_simil = 0) {
    UseMethod("textstat_simil2")
}
    

#' @export    
textstat_simil2.default <- function(x, selection = NULL,
                               margin = c("documents", "features"),
                               method = c("cosine", "correlation"), 
                               upper  = FALSE, diag = FALSE, min_simil = 0) {
    stop(friendly_class_undefined_message(class(x), "textstat_simil2"))
}
    
#' @export    
textstat_simil2.dfm <- function(x, selection = NULL,
                          margin = c("documents", "features"),
                          method = c("cosine", "correlation"), 
                          upper  = FALSE, diag = FALSE, min_simil = 0) {
    x <- as.dfm(x)
    if (!sum(x)) stop(message_error("dfm_empty"))
    margin <- match.arg(margin)
    method <- match.arg(method)
    
    if (margin == "documents") {
        if (is.null(selection)) {
            i <- seq(ndoc(x))
        } else {
            i <- match(selection, docnames(x))
            upper <- TRUE
        }
        m <- 1
    } else {
        if (is.null(selection)) {
            i <- seq(nrow(x))
        } else {
            i <- match(selection, rownames(x))
            upper <- TRUE
        }
        m <- 2
    }
    
    if (method == "cosine") {
        result <- qatd_cpp_cosine(mt, i, m, min_simil)
    } else if  (method == "correlation") {
        result <- qatd_cpp_cosine(mt, i, m, min_simil)
    }
    
    if (!upper)
        result <- result + t(result)
    if (diag)
        diag(result) <- 1
    
    
    label <- if (margin == "documents") rownames(x) else colnames(x)
    rownames(result) <- label
    if (is.null(selection)) {
        colnames(result) <- label
    } else {
        result <- result[,i, drop = FALSE]
        colnames(result) <- label[i]
    }
    
    return(result)
}


## code below based on assoc.R from the qlcMatrix package
## used Matrix::crossprod and Matrix::tcrossprod for sparse Matrix handling

# L2 norm
# norm2 <- function(x,s) { drop(Matrix::crossprod(x ^ 2, s)) ^ 0.5 }
# L1 norm
# norm1 <- function(x,s) { drop(Matrix::crossprod(abs(x),s)) }

# cosine similarity: xy / sqrt(xx * yy)
cosine_simil <- function(x, y = NULL, margin = 1) {
    
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    if (margin == 1) x <- t(x)
    S <- rep(1, nrow(x))
    N <- Matrix::Diagonal(x = sqrt(colSums(x ^ 2)) ^ -1)
    x <- x %*% N
    if (!is.null(y)) {
        if (margin == 1) y <- t(y)
        N <- Matrix::Diagonal(x = sqrt(colSums(y ^ 2)) ^ -1)
        y <- y %*% N
        return(as.matrix(Matrix::crossprod(x, y)))
    } else
        return(as.matrix(Matrix::crossprod(x)))
}

# Pearson correlation
correlation_simil <- function(x, y = NULL, margin = 1) {
    
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    func_cp <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    func_tcp <- if (margin == 2) Matrix::tcrossprod else Matrix::crossprod
    func_sum <- if (margin == 2) colSums else rowSums
    
    n <- if (margin == 2) nrow(x) else ncol(x)
    mux <- if (margin == 2) colMeans(x) else rowMeans(x)
    
    if (!is.null(y)) {
        stopifnot(if (margin == 2) nrow(x) == nrow(y) else ncol(x) == ncol(y))
        muy <- if (margin == 2) colMeans(y) else rowMeans(y)
        covmat <- (as.matrix(func_cp(x,y)) - n * tcrossprod(mux, muy)) / (n - 1)
        sdvecX <- sqrt((func_sum(x ^ 2) - n * mux ^ 2) / (n - 1))
        sdvecY <- sqrt((func_sum(y ^ 2) - n * muy ^ 2) / (n - 1))
        cormat <- covmat / tcrossprod(sdvecX, sdvecY)
    } else {
        covmat <- (as.matrix(func_cp(x)) - drop(n * tcrossprod(mux))) / (n - 1)
        sdvec <- sqrt(diag(covmat))
        cormat <- covmat / tcrossprod(sdvec)
    }
    cormat
}
