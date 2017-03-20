# deprecated function to compute similarity
# 
# Deprecated; use \code{\link{textstat_simil}} instead.
# @export
# @keywords internal deprecated
# similarity <- function(x, ...) {
#     .Deprecated("textstat_simil")
#     textstat_simil(x, ...)
# }


#' Similarity and distance computation between documents or features
#' 
#' These functions compute matrixes of distances and similarities between 
#' documents or features from a \code{\link{dfm}} and return a 
#' \code{\link[stats]{dist}} object (or a matrix if specific targets are
#' selected).
#' @param x a \link{dfm} object
#' @param selection character vector of document names or feature labels from
#'   \code{x}.  A \code{"dist"} object is returned if selection is \code{NULL}, 
#'   otherwise, a matrix is returned.
#' @param n the top \code{n} highest-ranking items will be returned.  If n is 
#'   \code{NULL}, return all items.  Useful if the output object will be coerced
#'   into a list, for instance if the top \code{n} most similar features to a
#'   target feature is desired.  (See examples.)
#' @param margin identifies the margin of the dfm on which similarity or 
#'   difference will be computed:  \code{documents} for documents or 
#'   \code{features} for word/term features.
#' @param method method the similarity or distance measure to be used; see
#'   Details
#' @param upper  whether the upper triangle of the symmetric \eqn{V \times V} 
#'   matrix is recorded
#' @param diag whether the diagonal of the distance matrix should be recorded
#' @details \code{textstat_simil} options are: \code{"correlation"} (default), 
#'   \code{"cosine"}, \code{"jaccard"}, \code{"eJaccard"}, \code{"dice"},
#'   \code{"eDice"}, \code{"simple matching"}, \code{"hamann"}, and 
#'   \code{"faith"}.
#' @note If you want to compute similarity on a "normalized" dfm object 
#'   (controlling for variable document lengths, for methods such as correlation
#'   for which different document lengths matter), then wrap the input dfm in 
#'   \code{\link{weight}(x, "relFreq")}.
#' @export
#' @seealso \code{\link{textstat_dist}}, \code{\link{as.list.dist}},
#'   \code{\link[base]{dist}}
#' @examples
#' # similarities for documents
#' (s1 <- textstat_simil(presDfm, method = "cosine", margin = "documents"))
#' as.matrix(s1)
#' as.list(s1)
#' 
#' # similarities for for specific documents
#' textstat_simil(presDfm, "2017-Trump", margin = "documents")
#' textstat_simil(presDfm, "2017-Trump", method = "cosine", margin = "documents")
#' textstat_simil(presDfm, c("2009-Obama" , "2013-Obama"), margin = "documents")
#' 
#' # compute some term similarities
#' (s2 <- textstat_simil(presDfm, c("fair", "health", "terror"), method = "cosine", 
#'                       margin = "features", n = 8))
#' as.list(s2)
#' 
textstat_simil <- function(x, selection = NULL, n = NULL,
                           margin = c("documents", "features"),
                           method = "correlation", 
                           upper  = FALSE, diag = FALSE) {
    UseMethod("textstat_simil")
}
    
#' @noRd
#' @export    
textstat_simil.dfm <- function(x, selection = NULL, n = NULL,
                          margin = c("documents", "features"),
                          method = "correlation", 
                          upper  = FALSE, diag = FALSE) {
    margin <- match.arg(margin)
   
    if (!is.null(selection)) {
        if (!is.character(selection)) 
            stop("'selection' should be character or character vector of document names or feature labels.")
        if (margin == "features") {
            selection <- intersect(selection, featnames(x))
            if (!length(selection))
                stop("The features specified by 'selection' do not exist.")
            y <- x[, selection, drop = FALSE]
        } else {
            selection <- intersect(selection, docnames(x))
            if (!length(selection))
                stop("The documents specified by 'selection' do not exist.")
            y <- x[selection, , drop = FALSE]
        }
    } else {
        y <- NULL
    }
    
    vecMethod <- c("cosine", "correlation", "jaccard", "eJaccard", "dice", "eDice", "simple matching", "hamann", "faith")
    
    if (method %in% vecMethod) {
        if (method == "simple matching") method <- "smc"
        temp <- get(paste0(method, "_sparse"))(x, y, margin = ifelse(margin == "documents", 1, 2))
    } else {
        stop("The metric is not currently supported by quanteda, please use other packages such as proxy::dist()/simil().")
    }
    
    # convert NaNs to NA
    # similmatrix[is.nan(similmatrix)] <- NA
    
    if (!is.null(selection)) {
        names <- c(colnames(temp), setdiff(rownames(temp), colnames(temp)))
        temp <- temp[names, , drop = FALSE] # sort for as.dist()
    }
    
    if (!is.null(n)) {
        n <- min(n, nrow(nrow(temp)))
        temp <- temp[seq_len(n), , drop = FALSE]
    }
    
    # create a new dist object
    if (is.null(selection)) {
        result <- stats::as.dist(temp, diag = diag, upper = upper)
        class(result) <- c("simil", class(result))
        attr(result, "method") <- method
        attr(result, "call") <- match.call()
        return(result)
    } else {
        result <- as.matrix(temp)
        if(!is.null(rownames(result)))
            attr(result,"Labels") <- rownames(result)
        else if(!is.null(colnames(result)))
            attr(result,"Labels") <- colnames(result)
        attr(result, "Size") <- ifelse(margin == "documents", nrow(result), ncol(result))
        attr(result, "method") <- method
        attr(result, "call") <- match.call()
        class(result) <- c("dist_selection")
        return(result)
    }
}


## code below based on assoc.R from the qlcMatrix package
## used Matrix::crossprod and Matrix::tcrossprod for sparse Matrix handling

# L2 norm
#norm2 <- function(x,s) { drop(Matrix::crossprod(x^2, s)) ^ 0.5 }
# L1 norm
##norm1 <- function(x,s) { drop(Matrix::crossprod(abs(x),s)) }

#cosine similarity: xy / sqrt(xx * yy)
cosine_sparse <- function(x, y = NULL, margin = 1) {
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    if (margin == 1) x <- t(x)
    S <- rep(1, nrow(x))			
    #N <- Matrix::Diagonal( x = match.fun(norm)(x, S)^-1 )
    N <- Matrix::Diagonal( x = sqrt(colSums(x^2))^-1 )
    x <- x %*% N
    if (!is.null(y)) {
        if (margin == 1) y <- t(y)
        #N <- Matrix::Diagonal( x = match.fun(norm)(y, S)^-1 )
        N <- Matrix::Diagonal( x = sqrt(colSums(y^2))^-1 )
        y <- y %*% N
        return(as.matrix(Matrix::crossprod(x,y)))
    } else
        return(as.matrix(Matrix::crossprod(x)))
}

# Pearson correlation
correlation_sparse <- function(x, y = NULL, margin = 1) {
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    cpFun <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    tcpFun <- if (margin == 2) Matrix::tcrossprod else Matrix::crossprod
    marginSums <- if (margin == 2) colSums else rowSums
    
    n <- if (margin == 2) nrow(x) else ncol(x)
    muX <- if (margin == 2) colMeans(x) else rowMeans(x)
    
    if (!is.null(y)) {
        stopifnot(ifelse(margin == 2, nrow(x) == nrow(y), ncol(x) == ncol(y)))
        muY <- if (margin == 2) colMeans(y) else rowMeans(y)
        covmat <- (as.matrix(cpFun(x,y)) - n * tcrossprod(muX, muY)) / (n-1)
        sdvecX <- sqrt((marginSums(x^2) - n * muX^2) / (n-1))
        sdvecY <- sqrt((marginSums(y^2) - n * muY^2) / (n-1))
        cormat <- covmat / tcrossprod(sdvecX, sdvecY)
    } else {
        covmat <- ( as.matrix(cpFun(x)) - drop(n * tcrossprod(muX)) ) / (n-1)
        sdvec <- sqrt(diag(covmat))
        cormat <- covmat / tcrossprod(sdvec)
    }
    cormat
}

# Jaccard similarity (binary), See http://stackoverflow.com/questions/36220585/efficient-jaccard-similarity-documenttermmatrix
# formula: J = |AB|/(|A| + |B| - |AB|)
jaccard_sparse <- function(x, y = NULL, margin = 1) {
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    # convert to binary matrix
    x <- tf(x, "boolean") 
    
    cpFun <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    marginSums <- if (margin == 2) colSums else rowSums
    marginNames <- if (margin == 2) colnames else rownames
    n <- if (margin == 2) ncol(x) else nrow(x)
    # union 
    an <- marginSums(x)
    if (!is.null(y)) {
        y <- tf(y, "boolean")
        A <- cpFun(x, y)
        bn <- marginSums(y)
        colNm <- marginNames(y)
        # number of features
        kk <- y@Dim[margin]
    } else {
        A <- cpFun(x)
        bn <- an
        kk <- n
        colNm <- marginNames(x)
    }
    rowNm <- marginNames(x)
    # # common values
    # im <- which(A > 0, arr.ind=TRUE, useNames = FALSE)
    # # non-zero values of m
    # Aim <- A[im]
    # 
    # jacmat <- sparseMatrix( i = im[,1],
    #                         j = im[,2],
    #                         x = Aim / (an[im[,1]] + bn[im[,2]] - Aim),
    #                         dims = dim(A)
    #                        )
    tmp <- matrix(rep(an, kk), nrow = n)
    tmp <-  tmp +  matrix(rep(bn, n), nrow = n, byrow=TRUE)
    jacmat <- A / (tmp - A)
    dimnames(jacmat) <- list(rowNm,  colNm)
    jacmat
}

# eJaccard similarity (real-valued data)
# formula: eJ = |AB|/(|A|^2 + |B|^2 - |AB|)
eJaccard_sparse <- function(x, y = NULL, margin = 1) {
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")

    cpFun <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    marginSums <- if (margin == 2) colSums else rowSums
    marginNames <- if (margin == 2) colnames else rownames
    n <- if (margin == 2) ncol(x) else nrow(x)
    # union 
    an <- marginSums(x^2)
    if (!is.null(y)) {
        A <- cpFun(x, y)
        bn <- marginSums(y^2)
        colNm <- marginNames(y)
        # number of features
        kk <- y@Dim[margin]
    } else {
        A <- cpFun(x)
        bn <- an
        kk <- n
        colNm <- marginNames(x)
    }
    rowNm <- marginNames(x)
    # common values
    tmp <- matrix(rep(an, kk), nrow = n)
    tmp <-  tmp +  matrix(rep(bn, n), nrow = n, byrow=TRUE)
    ejacmat <- A / (tmp - A)
    dimnames(ejacmat) <- list(rowNm,  colNm)
    ejacmat
}

# Dice similarity coefficient, binary
# formula: dice = 2|AB|/(|A| + |B|)
dice_sparse <- function(x, y = NULL, margin = 1) {
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    # convert to binary matrix
    x <- tf(x, "boolean") 
    
    cpFun <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    marginSums <- if (margin == 2) colSums else rowSums
    marginNames <- if (margin == 2) colnames else rownames
    n <- if (margin == 2) ncol(x) else nrow(x)
    # union 
    an <- marginSums(x)
    if (!is.null(y)) {
        y <- tf(y, "boolean")
        A <- cpFun(x, y)
        bn <- marginSums(y)
        kk <- y@Dim[margin]
        colNm <- marginNames(y)
    } else {
        A <- cpFun(x)
        bn <- an
        kk <- n
        colNm <- marginNames(x)
    }
    rowNm <- marginNames(x)
    tmp <- matrix(rep(an, kk), nrow = n)
    tmp <-  tmp +  matrix(rep(bn, n), nrow = n, byrow=TRUE)
    dicemat <- (2 * A)/tmp
    #dicemat <- (2 * A)/(an + bn)
    dimnames(dicemat) <- list(rowNm,  colNm)
    dicemat
}

# eDice similarity coefficient, extend from binary Dice to real-valued data 
# formula: eDice = 2|AB|/(|A|^2 + |B|^2)
eDice_sparse <- function(x, y = NULL, margin = 1) {
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    cpFun <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    marginSums <- if (margin == 2) colSums else rowSums
    marginNames <- if (margin == 2) colnames else rownames
    n <- if (margin == 2) ncol(x) else nrow(x)
    # union 
    an <- marginSums(x^2)
    if (!is.null(y)) {
        A <- cpFun(x, y)
        bn <- marginSums(y^2)
        colNm <- marginNames(y)
        kk <- y@Dim[margin]
    } else {
        A <- cpFun(x)
        bn <- an
        kk <- n
        colNm <- marginNames(x)
    }
    rowNm <- marginNames(x)
    # common values
    #im <- which(A > 0, arr.ind=TRUE, useNames = FALSE)
    # non-zero values of m
    #Aim <- A[im]
    
    #eDicemat <- sparseMatrix( i = im[,1],
    #                         j = im[,2],
    #                         x = (2 * Aim) / (an[im[,1]] + bn[im[,2]]),
    #                         dims = dim(A)
    #)
    tmp <- matrix(rep(an, kk), nrow = n)
    tmp <-  tmp +  matrix(rep(bn, n), nrow = n, byrow=TRUE)
    eDicemat <- (2 * A)/tmp
    dimnames(eDicemat) <- list(rowNm,  colNm)
    eDicemat
}

# simple matching coefficient(SMC) 
# formula: SMC = (M00+M11)/(M00+M11+M01+M10)
smc_sparse <- function(x, y = NULL, margin = 1) {
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    # convert to binary matrix
    x <- tf(x, "boolean") 
    x0 <- 1 - x
    cpFun <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    marginSums <- if (margin == 2) nrow else ncol
    marginNames <- if (margin == 2) colnames else rownames
    # union 
    an <- marginSums(x)
    if (!is.null(y)) {
        y <- tf(y, "boolean")
        y0 <- 1 - y
        A <- cpFun(x, y)
        A0 <- cpFun(x0, y0)
        colNm <- marginNames(y)
    } else {
        A <- cpFun(x)
        A0 <- cpFun(x0)
        colNm <- marginNames(x)
    }
    rowNm <- marginNames(x)
    # common values
    A <- A + A0
    smcmat <- A / an
    dimnames(smcmat) <- list(rowNm,  colNm)
    smcmat
}

# Hamann similarity: This measure gives the probability that a characteristic has the same state in both items 
# (present in both or absent from both) minus the probability that a characteristic has different states 
# in the two items (present in one and absent from the other).
# formula: Hamman = ((a+d)-(b+c))/n
# "Hamman" in proxy::dist
hamann_sparse <- function(x, y = NULL, margin = 1) {
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    # convert to binary matrix
    x <- tf(x, "boolean") 
    x0 <- 1 - x
    cpFun <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    marginSums <- if (margin == 2) nrow else ncol
    marginNames <- if (margin == 2) colnames else rownames
    # union 
    an <- marginSums(x)
    if (!is.null(y)) {
        y <- tf(y, "boolean")
        y0 <- 1 - y
        A <- cpFun(x, y)
        A0 <- cpFun(x0, y0)
        colNm <- marginNames(y)
    } else {
        A <- cpFun(x)
        A0 <- cpFun(x0)
        colNm <- marginNames(x)
    }
    rowNm <- marginNames(x)
    # common values
    hamnmat <- (2* (A +A0) - an) / an
    dimnames(hamnmat) <- list(rowNm,  colNm)
    hamnmat
}

# Faith similarity: This measure includes the
# negative match but only gave the half credits while giving
# the full credits for the positive matches.
# formula: Hamman = a+0.5d/n
faith_sparse <- function(x, y = NULL, margin = 1) {
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    # convert to binary matrix
    x <- tf(x, "boolean") 
    x0 <- 1 - x
    cpFun <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    marginSums <- if (margin == 2) nrow else ncol
    marginNames <- if (margin == 2) colnames else rownames
    # union 
    an <- marginSums(x)
    if (!is.null(y)) {
        y <- tf(y, "boolean")
        y0 <- 1 - y
        A <- cpFun(x, y)
        A0 <- cpFun(x0, y0)
        colNm <- marginNames(y)
    } else {
        A <- cpFun(x)
        A0 <- cpFun(x0)
        colNm <- marginNames(x)
    }
    rowNm <- marginNames(x)
    # common values
    faithmat <- (A + 0.5 * A0)/ an
    dimnames(faithmat) <- list(rowNm,  colNm)
    faithmat
}

#' Coerce a simil object into a matrix
#' 
#' Coerces a simil object, which is a specially classed dist object, into a matrix.
#' @param x a simil object from \code{\link{textstat_simil}}
#' @param Diag sets the value for matrix diagonal
#' @param ... unused
#' @export
#' @method as.matrix simil
#' @keywords textstat internal
as.matrix.simil <- function(x, Diag = 1L, ...) {
    size <- attr(x, "Size")
    df <- matrix(0, size, size)
    df[row(df) > col(df)] <- x
    df <- df + t(df)
    labels <- attr(x, "Labels")
    dimnames(df) <- if (is.null(labels))
        list(seq_len(size), seq_len(size))
    else list(labels, labels)
    diag(df) <- Diag
    df
}

