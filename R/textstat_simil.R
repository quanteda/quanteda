# deprecated function to compute similarity
# 
# Deprecated; use \code{\link{textstat_simil}} instead.
# @export
# @keywords internal deprecated
# similarity <- function(x, ...) {
#     .Deprecated("textstat_simil")
#     textstat_simil(x, ...)
# }

#' @param x a \link{dfm} object
#' @param selection character or character vector of document names or feature 
#'   labels from the dfm
#' @param n the top \code{n} highest-ranking items will be returned.  If n is 
#'   \code{NULL}, return all items.
#' @param margin identifies the margin of the dfm on which similarity or 
#'   difference will be computed:  \code{documents} for documents or 
#'   \code{features} for word/term features.
#' @param method method the distance measure to be used; see Details
#' @param upper  whether the upper triangle of the symmetric \eqn{V \times V} 
#'   matrix is recorded
#' @param diag whether the diagonal of the distance matrix should be recorded
#' @details \code{textstat_simil} options are: \code{"correlation"} (default),
#' \code{"cosine"}, \code{"jaccard"}, \code{"eJaccard"},
#' \code{"dice"}, \code{"eDice"}, \code{"simple matching"}, \code{"hamann"}, and
#' \code{"faith"}.
#' @note If you want to compute similarity on a "normalized" dfm object 
#'   (controlling for variable document lengths, for methods such as correlation
#'   for which different document lengths matter), then wrap the input dfm in 
#'   \code{\link{weight}(x, "relFreq")}.
#' @export
#' @seealso \code{\link{textstat_dist}}, \code{\link{as.list.dist}}
#' @import methods
#' @examples
#' ## similarities
#' 
#' # compute some document similarities
#' (tmp <- textstat_simil(presDfm, margin = "documents"))
#' 
#' # output as a list
#' as.list(tmp)[1:2]
#' 
#' # for specific comparisons
#' textstat_simil(presDfm, "1985-Reagan", n = 5, margin = "documents")
#' textstat_simil(presDfm, c("2009-Obama" , "2013-Obama"), n = 5, margin = "documents")
#' textstat_simil(presDfm, c("2009-Obama" , "2013-Obama"), margin = "documents")
#' textstat_simil(presDfm, c("2009-Obama" , "2013-Obama"), margin = "documents", method = "cosine")
#' 
#' # compute some term similarities
#' textstat_simil(presDfm, c("fair", "health", "terror"), method = "cosine", 
#'                margin = "features", 20)
#' 
textstat_simil <- function(x, selection = character(0), n = NULL,
                          margin = c("documents", "features"),
                          method = "correlation", 
                          upper  = FALSE, diag = FALSE) {
    if (!is.dfm(x))
        stop("x must be a dfm object")

    # value <- match.arg(value)
    
    margin <- match.arg(margin)
    if (margin == "features") {
        items <- featnames(x)
        xsize <- dim(x)[2]
    } else {
        items <- docnames(x)
        xsize <- dim(x)[1]
    }
    
    if (is.null(n) || n >= xsize)
        n <- xsize # choose all features/docs if n is NULL
    
    if (length(selection) != 0L) {
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
    
    vecMethod <- c("cosine", "correlation", "jaccard", "eJaccard", "dice", "eDice", "simple matching", "hamann", "faith")
    if (method %in% vecMethod) {
        if (method == "simple matching") method <- "smc"
        result <- get(paste(method,"Sparse", sep = ""))(x, xSelect, margin = ifelse(margin == "documents", 1, 2))
    } else {
        stop("The metric is not currently supported by quanteda, please use other packages such as proxy::dist()/simil().")
        #result <- as.matrix(proxy::dist(as.matrix(x), as.matrix(xSelect), method = method,
        #                                by_rows = ifelse(margin=="features", FALSE, TRUE)), diag = 1)
    }
    
    # convert NaNs to NA
    # similmatrix[is.nan(similmatrix)] <- NA
    
    # create a full square matrix if result is calculated only for selected features
    if (length(selection) != 0L) {
        # adjust the order of the rows to put the selected features as the top rows
        rname <- rownames(result)
        cname <- colnames(result)
        rname <- c(cname, rname[!rname %in% cname])
        result <- result[rname,]
        
        # create a full square matrix 
        nn <- if(length(selection) == 1L) length(result) else nrow(result)
        rname <- if(length(selection) == 1L) names(result) else rownames(result)
        x <- Matrix::Matrix(data = 0,nrow = nn,ncol = nn, dimnames = list(rname, rname))
        if(length(selection) == 1L){
            x[, 1] <- result
        } else {
            x[, 1:ncol(result)] <- result
        }
        result <- x
    }
    
    # truncate to n if n is not NULL
    if (!is.null(n))
        result <- result[1:n,]
        
    # create a new dist object
    distM <- stats::as.dist(result, diag = diag, upper = upper)
    attr(distM, "method") <- method
    attr(distM, "call") <- match.call()
    # This will call Stats::print.dist() and Stats::as.matrix.dist()
    distM
}


## code below based on assoc.R from the qlcMatrix package
## used Matrix::crossprod and Matrix::tcrossprod for sparse Matrix handling

# L2 norm
norm2 <- function(x,s) { drop(Matrix::crossprod(x^2, s)) ^ 0.5 }
# L1 norm
norm1 <- function(x,s) { drop(Matrix::crossprod(abs(x),s)) }

#cosine similarity: xy / sqrt(xx * yy)
cosineSparse <- function(x, y = NULL, margin = 1, norm = norm2) {
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
correlationSparse <- function(x, y = NULL, margin = 1) {
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
jaccardSparse <- function(x, y = NULL, margin = 1) {
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
eJaccardSparse <- function(x, y = NULL, margin = 1) {
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
# formula: eDice = 2|AB|/(|A| + |B|)
diceSparse <- function(x, y = NULL, margin = 1) {
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    # convert to binary matrix
    x <- tf(x, "boolean") 
    
    cpFun <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    marginSums <- if (margin == 2) colSums else rowSums
    marginNames <- if (margin == 2) colnames else rownames
    # union 
    an <- marginSums(x)
    if (!is.null(y)) {
        y <- tf(y, "boolean")
        A <- cpFun(x, y)
        bn <- marginSums(y)
        colNm <- marginNames(y)
    } else {
        A <- cpFun(x)
        bn <- an
        colNm <- marginNames(x)
    }
    rowNm <- marginNames(x)
    dicemat <- (2 * A)/(an + bn)
    dimnames(dicemat) <- list(rowNm,  colNm)
    dicemat
}

# eDice similarity coefficient, extend from binary Dice to real-valued data 
# formula: eDice = 2|AB|/(|A|^2 + |B|^2)
eDiceSparse <- function(x, y = NULL, margin = 1) {
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    cpFun <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    marginSums <- if (margin == 2) colSums else rowSums
    marginNames <- if (margin == 2) colnames else rownames
    # union 
    an <- marginSums(x^2)
    if (!is.null(y)) {
        A <- cpFun(x, y)
        bn <- marginSums(y^2)
        colNm <- marginNames(y)
    } else {
        A <- cpFun(x)
        bn <- an
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
    eDicemat <- (2 * A)/(an + bn)
    dimnames(eDicemat) <- list(rowNm,  colNm)
    eDicemat
}

# simple matching coefficient(SMC) 
# formula: SMC = (M00+M11)/(M00+M11+M01+M10)
smcSparse <- function(x, y = NULL, margin = 1) {
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
hamannSparse <- function(x, y = NULL, margin = 1) {
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
faithSparse <- function(x, y = NULL, margin = 1) {
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
