#' Similarity and distance computation between documents or features
#' 
#' These functions compute matrixes of distances and similarities between 
#' documents or features from a \code{\link{dfm}} and return a 
#' \code{\link[stats]{dist}} object (or a matrix if specific targets are
#' selected).  They are fast and robust because they operate directly on the sparse
#' \link{dfm} objects.
#' @param x a \link{dfm} object
#' @param selection character vector of document names or feature labels from
#'   \code{x}; or, a numeric vector or matrix which is conforms to \code{x}. A
#'   \code{"dist"} object is returned if selection is \code{NULL}, otherwise, a
#'   matrix is returned matching distances to the documents or features identified
#'   in the selection.
#' @param margin identifies the margin of the dfm on which similarity or 
#'   difference will be computed:  \code{"documents"} for documents or 
#'   \code{"features"} for word/term features.
#' @param method method the similarity or distance measure to be used; see
#'   Details
#' @param upper  whether the upper triangle of the symmetric \eqn{V \times V} 
#'   matrix is recorded
#' @param diag whether the diagonal of the distance matrix should be recorded
#' @details \code{textstat_simil} options are: \code{"correlation"} (default), 
#'   \code{"cosine"}, \code{"jaccard"}, \code{"ejaccard"}, \code{"dice"},
#'   \code{"edice"}, \code{"simple matching"}, \code{"hamann"}, and 
#'   \code{"faith"}.
#' @note If you want to compute similarity on a "normalized" dfm object 
#'   (controlling for variable document lengths, for methods such as correlation
#'   for which different document lengths matter), then wrap the input dfm in 
#'   \code{\link{dfm_weight}(x, "prop")}.
#' @return \code{textstat_simil} and \code{textstat_dist} return \code{dist} class objects.
#' @export
#' @seealso \code{\link{textstat_dist}}, \code{\link{as.list.dist}},
#'   \code{\link{dist}}
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
#' s2 <- textstat_simil(presDfm, c("fair", "health", "terror"), method = "cosine", 
#'                       margin = "features")
#' head(as.matrix(s2), 10)
#' as.list(s2, n = 8)
#' 
textstat_simil <- function(x, selection = NULL,
                           margin = c("documents", "features"),
                           method = "correlation", 
                           upper  = FALSE, diag = FALSE) {
    UseMethod("textstat_simil")
}
    

#' @export    
textstat_simil.default <- function(x, selection = NULL,
                               margin = c("documents", "features"),
                               method = "correlation", 
                               upper  = FALSE, diag = FALSE) {
    stop(friendly_class_undefined_message(class(x), "textstat_simil"))
}
    
#' @export    
textstat_simil.dfm <- function(x, selection = NULL,
                          margin = c("documents", "features"),
                          method = "correlation", 
                          upper  = FALSE, diag = FALSE) {
    x <- as.dfm(x)
    margin <- match.arg(margin)
    
    if (!is.null(selection)) {
        if (!is.character(selection)) {
            if (!is.dfm(selection)) selection_dfm <- as.dfm(as.matrix(selection))
            if (margin == "features") {
                if (ndoc(selection_dfm) != ndoc(x))
                    stop("The vector/matrix specified by 'selection' must be conform to the object x in rows.")
                y <- selection_dfm
            } else {
                if (nfeat(selection_dfm) != nfeat(x))
                    stop("The vector/matrix specified by 'selection' must be conform to the object x in columns.")
                y <- selection_dfm
            }
        } else {    
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
        }
    } else {
        y <- NULL
    }
    
    methods <- c("cosine", "correlation", "jaccard", "ejaccard", "dice", "edice", "simple matching", "hamann", "faith")
    
    if (method %in% methods) {
        if (method == "simple matching") method <- "smc"
        temp <- get(paste0(method, "_simil"))(x, y, margin = if (margin == "documents") 1 else 2)
    } else {
        stop(method, " is not implemented; consider trying proxy::simil().")
    }
    
    if (is.character(selection)) {
        name <- c(colnames(temp), setdiff(rownames(temp), colnames(temp)))
        # NOTE dense matrix does not accept "" as rowname
        temp <- temp[match(name, rownames(temp)), , drop = FALSE] # sort for as.dist()
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
        attr(result, "Size") <- if (margin == "documents") nrow(result) else ncol(result)
        attr(result, "method") <- method
        attr(result, "call") <- match.call()
        class(result) <- c("dist_selection")
        return(result)
    }
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

# Jaccard similarity (binary), See http://stackoverflow.com/questions/36220585/efficient-jaccard-similarity-documenttermmatrix
# formula: J = |AB|/(|A| + |B| - |AB|)
jaccard_simil <- function(x, y = NULL, margin = 1) {
    
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    # convert to binary matrix
    x <- dfm_weight(x, "boolean") 
    
    func_cp <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    func_sum <- if (margin == 2) colSums else rowSums
    func_name <- if (margin == 2) colnames else rownames
    n <- if (margin == 2) ncol(x) else nrow(x)
    # union 
    an <- func_sum(x)
    if (!is.null(y)) {
        y <- dfm_weight(y, "boolean")
        a <- func_cp(x, y)
        bn <- func_sum(y)
        colname <- func_name(y)
        # number of features
        kk <- y@Dim[margin]
    } else {
        a <- func_cp(x)
        bn <- an
        kk <- n
        colname <- func_name(x)
    }
    rowname <- func_name(x)
    tmp <- matrix(rep(an, kk), nrow = n)
    tmp <- tmp + matrix(rep(bn, n), nrow = n, byrow = TRUE)
    jacmat <- a / (tmp - a)
    dimnames(jacmat) <- list(rowname, colname)
    jacmat
}

# eJaccard similarity (real-valued data)
# formula: eJ = |AB|/(|A| ^ 2 + |B| ^ 2 - |AB|)
ejaccard_simil <- function(x, y = NULL, margin = 1) {
    
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    func_cp <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    func_sum <- if (margin == 2) colSums else rowSums
    func_name <- if (margin == 2) colnames else rownames
    n <- if (margin == 2) ncol(x) else nrow(x)
    # union 
    an <- func_sum(x ^ 2)
    if (!is.null(y)) {
        a <- func_cp(x, y)
        bn <- func_sum(y ^ 2)
        colname <- func_name(y)
        # number of features
        kk <- y@Dim[margin]
    } else {
        a <- func_cp(x)
        bn <- an
        kk <- n
        colname <- func_name(x)
    }
    rowname <- func_name(x)
    # common values
    tmp <- matrix(rep(an, kk), nrow = n)
    tmp <-  tmp + matrix(rep(bn, n), nrow = n, byrow=TRUE)
    ejacmat <- a / (tmp - a)
    dimnames(ejacmat) <- list(rowname, colname)
    ejacmat
}

# Dice similarity coefficient, binary
# formula: dice = 2|AB|/(|A| + |B|)
dice_simil <- function(x, y = NULL, margin = 1) {
    
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    # convert to binary matrix
    x <- dfm_weight(x, "boolean") 
    
    func_cp <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    func_sum <- if (margin == 2) colSums else rowSums
    func_name <- if (margin == 2) colnames else rownames
    n <- if (margin == 2) ncol(x) else nrow(x)
    # union 
    an <- func_sum(x)
    if (!is.null(y)) {
        y <- dfm_weight(y, "boolean")
        a <- func_cp(x, y)
        bn <- func_sum(y)
        kk <- y@Dim[margin]
        colname <- func_name(y)
    } else {
        a <- func_cp(x)
        bn <- an
        kk <- n
        colname <- func_name(x)
    }
    rowname <- func_name(x)
    tmp <- matrix(rep(an, kk), nrow = n)
    tmp <-  tmp +  matrix(rep(bn, n), nrow = n, byrow = TRUE)
    dicemat <- (2 * a) / tmp
    dimnames(dicemat) <- list(rowname, colname)
    dicemat
}

# eDice similarity coefficient, extend from binary Dice to real-valued data 
# formula: eDice = 2|AB|/(|A| ^ 2 + |B| ^ 2)
edice_simil <- function(x, y = NULL, margin = 1) {
    
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    func_cp <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    func_sum <- if (margin == 2) colSums else rowSums
    func_name <- if (margin == 2) colnames else rownames
    n <- if (margin == 2) ncol(x) else nrow(x)
    # union 
    an <- func_sum(x ^ 2)
    if (!is.null(y)) {
        a <- func_cp(x, y)
        bn <- func_sum(y ^ 2)
        colname <- func_name(y)
        kk <- y@Dim[margin]
    } else {
        a <- func_cp(x)
        bn <- an
        kk <- n
        colname <- func_name(x)
    }
    rowname <- func_name(x)
    tmp <- matrix(rep(an, kk), nrow = n)
    tmp <-  tmp +  matrix(rep(bn, n), nrow = n, byrow=TRUE)
    eDicemat <- (2 * a) / tmp
    dimnames(eDicemat) <- list(rowname, colname)
    eDicemat
}

# simple matching coefficient(SMC) 
# formula: SMC = (M00+M11)/(M00+M11+M01+M10)
smc_simil <- function(x, y = NULL, margin = 1) {
    
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    # convert to binary matrix
    x <- dfm_weight(x, "boolean") 
    x0 <- 1 - x
    func_cp <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    func_sum <- if (margin == 2) nrow else ncol
    func_name <- if (margin == 2) colnames else rownames
    # union 
    an <- func_sum(x)
    if (!is.null(y)) {
        y <- dfm_weight(y, "boolean")
        y0 <- 1 - y
        a <- func_cp(x, y)
        a0 <- func_cp(x0, y0)
        colname <- func_name(y)
    } else {
        a <- func_cp(x)
        a0 <- func_cp(x0)
        colname <- func_name(x)
    }
    rowname <- func_name(x)
    # common values
    a <- a + a0
    smcmat <- a / an
    dimnames(smcmat) <- list(rowname, colname)
    smcmat
}

# Hamann similarity: This measure gives the probability that a characteristic has the same state in both items 
# (present in both or absent from both) minus the probability that a characteristic has different states 
# in the two items (present in one and absent from the other).
# formula: Hamman = ((a+d)-(b+c))/n
# "Hamman" in proxy::dist
hamann_simil <- function(x, y = NULL, margin = 1) {
    
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    # convert to binary matrix
    x <- dfm_weight(x, "boolean") 
    x0 <- 1 - x
    func_cp <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    func_sum <- if (margin == 2) nrow else ncol
    func_name <- if (margin == 2) colnames else rownames
    # union 
    an <- func_sum(x)
    if (!is.null(y)) {
        y <- dfm_weight(y, "boolean")
        y0 <- 1 - y
        a <- func_cp(x, y)
        a0 <- func_cp(x0, y0)
        colname <- func_name(y)
    } else {
        a <- func_cp(x)
        a0 <- func_cp(x0)
        colname <- func_name(x)
    }
    rowname <- func_name(x)
    # common values
    hamnmat <- (2 * (a + a0) - an) / an
    dimnames(hamnmat) <- list(rowname, colname)
    hamnmat
}

# Faith similarity: This measure includes the
# negative match but only gave the half credits while giving
# the full credits for the positive matches.
# formula: Hamman = a+0.5d/n
faith_simil <- function(x, y = NULL, margin = 1) {
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    # convert to binary matrix
    x <- dfm_weight(x, "boolean") 
    x0 <- 1 - x
    func_cp <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    func_sum <- if (margin == 2) nrow else ncol
    func_name <- if (margin == 2) colnames else rownames
    # union 
    an <- func_sum(x)
    if (!is.null(y)) {
        y <- dfm_weight(y, "boolean")
        y0 <- 1 - y
        a <- func_cp(x, y)
        a0 <- func_cp(x0, y0)
        colname <- func_name(y)
    } else {
        a <- func_cp(x)
        a0 <- func_cp(x0)
        colname <- func_name(x)
    }
    rowname <- func_name(x)
    # common values
    faithmat <- (a + 0.5 * a0) / an
    dimnames(faithmat) <- list(rowname, colname)
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
    label <- attr(x, "Labels")
     if (is.null(label)) {
        dimnames(df) <- list(seq_len(size), seq_len(size))
    } else {
        dimnames(df) <- list(label, label)
    }
    diag(df) <- Diag
    df
}
