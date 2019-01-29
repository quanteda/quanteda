#' Correspondence analysis of a document-feature matrix
#' 
#' \code{textmodel_ca} implements correspondence analysis scaling on a 
#' \link{dfm}.  The method is a fast/sparse version of function \link[ca]{ca}. 
#' @param x the dfm on which the model will be fit
#' @param smooth a smoothing parameter for word counts; defaults to zero.
#' @param nd  Number of dimensions to be included in output; if \code{NA} (the 
#'   default) then the maximum possible dimensions are included.
#' @param sparse retains the sparsity if set to \code{TRUE}; set it to 
#'   \code{TRUE} if \code{x} (the \link{dfm}) is too big to be allocated after
#'   converting to dense
#' @param residual_floor specifies the threshold for the residual matrix for 
#'   calculating the truncated svd.Larger value will reduce memory and time cost
#'   but might reduce accuracy; only applicable when \code{sparse = TRUE}

#' @author Kenneth Benoit and Haiyan Wang
#' @references Nenadic, O. & Greenacre, M. (2007). \href{http://www.jstatsoft.org/v20/i03/}{Correspondence Analysis in R, with Two- and Three-dimensional Graphics: 
#'   The ca package}. \emph{Journal of Statistical Software}, 20(3).
#'   
#' @details \link[RSpectra]{svds} in the \pkg{RSpectra} package is applied to 
#'   enable the fast computation of the SVD.
#' @note You may need to set \code{sparse = TRUE}) and
#'   increase the value of \code{residual_floor} to ignore less important
#'   information and hence to reduce the memory cost when you have a very big
#'   \link{dfm}.
#'   If your attempt to fit the model fails due to the matrix being too large, 
#'   this is probably because of the memory demands of computing the \eqn{V
#'   \times V} residual matrix.  To avoid this, consider increasing the value of
#'   \code{residual_floor} by 0.1, until the model can be fit.
#' @return \code{textmodel_ca()} returns a fitted CA textmodel that is a special
#' class of \pkg{ca} object.
#' @examples 
#' dfmat <- dfm(data_corpus_irishbudget2010)
#' tmod <- textmodel_ca(dfmat)
#' summary(tmod)
#' @seealso \code{\link{coef.textmodel_lsa}}, \link[ca]{ca}
#' @export
textmodel_ca <- function(x, smooth = 0, nd = NA, sparse = FALSE, 
                         residual_floor = 0.1) {
    UseMethod("textmodel_ca")
}

#' @export
textmodel_ca.default <- function(x, smooth = 0, nd = NA, sparse = FALSE, 
                                 residual_floor = 0.1) {
    stop(friendly_class_undefined_message(class(x), "textmodel_ca"))
}
    
#' @export
textmodel_ca.dfm <- function(x, smooth = 0, nd = NA, sparse = FALSE, 
                             residual_floor = 0.1) {
    
    x <- as.dfm(x)
    if (!sum(x)) stop(message_error("dfm_empty"))
    
    x <- x + smooth  # smooth by the specified amount
    
    I <- dim(x)[1] 
    J <- dim(x)[2]
    rn <- dimnames(x)[[1]]
    cn <- dimnames(x)[[2]]
    
    # default value of rank k
    if (is.na(nd)){
        #nd <- max(floor(min(I, J)/4), 1)  
        nd <- max(floor(3 * log(min(I, J))), 1) 
    } else {
        nd.max <- min(dim(x)) - 1
        if (nd > nd.max ) nd <- nd.max
    }
    nd0 <- nd
    
    n <- sum(x) 
    P <- x / n
    rm <- rowSums(P) 
    cm <- colSums(P)

    if (sparse == FALSE){
        # generally fast for a not-so-large dfm
        eP <- Matrix::tcrossprod(rm, cm)
        S  <- (P - eP) / sqrt(eP)
    } else {
        # keep the residual matrix sparse
        S <- as(qatd_cpp_ca(P, residual_floor / sqrt(n)), 'dgCMatrix')
    }
    
    dec <- RSpectra::svds(S, nd)   
    
    chimat <- S ^ 2 * n
    sv     <- dec$d[seq_len(nd)]
    u      <- dec$u
    v      <- dec$v
    ev     <- sv ^ 2
    cumev  <- cumsum(ev)
    
    # Inertia:
    totin <- sum(ev)
    rin <- rowSums(S ^ 2)
    cin <- colSums(S ^ 2)
    
    # chidist
    rachidist <- sqrt(rin / rm)
    cachidist <- sqrt(cin / cm)
    rchidist <- rachidist
    cchidist <- cachidist
    
    # Standard coordinates:
    phi <- as.matrix(u[,seq_len(nd)]) / sqrt(rm)
    rownames(phi) <- rn
    colnames(phi) <- paste("Dim", seq_len(ncol(phi)), sep="")
    
    gam <- as.matrix(v[,1:nd]) / sqrt(cm)
    rownames(gam) <- cn
    colnames(gam) <- paste("Dim", seq_len(ncol(gam)), sep="")
    # remove attributes
    attr(rm, "names") <- NULL
    attr(cm, "names") <- NULL
    attr(rchidist, "names") <- NULL
    attr(cchidist, "names") <- NULL
    attr(rin, "names") <- NULL
    attr(cin, "names") <- NULL
    
    #results
    ca_model <- 
        list(sv         = sv, 
             nd         = nd0,
             rownames   = rn, 
             rowmass    = rm, 
             rowdist    = rchidist,
             rowinertia = rin, 
             rowcoord   = phi, 
             rowsup     = logical(0), 
             colnames   = cn, 
             colmass    = cm, 
             coldist    = cchidist,
             colinertia = cin, 
             colcoord   = gam, 
             colsup     = logical(0),
             call       = match.call())
    class(ca_model) <- c("textmodel_ca", "ca", "list")
    return(ca_model)  
}

#' Extract model coefficients from a fitted textmodel_ca object
#' 
#' \code{coef()} extract model coefficients from a fitted \code{textmodel_ca}
#' object.  \code{coefficients()} is an alias.
#' @param object a fitted \link{textmodel_ca} object
#' @param doc_dim,feat_dim the document and feature dimension scores to be
#'   extracted
#' @param ... unused
#' @keywords textmodel internal
#' @export
coef.textmodel_ca <- function(object, doc_dim = 1, feat_dim = 1, ...) {
    list(coef_feature = object$colcoord[, feat_dim],
         coef_feature_se = rep(NA, length(object$colnames)),
         coef_document = object$rowcoord[, doc_dim],
         coef_document_se = rep(NA, length(object$rownames)))
}

#' @rdname coef.textmodel_ca
#' @export
coefficients.textmodel_ca <- function(object, doc_dim = 1, feat_dim = 1, ...) {
    UseMethod('coef')
}
