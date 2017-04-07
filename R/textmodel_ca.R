#' @rdname textmodel-internal
#' @keywords internal textmodel
#' @export
setClass("textmodel_ca_fitted",
         slots = c(smooth = "numeric", 
                   nd = "numeric",
                   sparse = "logical",
                   threads = "numeric",
                   residual_floor = "numeric"),
         contains = "textmodel_fitted")

#' correspondence analysis of a document-feature matrix
#' 
#' \code{textmodel-ca} implements correspondence analysis scaling on a
#' \link{dfm}.  The method is a fast/sparse version of function \link[ca]{ca} in 
#' the \pkg{ca} package.
#' @param x the dfm on which the model will be fit
#' @param smooth a smoothing parameter for word counts; defaults to zero.
#' @param nd  Number of dimensions to be included in output; if \code{NA} (the 
#'   default) then the maximum possible dimensions are included.
#' @param sparse retains the sparsity if set to TRUE
#' @param threads specifies the number of threads to be used; set to 1 to use a serial version of the function. 
#' Only applies when sparse = TRUE.
#' @param residual_floor specifies the threshold for the residual matrix for 
#'   calculating the truncated svd.Larger value will reduce memory and time cost but 
#'   might sacrify the accuracy. Only applies when sparse = TRUE

#' @author Kenneth Benoit and Haiyan Wang
#' @references Nenadic, O. and Greenacre, M. (2007). Correspondence analysis in R, with two- and three-dimensional graphics: 
#' The ca package. \emph{Journal of Statistical Software}, 20 (3), \url{http://www.jstatsoft.org/v20/i03/}
#' 
#' Erichson, N. Benjamin, et al.(2016) Randomized matrix decompositions using R. arXiv preprint arXiv:1608.02148 .
#' @details Randomized matrix decompostion (\pkg{rsvd}) is applied to enable the fast computation of the SVD. 
#' @note Setting \link{threads} larger than 1 (when sparse = TRUE) will trigger multiple threads computation, which retains sparsity of all involved 
#' matrices.It might not help the speed unless you have a very big \link{dfm}.   
#' @examples 
#' ieDfm <- dfm(data_corpus_irishbudget2010)
#' wca <- textmodel_ca(ieDfm)
#' summary(wca) 
#' @export
textmodel_ca <- function(x, smooth = 0, nd = NA,
                         sparse = FALSE,
                         threads = 1,
                         residual_floor = 0.1) {
    UseMethod("textmodel_ca")
}

#' @noRd
#' @export
textmodel_ca.dfm <- function(x, smooth = 0, nd = NA,
                             sparse = FALSE,
                             threads = 1,
                             residual_floor = 0.1) {

    x <- x + smooth  # smooth by the specified amount
    
    I <- dim(x)[1] 
    J <- dim(x)[2]
    rn <- dimnames(x)[[1]]
    cn <- dimnames(x)[[2]]
    
    # default value of rank k
    if (is.na(nd)){
        #nd <- max(floor(min(I, J)/4), 1)  
        nd <- max(floor(3*log(min(I, J))), 1) 
    } else {
        nd.max <- min(dim(x)) - 1
        if (nd > nd.max ) nd <- nd.max
    }
    nd0 <- nd
    
    # Init:
    n <- sum(x) 
    P <- x/n
    rm <- rowSums(P) 
    cm <- colSums(P)
    
    # SVD:
    if (sparse == FALSE){
        # generally fast for a not-so-large dfm
        eP <- Matrix::tcrossprod(rm, cm)
        S      <- (P - eP) / sqrt(eP)
    } else {
        # c++ function to keep the residual matrix sparse
        S <- cacpp(P, threads, residual_floor/sqrt(n))
    }
    
    dec <- rsvd::rsvd(S, nd)   
    chimat <- S^2 * n
    dec    <- RSpectra::svds(S, nd)
    sv     <- dec$d[1:nd]
    u      <- dec$u
    v      <- dec$v
    ev     <- sv^2
    cumev  <- cumsum(ev)
    
    # Inertia:
    totin <- sum(ev)
    rin <- rowSums(S^2)
    cin <- colSums(S^2)
    
    # chidist
    rachidist <- sqrt(rin / rm)
    cachidist <- sqrt(cin / cm)
    rchidist <- rachidist
    cchidist <- cachidist
    
    # Standard coordinates:
    phi <- as.matrix(u[,1:nd]) / sqrt(rm)
    rownames(phi) <- rn
    colnames(phi) <- paste("Dim",1:ncol(phi), sep="")
    
    gam <- as.matrix(v[,1:nd]) / sqrt(cm)
    rownames(gam) <- cn
    colnames(gam) <- paste("Dim",1:ncol(gam), sep="")
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
    class(ca_model) <- c("textmodel_ca_fitted", "ca", "list")
    return(ca_model)  
}

#' @rdname textmodel-internal
#' @param doc_dim,feat_dim the document and feature dimension scores to be 
#'   extracted as coefficients
#' @export
setMethod("coef", signature(object = "textmodel_ca_fitted"),
          function(object, doc_dim = 1, feat_dim = 1, ...) 
              list(coef_feature = object$colcoord[, feat_dim],
                   coef_feature_se = rep(NA, length(object$colnames)),
                   coef_document = object$rowcoord[, doc_dim],
                   coef_document_se = rep(NA, length(object$rownames)))
)

#' @rdname textmodel-internal
#' @export
setMethod("coefficients", signature(object = "textmodel_ca_fitted"),
          function(object, ...) coef(object, ...))
