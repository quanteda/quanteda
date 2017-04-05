#' correspondence analysis of a document-feature matrix
#' 
#' \code{textmodel_ca_sparse} implements correspondence analysis scaling on a
#' \link{dfm}.  The method is a sparse version of function \link[ca]{ca} in the \pkg{ca} package.
#' @param obj the dfm on which the model will be fit
#' @param smooth a smoothing parameter for word counts; defaults to zero.
#' @param nd  Number of dimensions to be included (default: NA)
#' @author Kenneth Benoit and Haiyan Wang
#' @references Nenadic, O. and Greenacre, M. (2007). Correspondence analysis in R, with two- and three-dimensional graphics: 
#' The ca package. \emph{Journal of Statistical Software}, 20 (3), \url{http://www.jstatsoft.org/v20/i03/}
#' @examples 
#' ieDfm <- dfm(data_corpus_irishbudget2010)
#' wca <- textmodel_ca_sparse(ieDfm)
#' summary(wca) 
#' @export
textmodel_ca_sparse <- function(obj, smooth = 0, nd = NA) {
    if (!is(obj, "dfm"))
        stop("supplied data must be a dfm object.")
    obj <- obj + smooth  # smooth by the specified amount
    
    I  <- dim(obj)[1] ; J <- dim(obj)[2]
    rn <- dimnames(obj)[[1]]
    cn <- dimnames(obj)[[2]]

    # default value of rank k
    if (is.na(nd)){
        nd <- max(floor(min(I, J)/4), 1)  
    } else {
        nd.max <- min(dim(obj)) - 1
        if (nd > nd.max ) nd <- nd.max
    }
    nd0 <- nd
    
    # Init:
    n <- sum(obj) ; P <- obj/n
    #rm <- apply(P, 1, sum) ; cm <- apply(P, 2, sum)
    rm <- rowSums(P) 
    cm <- colSums(P)
   
    # SVD:
    #eP     <- rm %*% t(cm)
    eP <- Matrix::tcrossprod(rm, cm)
    eN     <- eP * n
    S      <- (P - eP) / sqrt(eP)
    
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
    class(ca_model) <- c("fittedtextmodel", "ca", "list")
    return(ca_model)  
}

