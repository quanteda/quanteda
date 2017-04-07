#' correspondence analysis of a document-feature matrix
#' 
#' \code{textmodel-ca} implements correspondence analysis scaling on a
#' \link{dfm}.  The method is a fast/sparse version of function \link[ca]{ca} in the \pkg{ca} package.
#' @param obj the dfm on which the model will be fit
#' @param smooth a smoothing parameter for word counts; defaults to zero.
#' @param nd  Number of dimensions to be included (default: NA)
#' @param method specified the truncated svd function; set to choose between \pkg{rsvd}(default) and \pkg{RSpectra}
#' @param mt set it to TRUE for a very large dfm to avoid the coercing of the dfm to dense 
#' @param threads specifies the number of threads to use; set to 1 to override
#'   the package settings and use a serial version of the function
#' @param residual_floor specifies the threshold for residual matrix for 
#'   calculating the truncated svd (default: 0.5)

#' @author Kenneth Benoit and Haiyan Wang
#' @references Nenadic, O. and Greenacre, M. (2007). Correspondence analysis in R, with two- and three-dimensional graphics: 
#' The ca package. \emph{Journal of Statistical Software}, 20 (3), \url{http://www.jstatsoft.org/v20/i03/}
#' 
#' Erichson, N. Benjamin, et al.(2016) Randomized matrix decompositions using R. arXiv preprint arXiv:1608.02148 .
#' @examples 
#' ieDfm <- dfm(data_corpus_irishbudget2010)
#' wca <- textmodel_ca(ieDfm)
#' summary(wca) 
#' @export
textmodel_ca <- function(obj, smooth = 0, nd = NA, 
                                method = c("rsvd", "RSpectra"),
                                mt = FALSE,
                                threads = quanteda_options("threads"),
                                residual_floor = 0.5) {
    if (!is(obj, "dfm"))
        stop("supplied data must be a dfm object.")
    obj <- obj + smooth  # smooth by the specified amount
    method <- match.arg(method)
    
    I  <- dim(obj)[1] ; J <- dim(obj)[2]
    rn <- dimnames(obj)[[1]]
    cn <- dimnames(obj)[[2]]

    # default value of rank k
    if (is.na(nd)){
        #nd <- max(floor(min(I, J)/4), 1)  
        nd <- max(floor(3*log(min(I, J))), 1) 
    } else {
        nd.max <- min(dim(obj)) - 1
        if (nd > nd.max ) nd <- nd.max
    }
    nd0 <- nd
    
    # Init:
    n <- sum(obj) ; P <- obj/n
    rm <- rowSums(P) 
    cm <- colSums(P)
   
    # SVD:
    if (mt == TRUE){
        # c++ function to keep the residual matrix sparse
        S <- cacpp(P, threads, residual_floor/sqrt(n))
    } else {
        # generally fast for a not-so-large dfm
        eP <- Matrix::tcrossprod(rm, cm)
        S      <- (P - eP) / sqrt(eP)
    }
    
    
    
    if (method == "rsvd"){
        dec <- rsvd::rsvd(S, nd)   
    } else {
        dec <- RSpectra::svds(S, nd)
    }
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

