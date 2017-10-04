#' Latent Semantic Analysis 
#' 
#' \code{textmodel_lsa} implements Latent Semantic Analysis scaling on a matrix, which can be a dfm or a tfidf.  
#' @param x the matrix on which the model will be fit
#' @param nd  Number of dimensions to be included in output.
#' @author Haiyan Wang and Kenneth Benoit
#' @details \link[RSpectra]{svds} in the \pkg{RSpectra} package is applied to 
#'   enable the fast computation of the SVD.
#' @note  The number of dimensions \code{nd} retained in LSA is an empirical issue. While a reduction in k can remove much of the noise, keeping too few dimensions
#'  or factors may loose important information.
#' @references Barbara Rosario.  2000. "Latent Semantic Indexing: An overview". 
#' \emph{Techn. rep. INFOSYS 240 Spring Paper, University of California, Berkeley.} 
#' \url{http://www.sims.berkeley.edu/rosario/projects/LSI.pdf} 
#'   
#' @examples 
#' ieDfm <- dfm(data_corpus_irishbudget2010)
#' mylsa <- textmodel_lsa(ieDfm[1:10, ])
#' head(mylsa$docs)
#' 
#' newlsa <- transform_lsa(ieDfm[11:14, ], mylsa)
#' newlsa[, 1:10]
#' @export
textmodel_lsa <- function(x, nd = 10) {
    UseMethod("textmodel_lsa")
}

#' @rdname textmodel_lsa
#' @export
textmodel_lsa <- function(x, nd = 10) {

    if (nd > min(nrow(x), ncol(x))) nd <- min(nrow(x), ncol(x))
    if (nd < 2) nd <- 2
    #dec <- rsvd::rsvd(S, nd)   #rsvd is not as stable as RSpectra
    #dec <- irlba::irlba(S, nd)
    dec <- RSpectra::svds(x, nd)   
    
    if (any(dec$d <= sqrt(.Machine$double.eps))) {
        warning("[lsa] - there are singular values which are zero.");
    }
    
    space <- NULL
    space$dk <- dec$u
    space$tk <- dec$v
    space$sk <- dec$d
    space$docs <- dec$u
    space$features <- dec$v
    rownames(space$dk) = rownames(x)
    rownames(space$tk) = colnames(x)
    rownames(space$docs) = rownames(x)
    rownames(space$features) = colnames(x)
    # to be compatible with "lsa" package
    class(space) = "LSAspace"
    
    # return the LSA space
    return ( space )
}

#' @rdname textmodel_lsa
#' @param newX new matrix to be transformed into the lsa space
#' @param LSAspace previously fitted lsa space
#' @export
transform_lsa <- function( newX, LSAspace ) {
    tsa =  newX %*% LSAspace$tk %*% solve(diag(LSAspace$sk))
    transfed =  t( LSAspace$tk %*% diag(LSAspace$sk) %*% t(tsa) ) 
    
    colnames(transfed) = rownames(LSAspace$tk)
    rownames(transfed) = rownames(newX)
    
    return (transfed)
}