#' @rdname textmodel-internal
#' @keywords internal textmodel
#' @export
setClass("textmodel_lsa_fitted",
         slots = c(nd = "numeric"),
         contains = "textmodel_fitted")
#' Latent Semantic Analysis 
#' 
#' \code{textmodel_lsa} implements Latent Semantic Analysis scaling on a 
#' \link{dfm}.  
#' @param x the dfm on which the model will be fit
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
#' mylsa <- textmodel_lsa(ieDfm)
#' @export
textmodel_lsa <- function(x, nd = 10) {
    UseMethod("textmodel_lsa")
}

#' @noRd
#' @export
textmodel_lsa.dfm <- function(x, nd = 10) {
    #tfidf
    x <- tfidf(x)
    
    if (nd > min(nrow(x), ncol(x))) nd <- min(nrow(x), ncol(x))
    if (nd < 2) nd <- 2
    #dec <- rsvd::rsvd(S, nd)   #rsvd is not as stable as RSpectra
    #dec <- irlba::irlba(S, nd)
    dec <- RSpectra::svds(x, nd)   
    
    if (any(dec$d <= sqrt(.Machine$double.eps))) {
        warning("[lsa] - there are singular values which are zero.");
    }
    
    docs <- dec$u %*% diag(dec$d) %*% t(dec$v)
    rownames(docs) <- rownames(x)
    colnames(docs) <- colnames(x)
    class(docs) <- "textmodel_lsa_fitted"
    
    #results
    return(docs)  
}
