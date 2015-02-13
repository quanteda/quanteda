#' Wordscores text model
#' 
#' 
#' \code{textmodel_wordfish} implements Laver, Benoit and Garry's (2003) 
#' wordscores method for scaling of a single dimension.  This can be called
#' directly, but the recommended method is through \code{\link{textmodel}}.
#' @importFrom Rcpp evalCpp
#' @useDynLib quanteda
#' @param wfm the dfm on which the model will be fit.
#' @param dir 
#' @param priors 
#' @param tol 
#' @export
textmodel_wordfish <- function(wfm, dir=c(1, 10), control=list(priors = c(Inf,Inf,3,1), tol = c(1e-6,1e-8))) {
	
  return(wordfishcpp(wfm, as.integer(dir), 1/(control$priors^2), control$tol))
    
}


