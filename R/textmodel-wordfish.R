#' Wordscores text model
#' 
#' 
#' \code{textmodel_wordfish} implements Laver, Benoit and Garry's (2003) 
#' wordscores method for scaling of a single dimension.  This can be called
#' directly, but the recommended method is through \code{\link{textmodel}}.
#' @importFrom Rcpp evalCpp
#' @useDynLib quanteda
#' @param wfm the dfm on which the model will be fit.  Does not need to contain
#'   only the training documents, since the index of these will be matched
#'   automatically.
#' @param priors vector of training scores associated with each document 
#'   identified in \code{refData}
#' @param tolerances a smoothing parameter for word counts; defaults to zero for the
#'   to match the LBG (2003) method.
#' @export
textmodel_wordfish <- function(wfm, priors, tolerances) {
	
	return(wordfishcpp(wfm, priors, tolerances))
    
}


