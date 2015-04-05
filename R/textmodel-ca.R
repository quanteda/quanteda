#' correspondence analysis of a document-feature matrix
#' 
#' \code{textmodel_ca} implements correspondence analysis scaling on a
#' \link{dfm}.  Currently the method is a wrapper to \link[ca]{ca.matrix} in the \pkg{ca} package.
#' @param data the dfm on which the model will be fit
#' @param smooth a smoothing parameter for word counts; defaults to zero.
#' @param ... additional arguments passed to \link[ca]{ca.matrix}
#' @author Kenneth Benoit
#' @examples 
#' ieDfm <- dfm(ie2010Corpus)
#' wca <- textmodel_ca(ieDfm)
#' summary(wca) 
#' @import ca
#' @export
textmodel_ca <- function(data, smooth=0, ...) {
    if (!is(data, "dfm"))
        stop("supplied data must be a dfm object.")
    data <- data + smooth  # smooth by the specified amount
    model <- ca::ca(as.matrix(data), ...)
    class(model) <- c("fittedtextmodel", "ca", "list")
    return(model)
}



