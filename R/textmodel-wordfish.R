#' Poisson scaling text model
#' 
#' \code{textmodel_wordfish} implements Slapin and Proksch's (2008) Poisson 
#' scaling model, also known as "wordfish", for a single dimension. This can be 
#' called directly, but the recommended method is through 
#' \code{\link{textmodel}}.  Currently the method is a wrapper to 
#' \link[austin]{wordfish} by Will Lowe.
#' @param data the dfm on which the model will be fit.  Does not need to contain
#'   only the training documents, since the index of these will be matched 
#'   automatically.
#' @param smooth a smoothing parameter for word counts; defaults to zero for the
#'   to match the LBG (2003) method.
#' @param ... additional arguments passed to \link[austin]{wordfish}
#' @references Slapin, Jonathan B, and Sven-Oliver Proksch. 2008. "A Scaling 
#'   Model for Estimating Time-Series Party Positions From Texts." American 
#'   Journal of Political Science 52(3): 705-22.
#' @import austin
#' @author Kenneth Benoit
#' @examples 
#' library(quantedaData)
#' data(ie2010Corpus)
#' ieDfm <- dfm(ie2010Corpus)
#' wf <- textmodel_wordfish(ieDfm, dir=c(2,1))
#' summary(wf)
#' @export
textmodel_wordfish <- function(data, smooth=0, ...) {
    if (!is.dfm(data))
        stop("supplied data must be a dfm object.")
    data <- data + smooth  # smooth by the specified amount
    model <- austin::wordfish(as.wfm(data, word.margin=2), ...)
    # class(model) <- c("wordfish", "list")
    return(model)
}


# @rdname predict.textmodel
# @param ... additional arguments passed to \link[austin]{predict.wordfish}
# @references LBG (2003); Martin and Vanberg (2007)
# @return \code{predict.wordfish} is a wrapper to \link[austin]{predict.wordfish}, and has the same returns, 
# except that the object is also classes as "wordfish".
# @author Ken Benoit, wrapper around Will Lowe's \pkg{austin} code.
# @export
# predict.wordfish <- function(object, newdata=NULL, ...) {
#     
#     result <- austin::predict.wordfish(object, newdata, ...)
#     class(result) <- c("wordfish", "data.frame")
#     return(result)
# }


