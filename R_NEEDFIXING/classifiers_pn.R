#' fit a word model
#'
#' Fit a word model to a dfm.
#' @param x a dfm object
#' @param ... additional parameters to be passed to other functions
#' @export
fitModel <- function(x, ...) {
    UseMethod("fitModel")
}

#' @rdname fitmodel
#' @param method the model type to be fit
#' @export
fitModel.dfm <- function(x, method=c("wordscores", "NB"), ...) {
    method <- match.arg(method)
    if (method=="wordscores") {
        return(wordscores_model(x, ...))
    } else {
        stop("Only wordscores method is currently implemented.")
    }
}






#' Wordscores model
#'
#' \code{wordscores_model} implements Laver, Benoit and Garry's (2003)
#' wordscores method for scaling of a single dimension
#' @param trainData a dfm representing the reference (training) documents
#' @param trainLabels a list of scalar values representing the `true` position of the training documents
#' @author Paul Nulty
#' @references Laver, Benoit and Garry (2003)
#' @export
wordscores.fit <- function(refData, refScores, smooth=1) {

    #   if (nrow(refData) != length(refScores))
    #       stop("number of training documents differs from trainLabels length.")
    #    naInd <- which(is.na(refScores))
    # if(length(naInd)>0){
    #     message(sprintf('ignoring %i observations with NA reference scores', length(naInd)))
    #  }
    if (length(refData) < 2)
        stop("wordscores model requires at least two documents with reference scores.")
    #  refData <- refData[-naInd,]
    # refScores <- refScores[-naInd]

    # add-one smoothing
    refData <- refData + smooth
    Fwr <- refData/rowSums(refData)
    Pwr <- Fwr/colSums(Fwr)
    Sw <- colSums(Pwr * (refScores))
    model <- c(wordScores=Sw)
    class(model) <- c("textmodel", "wordscores", class(model))
    return(model)
}

wordscores <- function(formula, data, smooth=1, ...){
    cl <- match.call()

    ## keep only the arguments which should go into the model frame
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "smooth"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf[[1]] <- as.name("model.frame")
    mf <- eval.parent(mf)

    ## 1) allow model.frame to update the terms object before saving it.
    mt <- attr(mf, "terms")

    x <- model.matrix(mt, mf)
    print(dim(x))
    y <- model.response(mf, "numeric")
    z <- wordscores.fit(x, y, smooth = smooth, ...)


}


# wordscores
require(quantedaData)
data(ie2010Corpus)
ieDfm <- dfm(ie2010Corpus)
refData <- ieDfm[5:8,] # Cowen and Kenny
refScores <- c(-1,1,NA,NA)
mod <- wordscores(refScores ~ refData)


