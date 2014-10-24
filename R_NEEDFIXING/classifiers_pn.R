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
fitModel.formula <- function(formula, data, smooth=1, method=c("wordscores", "NB"), ...) {
    method <- match.arg(method)
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
    z <- c()
    if(method == 'wordscores'){z <- textModelWordscores(x, y, smooth = smooth, ...)
    }else if(method == 'NB') { z <- textModelNB(x, y, smooth = smooth, ...)
    }
    else {
        stop("Only wordscores method is currently implemented.")
    }
}

#' Naive Bayes text model
#'
#' \code{textmodel_NB} implements Naive Bayes model for class prediction on a
#' set of labelled texts.
#' @param x the dfm on which the model will be fit.  Does not need to contain
#'   only the training documents, since the index of these will be identified in
#'   \code{train}.
#' @param train integer index of documents, or character vector of document
#'   names.  For \code{scale="logit"}, only two documents can be specified
#' @param train.class vector of training labels associated with each document
#'   identified in \code{train}.  (These will be converted to factors if not
#'   already factors.)
#' @param smooth a smoothing parameter for word counts, default is 1.0.
#' @param distribution either \code{multinomial} to count all word occurrences,
#' or \code{bernoulli} to count feature occurrences as binary
#' @param classpriors an optional named vector of class priors, corresponding to
#'   the levels or values of \code{train.class}.  This does not need to be
#'   ordered, but must be named, and the names must match 1:1 to the levels of
#'   \code{train.class}.  Default is uniform class priors, not based on the
#'   balance of trainind documents, but rather uniform across the possible
#'   classes.  See details.
#' @section Class priors: Details on this will be provided soon.
#' @section Smoothing values: Smoothing values are added to features observed in
#'   each class, which consist of all observed features per class, not per
#'   document.  More soon.
#' @author Kenneth Benoit
#' @references Laver, Benoit and Garry (2003); Martin and Vanberg (2007)
#' @export
textmodelNB <- function(trainData, trainLabels, smooth=1,
                         distribution=c("multinomial", "bernoulli"),
                         classpriors=1/length(table(train.class))) {
    thecall <- match.call()
    distribution <- match.arg(distribution)

    warning("textmodel_NB not implented yet!")

    model <- list(featureScores = NULL, train=train, train.class=train.class)
    class(model) <- c("textmodel", "NB", class(model))
    return(model)
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
textModelWordscores <- function(refData, refScores, smooth=1) {

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

# wordscores
require(quantedaData)
data(ie2010Corpus)
ieDfm <- dfm(ie2010Corpus)
refData <- ieDfm[5:8,] # Cowen and Kenny
refScores <- c(-1,1,NA,NA)
mod <- fitModel(refScores ~ refData)


