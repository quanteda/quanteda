#' fit a text model
#'
#' Fit a text model to a dfm.
#' @name fitModel
#' @rdname fitModel
#' @param x a dfm object
#' @param ... additional parameters to be passed to other functions
#' @export
#' @examples
#' require(quantedaData)
#' data(ie2010Corpus)
#' ieDfm <- dfm(ie2010Corpus)
#' refDfm <- as.dfm(ieDfm[5:6,])
#' ws <- fitModel(refDfm, refScores=c(-1,1), smooth=1)
#' bs <- fitModel(refDfm, refScores=c(-1,1), scale="logit", smooth=1)
#' plot(ws$featureScores, bs$featureScores, xlim=c(-1, 1),
#'      xlab="Linear word score", ylab="Logit word score")
#'
#' # prediction method for wordscores
#' #predict(ws, ieDfm, rescaling="mv")
fitModel <- function(x, ...) {
    UseMethod("fitModel")
}

#' @name fitModel
#' @rdname fitModel
#' @param method the model type to be fit.  Currently implemented methods are:
#' \describe{
#'   \item{\code{wordscores}}{Fits the "wordscores" model of Laver, Benoit, and
#'   Garry (2003). Options include the original linear scale of LBG or the logit
#'   scale proposed by Beauchamps (2001).  See \link{textmodel_wordscores}.}
#'
#'   \item{\code{NB}}{Fits a Naive Bayes model to the dfm, with options for
#'   smoothing, setting class priors, and a choice of multinomial or binomial
#'   probabilities.}}
#' @param data a dfm object
#' @param refScores reference scores
#' @return a \code{textmodel} class list, containing the fitted model and
#'   additional information specific to the model class.  See the methods for
#'   specific models, e.g. \link{textmodel_wordscores}, \link{textmodel_NB},
#'   etc.
#' @references LBG (2003); Beauchamps (20XX)
#' @export
fitModel.dfm <- function(x, refScores, method=c("wordscores", "NB"), ...) {
    method <- match.arg(method)
    if (method=="wordscores") {
        return(textModelWordscores(x, refScores, ...))
    } else if (method=="NB") {
        return(textModelNB(x, y, ...))
    } else {
        stop(paste("Method", method, "not yet implemented."))
    }
}

#' Fit a dfm to a text model using formula notation
#'
#' Provides an alternative syntax for fitting text models, using the ~ notation
#' as would be used by lm or glm.
#' @rdname fitModel
#' @name fitModel
#' @param formula A formula using ~ notation, as in lm or glm models in R. In
#'   the case of text models, the x variable is the dfm, and the y variable a
#'   vector of target variables associated with each document
#' @param smooth A value to add to each cell in the dfm for smoothing
#' @param method the model type to be fit
#' @author Paul Nulty
#' @export
fitModel.formula <- function(x, smooth=1, method=c("wordscores", "NB"), ...) {
    method <- match.arg(method)
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("x", "data", "smooth"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf[[1]] <- as.name("model.frame")
    mf <- eval.parent(mf)
    mt <- attr(mf, "terms")
    x <- model.matrix(mt, mf)
    print(dim(x))
    y <- model.response(mf, "numeric")
    z <- c()
    if(method == 'wordscores') { z <- textModelWordscores(x, y, smooth = smooth, ...)
    } else if (method == 'NB') { z <- textModelNB(x, y, smooth = smooth, ...)
    } else { stop("Only wordscores method is currently implemented.")  }

    z$model <- mf
}

#' Wordscores text model
#'
#' \code{textmodel_wordscores} implements Laver, Benoit and Garry's (2003)
#' wordscores method for scaling of a single dimension
#' @param refData the dfm on which the model will be fit.  Does not need to contain
#'   only the training documents, since the index of these will be identified in
#'   \code{train}.
#' @param refScores vector of training scores associated with each document
#'   identified in \code{train}
#' @param smooth a smoothing parameter for word counts
#' @param scale classic LBG linear posterior weighted word class differences, or
#'   logit scale of log posterior differences
#' @name textModelWordScores
#' @author Kenneth Benoit
#' @export
textModelWordscores <- function(refData, refScores,
                             scale=c("linear", "logit"), smooth=0) {
    thecall <- match.call()
    scale <- match.arg(scale)
    if (length(refData) < 2)
        stop("wordscores model requires at least two training documents.")
    if (length(refData) != length(refScores))
    x <- refData          # select only the reference texts
    x <- x + smooth            # add one to all word counts
    Fwr <- tf(x)               # normalize words to term frequencies "Fwr"
    Pwr <- tf(t(Fwr))          # posterior word probability Pwr
    # compute likelihoods "Pwr" Pr(this word | document)
    if (scale=="linear") {
        Sw <- Pwr %*% refScores
        Sw <- Sw[,1]
    } else if (scale=="logit") {
        if (length(refScores) > 2)
            stop("For logit scale, only two training texts can be used.")
        if (sum(refScores) != 0) {
            warning("For logit scale, training scores are automatically rescaled to -1 and 1.")
            refScores <- rescaler(refScores)
        }
        lower <- 1
        upper <- 2
        if (refScores[1] > refScores[2]) { lower <- 2; upper <- 1 }
        Sw <- log(Pwr[, upper]) - log(Pwr[, lower])
    }
    model <- list(featureScores = Sw, refData=refData, refScores=refScores)
    class(model) <- c("textmodel", "wordscores", class(model))
    return(model)
}

#' Naive Bayes text model
#'
#' \code{textmodel_NB} implements Naive Bayes model for class prediction on a
#' set of labelled texts.
#' @param trainData the dfm on which the model will be fit.  Does not need to contain
#'   only the training documents, since the index of these will be identified in
#'   \code{train}.
#' @param trainLabels vector of training labels associated with each document
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
#' @name textmodelNB
#' @author Kenneth Benoit
#' @references Laver, Benoit and Garry (2003); Martin and Vanberg (2007)
#' @export
textmodelNB <- function(trainData, trainLabels, smooth=1,
                         distribution=c("multinomial", "bernoulli"),
                         classpriors=1/length(table(trainLabels))) {
    thecall <- match.call()
    distribution <- match.arg(distribution)
    labels <- levels(as.factor(trainLabels))
    trainData <- trainData + smooth
    Pwc <- rowSums(log(trainData))

    warning("textmodel_NB not implented yet!")

   # model <- list(featureScores = NULL, train=train, train.class=train.class)
#    class(model) <- c("textmodel", "NB", class(model))
    return(model)
}



#' predict a text model on a test set
#'
#' Apply a fitted text model to make predictions on test data.
#' @param object a fitted textmodel object (from \code{\link{fitmodel}})
#' @param ... further arguments passed to or from other methods
#' @export
predict.textmodel <- function(object, ...) {
    NextMethod(object, ...)
}

#' @rdname predict.textmodel
#' @param newdata A dfm or matrix object containing features found in the
#'   training object.  If omitted, the original dfm on which the model was fit
#'   will be used -- or it will eventually, since we have not yet implemented
#'   that functionality, which would require us to include the original data in
#'   the list defined by the \code{textmodel} class.
#' @param se.fit whether and how to compute standard errors -- not yet
#'   implemented and will be somewhat model-specific.
#' @param rescaling \code{none} for "raw" scores; \code{lbg} for LBG (2003)
#'   rescaling; or \code{mv} for the rescaling proposed by Martin and Vanberg
#'   (2007).  (Note to authors: Provide full details here in documentation.)
#' @references LBG (2003); Martin and Vanberg (2007)
#' @return \code{predict.wordscores} returns a data.frame whose rows are the
#'   documents fitted and whose columns contain the scored textvalues, with the
#'   number of columns depending on the options called (for instance, how many
#'   rescaled scores, and whether standard errors were requested.)  (Note: We
#'   may very well change this soon so that it is a list similar to other
#'   existing fitted objects.)
#' @export
predict.wordscores <- function(object, newdata, se.fit=FALSE,
                               rescaling = c("none", "lbg", "mv"), ...) {
    rescaling <- match.arg(rescaling)

    textscores <- data.frame(textscore_raw = tf(newdata) %*% object$featureScores)

    if (rescaling=="mv") {
        lowerIndex <- object$train[which(object$train.scores==min(object$train.scores))]
        upperIndex <- object$train[which(object$train.scores==max(object$train.scores))]
        textscores$textscores_mv <-
            (textscores$textscore_raw - textscores[lowerIndex, "textscore_raw"]) *
            (max(object$train.scores) - min(object$train.scores)) /
            (textscores[upperIndex, "textscore_raw"] - textscores[lowerIndex, "textscore_raw"]) +
            min(object$train.scores)
    }

    return(textscores)
}

####
#### ADDITIONAL DOCUMENTATION
####

#' @name textmodel
#' @docType data
#' @title \code{textmodel} class of fitted models
#' @description Here you can find mode documentation on the
#' implementation of the \code{textmodel} class of fitted models on \link{dfm} objects.
#' @seealso \code{\link{textModelWordscores}}, \code{\link{textModelNB}},
#' \code{\link{fitModel}}
NULL

####
#### HELPER FUNCTIONS
####

## rescale a vector so that the endpoints match scale.min, scale.max
rescaler <- function(x, scale.min=-1, scale.max=1) {
    scale.width <- scale.max - scale.min
    scale.factor <- scale.width / (max(x) - min(x))
    return((x-min(x)) * scale.factor - scale.max)
}

## make rows add up to one
rowNorm <- function(x) {
    x / outer(rowSums(x), rep(1, ncol(x)))
}

## make cols add up to one
colNorm <- function(x) {
    x / outer(rep(1, nrow(x)), colSums(x))
}

# #wordscores
# require(quantedaData)
# data(ie2010Corpus)
# ieDfm <- dfm(ie2010Corpus)
# refData <- ieDfm[5:8,] # Cowen and Kenny
# refScores <- c(-1,1,NA,NA)
# fitModel(ieDfm, refScores=c(-1,1), scale="logit")

# mod <- fitModel(refScores ~ refData, method="wordscores", scale="logit")
#
#
# # naive bayes
# require(quantedaData)
# data(ie2010Corpus)
# ieDfm <- dfm(ie2010Corpus)
# trainData <- ieDfm[5:6,] # Cowen and Kenny
# trainLabels <- c(-1,1)
# smooth<-1
# labels <- levels(as.factor(trainLabels))
# trainData <- trainData + smooth
# PwGc <- rowNorm(trainData)
# PcGw <- colNorm(PwGc * outer(1/length(trainLabels), rep(1, ncol(PwGc))))

#


