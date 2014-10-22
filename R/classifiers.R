#' fit a text model
#' 
#' Fit a text model to a dfm.
#' @param x a dfm object
#' @param ... additional parameters to be passed to other functions
#' @export
#' @examples
#' require(quantedaData)
#' data(ie2010Corpus)
#' ieDfm <- dfm(ie2010Corpus)
#' ws <- fitmodel(ieDfm, train=c("2010_BUDGET_06_Enda_Kenny_FG", "2010_BUDGET_05_Brian_Cowen_FF"),
#'                train.scores=c(-1,1), smooth=1)
#' bs <- fitmodel(ieDfm, train=c("2010_BUDGET_06_Enda_Kenny_FG", "2010_BUDGET_05_Brian_Cowen_FF"),
#'                train.scores=c(-1,1), scale="logit", smooth=1)
#' plot(ws$featureScores, bs$featureScores, xlim=c(-1, 1),
#'      xlab="Linear word score", ylab="Logit word score")
#'      
#' # prediction method for wordscores
#' predict(ws, ieDfm, rescaling="mv")
fitmodel <- function(x, ...) {
    UseMethod("fitmodel")
}

#' @rdname fitmodel
#' @param method the model type to be fit.  Currently implemented methods are:
#' \describe{ 
#'   \item{\code{wordscores}}{Fits the "wordscores" model of Laver, Benoit, and 
#'   Garry (2003). Options include the original linear scale of LBG or the logit
#'   scale proposed by Beauchamps (2001).  See \link{textmodel_wordscores}.}
#'   
#'   \item{\code{NB}}{Fits a Naive Bayes model to the dfm, with options for
#'   smoothing, setting class priors, and a choice of multinomial or binomial
#'   probabilities.}}
#' @return a \code{textmodel} class list, containing the fitted model and
#'   additional information specific to the model class.  See the methods for
#'   specific models, e.g. \link{textmodel_wordscores}, \link{textmodel_NB},
#'   etc.
#'   
#' @references LBG (2003); Beauchamps (20XX)
#' @export
fitmodel.dfm <- function(x, method=c("wordscores", "NB"), ...) {
    method <- match.arg(method)
    if (method=="wordscores") {
        return(textmodel_wordscores(x, ...))
    } else if (method=="NB") {
        return(textmodel_NB(x, ...))
    } else {
        stop(paste("Method", method, "not yet implemented."))
    }
}

#' Wordscores text model
#' 
#' \code{textmodel_wordscores} implements Laver, Benoit and Garry's (2003) 
#' wordscores method for scaling of a single dimension
#' @param x the dfm on which the model will be fit.  Does not need to contain
#'   only the training documents, since the index of these will be identified in 
#'   \code{train}.
#' @param train integer index of documents, or character vector of document 
#'   names.  For \code{scale="logit"}, only two documents can be specified
#' @param train.scores vector of training scores associated with each document
#'   identified in \code{train}
#' @param smooth a smoothing parameter for word counts
#' @param scale classic LBG linear posterior weighted word class differences, or
#'   logit scale of log posterior differences
#' @author Kenneth Benoit
#' @references Laver, Benoit and Garry (2003); Martin and Vanberg (2007)
#' @export
textmodel_wordscores <- function(x, train, train.scores, 
                             scale=c("linear", "logit"), smooth=0) {
    thecall <- match.call()
    scale <- match.arg(scale)
    if (length(train) < 2)
        stop("wordscores model requires at least two training documents.")
    if (length(train) != length(train.scores))
        stop("number of training documents differs from train.scores length.")
    if (any(is.na(train.scores))) 
        stop("no train.scores can be NA.")

    x <- x[train, ]            # select only the reference texts
    x <- x + smooth            # add one to all word counts
    Fwr <- tf(x)               # normalize words to term frequencies "Fwr"
    Pwr <- tf(t(Fwr))          # posterior word probability Pwr
    
    # compute likelihoods "Pwr" Pr(this word | document)
    if (scale=="linear") {
        Sw <- Pwr %*% train.scores
        Sw <- Sw[,1]
    } else if (scale=="logit") {
        if (length(train) > 2)
            stop("For logit scale, only two training texts can be used.")
        if (sum(train.scores) != 0) {
            warning("For logit scale, training scores are automatically rescaled to -1 and 1.")
            train.scores <- rescaler(train.scores)
        }
        lower <- 1
        upper <- 2
        if (train.scores[1] > train.scores[2]) { lower <- 2; upper <- 1 }
        Sw <- log(Pwr[, upper]) - log(Pwr[, lower])
    }
    model <- list(featureScores = Sw, train=train, train.scores=train.scores)
    class(model) <- c("textmodel", "wordscores", class(model))
    return(model)
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
textmodel_NB <- function(x, train, train.class, smooth=1, 
                         distribution=c("multinomial", "bernoulli"),
                         classpriors=1/length(table(train.class))) {
    thecall <- match.call()
    distribution <- match.arg(distribution)
    
    warning("textmodel_NB not implented yet!")
    
    model <- list(featureScores = NULL, train=train, train.class=train.class)
    class(model) <- c("textmodel", "NB", class(model))
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
#' @seealso \code{\link{textmodel_wordscores}}, \code{\link{textmodel_NB}}, 
#' \code{\link{fitmodel}}
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



