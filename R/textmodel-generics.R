#' fit a text model
#'
#' Fit a text model to a dfm.
#' @param ... additional arguments to be passed to specific model types
#' @seealso \code{\link{textmodel_wordscores}}, \code{\link{textmodel_NB}},
#' \code{\link{textmodel_wordfish}}, \code{\link{textmodel_lda}}, 
#' \code{\link{textmodel}}
#' @section Class hierarchy:
#' Here will go the description of the class hierarchy that governs dispatch for the 
#' predict, print, summary methods, since this is not terribly obvious. 
#' (Blame it on the S3 system.)
#' @export
#' @examples
#' require(quantedaData)
#' data(ie2010Corpus)
#' ieDfm <- dfm(ie2010Corpus)
#' refscores <- c(rep(NA, 4), -1, 1, rep(NA, 8))
#' ws <- textmodel(ieDfm, refscores, model="wordscores", smooth=1)
#' bs <- textmodel(as.dfm(ieDfm[5:6,]), refscores[5:6], model="wordscores", scale="logit", smooth=1)
#' plot(ws$pi, bs$pi, xlim=c(-1, 1), xlab="Linear word score", ylab="Logit word score")
#'
#' # prediction method for wordscores
#' predict(ws, ieDfm, rescaling="mv")
#' 
#' # wordfish
#' wf <- textmodel(ieDfm, model="wordfish", dir=c(2,1))
#' 
textmodel <- function(x, ...) {
    UseMethod("textmodel")
}

#' @rdname textmodel
#' @param x a quanteda \link{dfm} object containing feature counts by document
#' @param y for supervised models, a vector of class labels or values for 
#'   training the model, with \code{NA} for documents to be excluded from the 
#'   training set; for unsupervised models, this will be left NULL
#' @param model the model type to be fit.  Currently implemented methods are: 
#'   \describe{ \item{\code{wordscores}}{Fits the "wordscores" model of Laver, 
#'   Benoit, and Garry (2003). Options include the original linear scale of LBG 
#'   or the logit scale proposed by Beauchamps (2001).  See 
#'   \link{textmodel_wordscores}.}
#'   
#'   \item{\code{NB}}{Fits a Naive Bayes model to the dfm, with options for 
#'   smoothing, setting class priors, and a choice of multinomial or binomial 
#'   probabilities.}}
#' @return a \code{textmodel} class list, containing the fitted model and 
#'   additional information specific to the model class.  See the methods for 
#'   specific models, e.g. \link{textmodel_wordscores}, \link{textmodel_NB}, 
#'   etc.
#' @export
textmodel.dfm <- function(x, y=NULL, model=c("wordscores", "NB", "wordfish", "lda"), ...) {
    model <- match.arg(model)
    call <- match.call()
    Yname <- deparse(substitute(y))
    
    if (model=="wordscores") {
        if (nrow(x) != length(y))
            stop("x and y contain different numbers of documents.")
        
        result <- textmodel_wordscores(x, y, ...)
    } else if (model=="NB") {
        if (nrow(x) != length(y))
            stop("x and y contain different numbers of documents.")
        result <- textmodel_NB(x, y, ...)
    } else if (model=="wordfish") {
        if (!is.null(y))
            warning("y values not used with wordfish model. ")
        result <- textmodel_wordfish(x, ...)
    } else if (model=="lda") {
        if (!is.null(y))
            warning("y values not used with wordfish model. ")
        result <- textmodel_lda(x, ...)
    } else {
        stop(paste("model", method, "not implemented."))
    }
    result$call <- call
    class(result) <- c("textmodel", "textmodelfitted", class(result))
    result
}


#' Fit a dfm to a text model using formula notation
#' 
#' Provides an alternative syntax for fitting text models, using the ~ notation 
#' as would be used by lm or glm.
#' @rdname textmodel
#' @param formula An object of class \link{formula} of the form \code{class ~ x1
#'   + x2 + ...}.  (Interactions are not currently allowed for any of the models
#'   implemented.)  The \code{x} variable(s) is
#'   typically a \link{dfm}, and the y variable a vector of class labels or 
#'   training values associated with each document.
#' @param data dfm or data.frame from which to take the formula
#' @param method the model type to be fit
#' @author Paul Nulty
#' @export
textmodel.formula <- function(formula, data=NULL, model=c("wordscores", "NB"), ...) {
    model <- match.arg(model)
    call <- match.call()
    Yname <- as.character(formula[[2]])
    
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("x", "data"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf[[1]] <- as.name("model.frame")
    mf <- eval.parent(mf)
    mt <- attr(mf, "terms")
    x <- model.matrix(mt, mf)
    print(dim(x))
    y <- model.response(mf, "numeric")
#     z <- c()
#     if (method == 'wordscores') { 
#         z <- textModelWordscores(x, y, smooth = smooth, ...)
#     } else if (method == 'NB') {
#         z <- textModelNB(x, y, smooth = smooth, ...)
#     } else { 
#         stop("Only wordscores method is currently implemented.")  
#     }

    #z$model <- mf
    tm <- textmodel(as.dfm(x), y, model, ...)
    tm$call <- call
    tm$model <- mf
    tm
}


#' predict a text model on new data
#'
#' Apply a fitted text model to make predictions on test data.
#' @param object a fitted textmodel object (from \code{\link{textmodel}})
#' @param ... further arguments passed to or from other methods
#' @export
predict.textmodel <- function(object, ...) {
    result <- NextMethod(object, ...)
    class(result) <- c("textmodel", class(result))
    result
}


#' @rdname predict.textmodel
#' @param newdata A dfm or matrix object containing features found in the 
#'   training object.  If omitted, the original dfm on which the model was fit 
#'   will be used.
#' @export
predict.textmodelfitted <- function(object, ...) {
    result <- NextMethod(object, ...)
    class(result) <- c("textmodelpredicted", class(result))
    result
}



# print a textmodel object
# 
# print a class of textmodel object
# @param x textmodel object to be printed
# @param ... additional arguments passed to \code{print}
#' @export
#' @method print textmodel
print.textmodel <- function(x, ...) {
    NextMethod(x, ...)
}

# @rdname print.textmodel
#' @export
#' @method print textmodelfitted
print.textmodelfitted <- function(x, ...) {
    cat("Fitted textmodel of type:", class(x)[3], "\n")
    
    NextMethod(x, ...)
}

# @rdname print.textmodel
#' @export
#' @method print textmodelpredicted
print.textmodelpredicted <- function(x, ...) {
    cat("Predicted textmodel of type:", class(x)[3], "\n\n")
    #cat("Call:\n\t")
    #print(object$call)
    
    ## options if a wordscores object 
    ## (and no, it's not very object-oriented!)
    names(x)[which(names(x)=="textscore_raw")] <- "textscore"
    names(x)[which(names(x)=="textscore_raw_se")] <- "LBG se"
    names(x)[which(names(x)=="textscore_raw_lo")] <- "ci lo"
    names(x)[which(names(x)=="textscore_raw_hi")] <- "ci hi"
    names(x)[which(names(x)=="textscore_mv")] <- "MV rescaled"
    names(x)[which(names(x)=="textscore_lbg")] <- "LBG rescaled"
    names(x)[which(names(x)=="textscore_lbg_lo")] <- "LBG lo"
    names(x)[which(names(x)=="textscore_lbg_hi")] <- "LBG hi"
    # pare down the output if rescaling has been specified
    if (any(c("LBG rescaled", "MV rescaled") %in% names(x)))
        x$ci.lo <- x$ci.hi <- NULL
    
    if (class(x)[3]=="wordscores") {
        print(round(as.data.frame(x), 4))
        cat("\n")
    } else if (class(x)[3]=="wordfish") {
        NextMethod(x, ...)
    } else {
        cat("Document-level quantitites:\n")
        print(data.frame(Prediction=x$docs$nb.predicted,
                         PostPrDiff=x$docs$posterior.diff))
    }
}


# summary of a textmodel object
# 
# Summarize the results of a fitted or predicted textmodel object.
# @param object textmodel object to be summarized
# @param ... additional arguments to \link{print}
#' @export
#' @method summary textmodel
summary.textmodel <- function(object, ...) {
    NextMethod(object)    
}

# @rdname summary.textmodel
#' @export
#' @method summary textmodelfitted
summary.textmodelfitted <- function(object, ...) {
    cat("Predicted textmodel of type:", class(x)[3], "\n\n")
    cat("Call:\n\t")
    print(object$call)
    
    NextMethod(object)
}

# @rdname summary.textmodel
#' @export
#' @method summary textmodelpredicted
summary.textmodelpredicted <- function(object, ...) {
    NextMethod(object)    
}

