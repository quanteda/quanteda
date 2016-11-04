### IN THIS FILE, THE MAIN METHOD and OBJECTS are S4.
### This allows multiple dispatch, esp. for the S4 dfms, but also still
### works on the S3 old-style matrix-based dfm.  
###
### textmodel creates S4 objects now inheriting from textmodel_fitted-class
###
### the print, summary, predict methods are STILL S3 -- which is why they 
### do not dispatch properly unless print() (e.g.) is called specifically, 
### but this could be easily solved by writing appropriate methods in S4.


#' @rdname textmodel_fitted-class 
#' @export
setClass("textmodel_wordscores_fitted",
         slots = c(scale = "character", Sw = "numeric"),
         prototype = list(scale = "linear"),
         contains = "textmodel_fitted")

#' @rdname textmodel_fitted-class
#' @export
setClass("textmodel_wordscores_predicted",
         slots = c(newdata = "dfm", rescaling = "character", level = "numeric",
                   textscores = "data.frame"),
         prototype = list(newdata = NULL, rescaling = "none", level = 0.95),
         contains = "textmodel_wordscores_fitted")


#' Wordscores text model
#' 
#' \code{textmodel_wordscores} implements Laver, Benoit and Garry's (2003) 
#' wordscores method for scaling of a single dimension.  This can be called
#' directly, but the recommended method is through \code{\link{textmodel}}.
#' 
#' @param data the dfm on which the model will be fit.  Does not need to contain
#'   only the training documents, since the index of these will be matched
#'   automatically.
#' @param scores vector of training scores associated with each document 
#'   identified in \code{refData}
#' @param smooth a smoothing parameter for word counts; defaults to zero for the
#'   to match the LBG (2003) method.
#' @param scale classic LBG linear posterior weighted word class differences, or
#'   logit scale of log posterior differences
#' @details
#' Fitting a \code{textmodel_wordscores} results in an object of class 
#' \code{textmodel_wordscores_fitted} containing the
#' following slots:
#' @slot scale \code{linear} or \code{logit}, according to the value of \code{scale}
#' @slot Sw the scores computed for each word in the training set
#' @slot x  the dfm on which the wordscores model was called
#' @slot y  the reference scores
#' @slot call  the function call that fitted the model
#' @slot method takes a value of \code{wordscores} for this model
#' @author Kenneth Benoit
#' @examples 
#' (ws <- textmodel(LBGexample, c(seq(-1.5, 1.5, .75), NA), model="wordscores"))
#' predict(ws)
#' predict(ws, rescaling="mv")
#' predict(ws, rescaling="lbg")
#'
#' # same as:
#' (ws2 <- textmodel_wordscores(LBGexample, c(seq(-1.5, 1.5, .75), NA)))
#' predict(ws2)
#' @references Laver, Michael, Kenneth R Benoit, and John Garry. 2003. 
#' "Extracting Policy Positions From Political Texts Using Words as Data." 
#' American Political Science Review 97(02): 311-31
#' 
#' Beauchamp, N. 2012. "Using Text to Scale Legislatures with Uninformative
#' Voting." New York University Mimeo.
#' 
#' Martin, L W, and G Vanberg. 2007. "A
#' Robust Transformation Procedure for Interpreting Political Text." Political 
#' Analysis 16(1): 93-100.
#' @export
textmodel_wordscores <- function(data, scores,
                                 scale=c("linear", "logit"), smooth=0) {
    scale <- match.arg(scale)
    
    if (nrow(data) < 2)
        stop("wordscores model requires at least two training documents.")
    if (nrow(data) != length(scores))
        stop("trainingdata and scores vector must refer to same number of documents.")
    
    inRefSet <- which(!is.na(scores))
    if (!is.numeric(scores[inRefSet]))
        stop("wordscores model requires numeric scores.")
    
    setscores <- scores[inRefSet] # only non-NA reference texts
    if (smooth) data <- data + 1 # smooth if not 0
    x <- data[inRefSet, ]         # select only the reference texts
    
    Fwr <- tf(x, "prop")          # normalize words to term frequencies "Fwr"
    tFwr <- t(Fwr)
    Pwr <- tFwr / rowSums(tFwr)    # posterior word probability Pwr
    # compute likelihoods "Pwr" Pr(this word | document)
    if (scale=="linear") {
        Sw <- Pwr %*% setscores
        Sw <- Sw[,1]
    } else if (scale=="logit") {
        if (length(setscores) > 2)
            stop("\nFor logit scale, only two training texts can be used.")
        if (!identical(c(-1,1), sort(setscores))) {
            warning("\nFor logit scale, training scores are automatically rescaled to -1 and 1.")
            scores <- rescaler(setscores)
        }
        lower <- 1
        upper <- 2
        if (setscores[1] > setscores[2]) { lower <- 2; upper <- 1 }
        Sw <- log(Pwr[, upper]) - log(Pwr[, lower])
    }
    
    namesTemp <- names(Sw)
    Sw <- as.vector(Sw[which(colSums(x) > 0)])  # remove words with zero counts in ref set
    names(Sw) <- namesTemp[which(colSums(x) > 0)]
    
    new("textmodel_wordscores_fitted", Sw=Sw, x=data, y=scores, 
        method="wordscores", scale=scale, call = match.call())
}


#' @rdname textmodel_wordscores
#' @param object a fitted Wordscores textmodel
#' @param level probability level for confidence interval width
#' @param rescaling \code{none} for "raw" scores; \code{lbg} for LBG (2003) 
#'   rescaling; or \code{mv} for the rescaling proposed by Martin and Vanberg 
#'   (2007).  (Note to authors: Provide full details here in documentation.)
#' @param newdata dfm on which prediction should be made
#' @param ... additional arguments passed to other functions
#' @param verbose If \code{TRUE}, output status messages
#' @references Laver, Michael, Kenneth R Benoit, and John Garry. 2003. 
#' "Extracting Policy Positions From Political Texts Using Words as Data." 
#' \emph{American Political Science Review} 97(02): 311-31.
#' 
#' Martin, L W, and G Vanberg. 2007. "A Robust Transformation Procedure for
#' Interpreting Political Text." \emph{Political Analysis} 16(1): 93-100.
#' @return The \code{predict} method for a wordscores fitted object returns a 
#'   data.frame whose rows are the documents fitted and whose columns contain 
#'   the scored textvalues, with the number of columns depending on the options 
#'   called (for instance, how many rescaled scores, and whether standard errors
#'   were requested.)  (Note: We may very well change this soon so that it is a 
#'   list similar to other existing fitted objects.)
#' @export
#' @importFrom stats qnorm median sd
predict.textmodel_wordscores_fitted <- function(object, newdata=NULL, rescaling = "none", 
                               level=0.95, verbose=TRUE, ...) {    
    if (length(list(...))>0) 
        stop("Arguments:", names(list(...)), "not supported.\n")
    rescaling <- match.arg(rescaling, c("none", "lbg", "mv"), several.ok=TRUE)
    
    if (!is.null(newdata))
        data <- newdata
    else {
        data <- object@x
        newdata <- data
    }
    
    featureIndex <- match(names(object@Sw), features(data))
    
    scorable <- which(colnames(data) %in% names(object@Sw))
    Sw <- object@Sw[features(data)[scorable]]
    if (verbose)
        catm(paste(length(scorable), " of ", nfeature(data), " features (",
                  round(100*length(scorable)/nfeature(data), 2),
                  "%) can be scored\n\n", sep=""))
    
    # compute text scores as weighted mean of word scores in "virgin" document
    #Fw <- tf(data)   # first compute relative term weights
    #scorable.newd <- Fw[, featureIndex]  # then exclude any features not found/scored
    scorable.newd <- data[, scorable]
    ## NOTE: This is different from computing term weights on only the scorable words
    textscore_raw <- as.matrix(tf(scorable.newd, "prop") %*% Sw)
    
    textscore_raw_se <- rep(NA, length(textscore_raw))
    Fwv <- tf(scorable.newd, "prop")
    for (i in 1:length(textscore_raw_se))
        textscore_raw_se[i] <- sqrt(sum(Fwv[i, , drop=FALSE] * (textscore_raw[i] - Sw)^2)) / sqrt(rowSums(scorable.newd)[i])
    
    z <- stats::qnorm(1 - (1-level)/2)
    
    result <- data.frame(textscore_raw,
                         textscore_raw_se,
                         textscore_raw_lo = textscore_raw - z * textscore_raw_se,
                         textscore_raw_hi = textscore_raw + z * textscore_raw_se)
    
    if ("mv" %in% rescaling) {
        if (sum(!is.na(object@y)) > 2)
            warning("\nMore than two reference scores found with MV rescaling; using only min, max values.")
        lowerIndex <- which(object@y==min(object@y, na.rm=TRUE))
        upperIndex <- which(object@y==max(object@y, na.rm=TRUE))
        textscore_mv <-
            (textscore_raw - textscore_raw[lowerIndex]) *
            (max(object@y, na.rm=TRUE) - min(object@y, na.rm=TRUE)) /
            (textscore_raw[upperIndex] - textscore_raw[lowerIndex]) +
            min(object@y, na.rm=TRUE)
        result$textscore_mv <- textscore_mv
    } 
    
    if ("lbg" %in% rescaling) {
        SDr <- stats::sd(object@y, na.rm=TRUE)
        Sv <- mean(textscore_raw, na.rm=TRUE)
        SDv <- ifelse(length(textscore_raw)<2, 0, stats::sd(textscore_raw))
        mult <- ifelse(SDv==0, 0, SDr/SDv)
        textscore_lbg <- (textscore_raw - Sv) * mult + Sv
        # borrowed the next few lines from https://github.com/conjugateprior/austin
        if (mult == 0) {
            textscore_lbg_lo <- textscore_raw + z * textscore_raw_se
            textscore_lbg_hi <- textscore_raw + z * textscore_raw_se
        } else {
            textscore_lbg_lo <- (result$textscore_raw_lo - Sv) * mult + Sv
            textscore_lbg_hi <- (result$textscore_raw_hi - Sv) * mult + Sv
        }
        result <- cbind(result, data.frame(textscore_lbg,
                                           textscore_lbg_lo,
                                           textscore_lbg_hi))
    }
    
    new("textmodel_wordscores_predicted", rescaling = rescaling,
        newdata = newdata, textscores = result)
}


## rescale a vector so that the endpoints match scale.min, scale.max
rescaler <- function(x, scale.min=-1, scale.max=1) {
    scale.width <- scale.max - scale.min
    scale.factor <- scale.width / (max(x) - min(x))
    return((x-min(x)) * scale.factor - scale.max)
}


#' @rdname textmodel_wordscores
#' @param x for print method, the object to be printed
#' @param n max rows of dfm to print
#' @param digits number of decimal places to print for print methods
# @param ... not used in \code{print.textmodel_wordscores_fitted}
#' @export
#' @method print textmodel_wordscores_fitted
print.textmodel_wordscores_fitted <- function(x, n=30L, digits=2, ...) {
    cat("Fitted wordscores model:\n")
    cat("Call:\n\t")
    print(x@call)
    cat("\nReference documents and reference scores:\n\n")
    refscores <- data.frame(Documents=docnames(x@x),
                            "Ref scores" = x@y)
    refscores$Ref.scores <- format(refscores$Ref.scores, digits=digits)
    refscores$Ref.scores[grep("NA", refscores$Ref.scores)] <- "."
    names(refscores)[2] <- "Ref scores"
    print(refscores, row.names=FALSE, digits=digits)
    cat("\nWord scores: ")
    if (length(x@Sw) > n)
        cat("showing first", n, "scored features")
    cat("\n\n")
    print(head(x@Sw, n), digits=digits)
}

#' @rdname textmodel_wordscores
#' @export
setMethod("show", signature(object = "textmodel_wordscores_fitted"), function(object) print(object))

#' @rdname textmodel_wordscores
#' @export
setMethod("show", signature(object = "textmodel_wordscores_predicted"), function(object) print(object))


#' @export
#' @method summary textmodel_wordscores_fitted
summary.textmodel_wordscores_fitted <- function(object, ...) {
    cat("Call:\n\t")
    print(object@call)
    
    cat("\nReference Document Statistics:\n")
    cat("(ref scores and feature count statistics)\n\n")
    dd <- data.frame(Score=object@y,
                     Total=apply(object@x, 1, sum),
                     Min=apply(object@x, 1, min),
                     Max=apply(object@x, 1, max),
                     Mean=apply(object@x, 1, mean),
                     Median=apply(object@x, 1, stats::median))
    rownames(dd) <- docnames(object@x)
    print(dd, ...)
    invisible(dd)
}

#' @export
#' @method summary textmodel_wordscores_predicted
summary.textmodel_wordscores_predicted <- function(object, ...) {
    print(object)
}


#' @rdname textmodel_wordscores
#' @export
#' @method print textmodel_wordscores_predicted
print.textmodel_wordscores_predicted <- function(x, ...) {
    cat("Predicted textmodel of type: wordscores\n\n")
    #cat("Call:\n\t")
    #print(object$call)
    
    x <- x@textscores
    
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
    
    print(round(as.data.frame(x), 4))
    cat("\n")
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

