

#' Wordscores text model
#' 
#' \code{textmodel_wordscores} implements Laver, Benoit and Garry's (2003) 
#' wordscores method for scaling of a single dimension.  This can be called directly, but the
#' recommended method is through \code{\link{textmodel}}.
#' @param data the dfm on which the model will be fit.  Does not need to
#'   contain only the training documents, since the index of these will be
#'   matched automatically.
#' @param scores vector of training scores associated with each document 
#'   identified in \code{refData}
#' @param smooth a smoothing parameter for word counts; defaults to zero for the 
#' to match the LBG (2003) method.
#' @param scale classic LBG linear posterior weighted word class differences, or
#'   logit scale of log posterior differences
#' @author Kenneth Benoit
#' @examples 
#' library(quantedaData)
#' data(LBGexample)
#' LBGexample <- as.dfm(LBGexample)
#' ws <- textmodel(LBGexample, c(seq(-1.5, 1.5, .75), NA), model="wordscores")
#' ws
#' # same as:
#' textmodel_wordscores(LBGexample, c(seq(-1.5, 1.5, .75), NA))
#' predict(ws)
#' @references Laver, Michael, Kenneth R Benoit, and John Garry. 2003. 
#'   "Extracting Policy Positions From Political Texts Using Words as Data." 
#'   American Political Science Review 97(02): 311-31; Beauchamp, N. 2012. 
#'   "Using Text to Scale Legislatures with Uninformative Voting." New York 
#'   University Mimeo.; Martin, L W, and G Vanberg. 2007. "A Robust
#'   Transformation Procedure for Interpreting Political Text." Political
#'   Analysis 16(1): 93-100.
#' @export
textmodel_wordscores <- function(data, scores,
                                 scale=c("linear", "logit"), smooth=0) {
    scale <- match.arg(scale)
    
    if (length(data) < 2)
        stop("wordscores model requires at least two training documents.")
    if (nrow(data) != length(scores))
        stop("trainingdata and scores vector must refer to same number of documents.")
    
    inRefSet <- which(!is.na(scores))
    if (!is.numeric(scores[inRefSet]))
        stop("wordscores model requires numeric scores.")
    
    setscores <- scores[inRefSet] # only non-NA reference texts
    data <- data + smooth         # add one to all word counts
    x <- data[inRefSet, ]         # select only the reference texts
    
    
    Fwr <- tf(x)                  # normalize words to term frequencies "Fwr"
    Pwr <- tf(t(Fwr))             # posterior word probability Pwr
    # compute likelihoods "Pwr" Pr(this word | document)
    if (scale=="linear") {
        Sw <- Pwr %*% setscores
        Sw <- Sw[,1]
    } else if (scale=="logit") {
        if (length(setscores) > 2)
            stop("\nFor logit scale, only two training texts can be used.")
        if (sum(setscores) != 0) {
            warning("\nFor logit scale, training scores are automatically rescaled to -1 and 1.")
            scores <- rescaler(setscores)
        }
        lower <- 1
        upper <- 2
        if (scores[1] > scores[2]) { lower <- 2; upper <- 1 }
        Sw <- log(Pwr[, upper]) - log(Pwr[, lower])
    }
    
    namesTemp <- names(Sw)
    Sw <- as.vector(Sw[which(colSums(x) > 0)])  # remove words with zero counts in ref set
    names(Sw) <- namesTemp[which(colSums(x) > 0)]
    
    model <- list(pi = Sw, data=data, scores=scores)
    class(model) <- c("wordscores", "list")
    return(model)
}


#' @rdname predict.textmodel
#' @param level probability level for confidence interval width
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
#' @author Ken Benoit, borrowed in places from Will Lowe, who probably borrowed from me at some early stage.
#' @export
predict.wordscores <- function(object, newdata=NULL, rescaling = "none", 
                               level=0.95, ...) {
    
    rescaling <- match.arg(rescaling, c("none", "lbg", "mv"), several.ok=TRUE)
    
    if (!is.null(newdata))
        data <- as.dfm(newdata)
    else data <- object$data
    
    featureIndex <- match(names(object$pi), features(data))
    
    scorable <- which(colnames(data) %in% names(object$pi))
    pi <- object$pi[features(data)[scorable]]
    cat(paste(length(scorable), " of ", nfeature(data), " features (",
                  round(100*length(scorable)/nfeature(data), 2),
                  "%) can be scored\n\n", sep=""))
    
    # compute text scores as weighted mean of word scores in "virgin" document
    #Fw <- tf(data)   # first compute relative term weights
    #scorable.newd <- Fw[, featureIndex]  # then exclude any features not found/scored
    scorable.newd <- subset(data, select=scorable)
    ## NOTE: This is different from computing term weights on only the scorable words
    textscore_raw <- tf(scorable.newd) %*% pi
    
    textscore_raw_se <- rep(NA, length(textscore_raw))
    Fwv <- tf(scorable.newd)
    for (i in 1:length(textscore_raw_se))
        textscore_raw_se[i] <- sqrt(sum(Fwv[i, , drop=FALSE] * (textscore_raw[i] - pi)^2)) / sqrt(rowSums(scorable.newd)[i])
    
    z <- qnorm(1 - (1-level)/2)
    
    result <- data.frame(textscore_raw,
                         textscore_raw_se,
                         textscore_raw_lo = textscore_raw - z * textscore_raw_se,
                         textscore_raw_hi = textscore_raw + z * textscore_raw_se)
    
    if ("mv" %in% rescaling) {
        if (sum(!is.na(object$scores)) > 2)
            warning("\nMore than two reference scores found with MV rescaling; using only min, max values.")
        lowerIndex <- which(object$scores==min(object$scores, na.rm=TRUE))
        upperIndex <- which(object$scores==max(object$scores, na.rm=TRUE))
        textscore_mv <-
            (textscore_raw - textscore_raw[lowerIndex]) *
            (max(object$scores, na.rm=TRUE) - min(object$scores, na.rm=TRUE)) /
            (textscore_raw[upperIndex] - textscore_raw[lowerIndex]) +
            min(object$scores, na.rm=TRUE)
        result$textscore_mv <- textscore_mv
    } 
    
    if ("lbg" %in% rescaling) {
        SDr <- sd(object$scores, na.rm=TRUE)
        Sv <- mean(textscore_raw, na.rm=TRUE)
        SDv <- ifelse(length(textscore_raw)<2, 0, sd(textscore_raw))
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
    
    class(result) <- c("wordscores", "data.frame")
    return(result)
}


## rescale a vector so that the endpoints match scale.min, scale.max
rescaler <- function(x, scale.min=-1, scale.max=1) {
    scale.width <- scale.max - scale.min
    scale.factor <- scale.width / (max(x) - min(x))
    return((x-min(x)) * scale.factor - scale.max)
}


#' @rdname print.textmodel
#' @param n max rows of dfm to print 
#' @export
print.wordscores <- function(x, n=30L, ...) {
    cat("\nReference documents and reference scores:\n\n")
    print(data.frame(Documents=docnames(x$data),
                     `Ref scores`=x$scores), ...)
    cat("\nWord scores: ")
    if (length(x$pi) > n)
        cat("showing first", n, "scored features ...")
    cat("\n\n")
    print(head(x$pi, n), ...)
}


#' @rdname summary.textmodel
#' @export
summary.wordscores <- function(object, ...) {
    cat("Call:\n\t")
    print(object$call)
    
    cat("\nReference Document Statistics:\n\n")
    dd <- data.frame(Total=apply(object$data, 1, sum),
                     Min=apply(object$data, 1, min),
                     Max=apply(object$data, 1, max),
                     Mean=apply(object$data, 1, mean),
                     Median=apply(object$data, 1, median),
                     Score=object$score)
    rownames(dd) <- docnames(object$data)
    print(dd, ...)
    invisible(dd)
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

