#' Transform a list of ordinal target labels to a numeric vector
#' 
#' Returns a numeric vector for use as target variable for a textmodel
#' 
#' @param categories A character vector of ordinal target values
#' @param values A list of numbers to map the ordinal values on to
#' @return A numeric vector of the same length as categories
#' @export
#' @author Paul Nulty
#' @examples
#' cats <- docvars(inaugCorpus, 
#' 
#' # combine these methods for more complex weightings, e.g. as in Section 6.4 of
#' # Introduction to Information Retrieval
#' logTfDtm <- weight(dtm, type="logTf")
#' wfidfDtm <- weight(logTfDtm, type="tfidf")
#' 
#' @references Manning, Christopher D., Prabhakar Raghavan, and Hinrich Schutze.
#'   Introduction to information retrieval. Vol. 1. Cambridge: Cambridge 
#'   university press, 2008.
transformOrdinal <- function(categories, values=NULL){
    catFactor <- as.factor(categories)
    if(is.null(values)){
        return(as.numeric(catFactor))
    }
    if(length(levels(catFactor))!=length(values)){
        stop(sprintf('Number of values (%d) does not equal number of categories (%d)',length(values), length(levels(catFactor)) ))
    }
    
    newVals <- rep(values[1],length(cats))
    i <- 2
    while(i<=length(values)){
        newVals[which(cats==levels(catFactor)[i])] <- values[i]
        i <- i+1
    }
    return(newVals)
}



#' Perform cross-validation
#' 
#' Returns a numeric vector for use as target variable for a textmodel
#' 
#' @param dtm A dfm containing documents and features for training and testing.
#' @param values The 'true' target values for each document
#' @param k The number of 'folds' to split the data into. Default is leave-one-out.
#' @export
#' @author Paul Nulty
crossVal <- function(dtm, values, k=nrow(dtm)){
    foldSize <-  floor(nrow(dtm)/k)
    foldStart <- 1
    foldEnd <- 1
    while(foldStart < nrow(dtm)){
        foldEnd <- foldStart+foldSize
        if (foldEnd > nrow(dtm)){
            foldEnd <-  nrow(dtm)
        }
        
        thisTrain <- as.dfm(dtm[-(foldStart:foldEnd),])
        trainRefs <- refs[-(foldStart:foldEnd)]
        
        thisHoldout <- as.dfm(dtm[foldStart:foldEnd,])
        testRefs <- refs[foldStart:foldEnd]
         
        thisModel <- textmodel(thisTrain, trainRefs)
        thisResult <- predict(thisModel, thisHoldout)
        
        foldStart <- foldStart+foldSize
    }
}


library(quantedaData)
library(quanteda)
data(sotuCorp)
dtm <- dfm(sotuCorp)
refs <- transformOrdinal(docvars(sotuCorp, "party"))
crossVal(dtm,refs)
i <- 1
k=5
foldSize <-  floor(nrow(dtm)/k)
foldStart <- 1
foldEnd <- 1
while(foldStart < nrow(dtm)){
    foldEnd <- foldStart+foldSize
    if (foldEnd > nrow(dtm)){
        foldEnd = nrow(dtm)
    }
    
    thisTrain <- as.dfm(dtm[-(foldStart:foldEnd),])
    trainRefs <- refs[-(foldStart:foldEnd)]
    
    thisHoldout <- as.dfm(dtm[foldStart:foldEnd,])
    testRefs <- refs[foldStart:foldEnd]
    
    
    thisModel <- textmodel(thisTrain, trainRefs)
    thisResult <- predict(thisModel, thisHoldout)
    
    foldStart <- foldStart+foldSize
}

thisModel <- textmodel(thisTrain, trainRefs)
thisResult <- predict(thisModel, thisHoldout)


# Naive Bayes classifier for texts
#
# Currently working for vectors of texts.
# @title Naive Bayes classifier for texts
# @param x character vector of training texts
# @param y character vector of test texts
# @param smooth smoothing parameter for feature counts by class
# @param prior prior distribution on texts, see details
# @param distribution count model for text features, can be \code{multinomial} or \code{Bernoulli} 
# @param ... 
# @return A list of return values, consisting of:
# @return \item{call}{original function call}
# @return \item{PwGc}{probability of the word given the class (empirical likelihood)}
# @return \item{Pc}{class prior probability}
# @return \item{PcGw}{posterior class probability given the word}
# @return \item{Pw}{baseline probability of the word}
# @return \item{data}{list consisting of \code{x} training class, and \code{y} test class}
# @return \item{distribution}{the distribution argument}
# @return \item{prior}{argument passed as a prior}
# @return \item{smooth}{smoothing parameter}
# @author Kenneth Benoit
# @export
naiveBayesText <- function(x, y, smooth=1, prior="uniform", distribution="multinomial", ...) 
{
    x.trset <- x[!is.na(y),]
    y.trclass <- y[!is.na(y)]
    call <- match.call()
    types <- colnames(x)
    docs <- rownames(x)  
    levs <- levels(y.trclass)
    
    ## distribution
    if (distribution=="Bernoulli") 
        x.trset[x.trset>0] <- 1 # convert to Boolean
    else
        if (distribution!="multinomial")
            stop("Distribution can only be multinomial or Bernoulli.")
    
    ## prior
    if (prior=="uniform")
        Pc <- rep(1/length(levs), length(levs))
    else if (prior=="docfreq")
        Pc <- prop.table(table(y.trclass))
    else if (prior=="termfreq") {
        # weighted means the priors are by total words in each class
        # (the probability that any given word is in a particular class)
        temp <- aggregate(x.trset, by=list(y.trclass), sum)
        temp2 <- apply(temp[,-1], 1, sum)
        names(temp2) <- temp[,1]
        Pc <- prop.table(as.table(temp2))
    } else stop("Prior must be either docfreq (default), wordfreq, or uniform")
    
    ## multinomial ikelihood: class x words, rows sum to 1
    # d <- aggregate(x.trset, by=list(CLS=y.trclass), sum)  ## SO SLOW!!!
    d <- t(sapply(split(as.data.frame(x.trset), y.trclass), colSums))
    PwGc <- rowNorm(d + smooth)
    names(Pc) <- rownames(d)
    
    ## posterior: class x words, cols sum to 1
    PcGw <- colNorm(PwGc * outer(Pc, rep(1, ncol(PwGc))))  
    
    ## P(w)
    Pw <- t(PwGc) %*% Pc
    
    ll <- list(call=call, PwGc=PwGc, Pc=Pc, PcGw=PcGw, Pw=Pw, data=list(x=x, y=y), 
               distribution=distribution, prior=prior, smooth=smooth)
    class(ll) <- c("naivebayes", class(ll))
    return(ll)
}


predictold.naivebayes <- function(object, newdata=NULL, log.probs=FALSE, normalise=TRUE) {
    ## does not check that we have the same set of words
    ## log.probs: would you like the class conditional sums of log prior + sum log word given class?
    log.lik   <- t(log(object$PwGc))  
    log.prior <- log(object$Pc)
    if (is.null(newdata))
        newdata <- object$data$x
    
    D <- newdata %*% log.lik  ## D_ij = sum^V_w count_w-in-i log P(w-in-j | class=j) 
    lp <- D + outer(rep(1, nrow(newdata)), log.prior)
    if (log.probs)
        return(lp) 
    else if (normalise)
        return(rowNorm(exp(lp)))  # numeric underflow with any real data!
    else return(exp(lp))
}


# prediction method for Naive Bayes classifier objects
#
# implements class predictions using trained Naive Bayes examples 
# (from \code{naiveBayesText()})
# 
# @title prediction method for Naive Bayes classifiers
# @param object a naivebayes class object
# @param newdata new data on which to perform classification
# @param scores "reference" values when the wordscores equivalent implementation
# of Naive Bayes prediction is used.  Default is \code{c(-1, 1)}.
# @return A list of two data frames, named \code{docs} and \code{words} corresponding
# to word- and document-level predicted quantities
# @return \item{docs}{data frame with document-level predictive quantities: 
# nb.predicted, ws.predicted, bs.predicted, PcGw, wordscore.doc, bayesscore.doc, 
# posterior.diff, posterior.logdiff.  Note that the diff quantities are currently 
# implemented only for two-class solutions.}
# @return \item{words}{data-frame with word-level predictive quantities: 
# wordscore.word, bayesscore.word}
# @author Kenneth Benoit
# @rdname predict
# @method predict naivebayes
# @S3method predict naivebayes
# @export
predict.naivebayes <- function(object, newdata=NULL, scores=c(-1,1)) {
    ## does not check that we have the same set of words
    ## log.probs: would you like the class conditional sums of log prior + sum log word given class?
    bayesscore.word <- NULL
    bayesscore.doc <- NULL
    wordscore.word <- NULL
    wordscore.doc <- NULL
    if (is.null(newdata))
        newdata <- object$data$x
    # need to check that has same dimensions as x if (!is.null(newdata))
    
    # remove any words for which zero probabilities exist in training set --
    # would happen if smooth=0
    # the condition assigns the index of zero occurring words to vector "notinref" and only 
    # trims the objects if this index has length>0
    if (length(notinref <- which(colSums(object$PwGc)==0))) {
        object$PwGc <- object$PwGc[-notinref]
        object$PcGw <- object$PcGw[-notinref]
        object$Pw   <- object$Pw[-notinref]
        object$data$x <- object$data$x[,-notinref]
        newdata <- newdata[,-notinref] 
    }
    
    log.lik   <- t(log(object$PwGc))
    lPwGc <- (newdata %*% log.lik)  # log P(w|c) class conditional word likelihood
    # lPc <- outer(rep(1, nrow(newdata)), log(object$Pc))  # log P(c) class prior
    # lPw <- sum(log(object$Pw))
    # lPcGw <- lPc + lPwGc - lPw  # log P(c|w), prop to since excludes P(w) normalization constant
    
    # compute the scaled quantities that come from the training set words
    if (!is.null(scores)) {
        if (length(object$Pc)!=length(scores))
            stop("scores must be equal in length to number of classes.")
        bayesscore.word <- log.lik %*% scores
        wordscore.word <- t(object$PcGw) %*% scores
        bayesscore.doc <- rowNorm(newdata) %*% bayesscore.word 
        #  newdata * t(outer(as.vector(temp), rep(1, nrow(newdata))))
        wordscore.doc <- rowNorm(newdata) %*% wordscore.word
    }
    
    # eta <- lPwGc[,1] - lPwGc[,2]
    eta <- bayesscore.doc * rowSums(newdata) + log(object$Pc[2]/object$Pc[1])
    bayesscore.doc <- bayesscore.doc + log(object$Pc[2]/object$Pc[1])
    PcGw <- cbind(1/(1+exp(eta)), 1/(1+exp(-eta))) 
    colnames(PcGw) <- colnames(lPwGc)  
    
    nb.predicted <- colnames(PcGw)[apply(PcGw, 1, which.max)]
    dirtest <- ifelse(!is.null(scores) && scores[1]>scores[2], -1, 1)
    ws.predicted <- colnames(PcGw)[(dirtest*wordscore.doc > 0)+1]
    bs.predicted <- colnames(PcGw)[(dirtest*bayesscore.doc > 0)+1]
    # can't predict these without a smoother
    
    if (sum(object$PwGc==0)) nb.predicted <- bs.predicted <- rep(NA, nrow(wordscore.doc))
    
    df.doc <- data.frame(nb.predicted, ws.predicted, bs.predicted, PcGw, wordscore.doc, bayesscore.doc) 
    df.doc$posterior.diff <- 1 - 2*PcGw[,1]  ## ONLY WORKS NOW FOR 2-CLASS SOLUTIONS
    df.doc$posterior.logdiff <- log(1-PcGw[,1]) - log(PcGw[,1])  ## ONLY WORKS NOW FOR 2-CLASS SOLUTIONS
    df.words <- data.frame(wordscore.word, bayesscore.word)
    return(list(docs=df.doc, words=df.words))
}

logsumexp <- function(x) {
    xmax <- which.max(x)
    log1p(sum(exp(x[-xmax] - x[xmax]))) + x[xmax]
}




feature.select <- function(wfm, trclass, freq="document", method="chi2", 
                           min.count=NULL, min.doc=NULL, sample=NULL) {
    require(austin)
    if (!is.wfm(wfm)) stop("Input must be an (austin-defined) wfm object.")
    mY <- as.worddoc(wfm[,!is.na(trainingclass)])  # select only training set docs
    
    if (method=="frequency") {
        if (is.null(min.count) & is.null(min.doc) & is.null(sample)) {
            stop("With method='frequency', must specify at least one of min.count, min.doc, or sample.")
        }
        return(trim(wfm, min.count, min.doc, sample))
    }
    
    if (freq=="document") {
        mY[mY>0] <- 1 # convert to Boolean
    } else if (freq=="word") {
        stop("freq=word not yet implemented")
    } else 
        stop("Only document or word valid for frequency= argument.")
    
    getNs <- function(word, class) {
        t <- table(word, class)
        N00 <- t[1,1]
        N01 <- t[1,2]
        N10 <- t[2,1]
        N11 <- t[2,2]
        N1S <- N10 + N11
        N0S <- N00 + N01
        NS1 <- N01 + N11
        NS0 <- N00 + N01
        return(list(N00, N01, N10, N11, N1S, N0S, NS1, NS0))
    }
    
    if (method=="chi2") {
        result <- apply(mY, 1, 
                        function(word) ifelse(sum(dim(table(word, trclass)))==4, chisq.test(word, trclass)$statistic, NA))
    } else if (method=="mi") {
        require(entropy)
        entropyClass <- entropy(table(trclass))
        result <- apply(mY, 1, 
                        function(word) entropy(table(word)) + entropyClass - entropy(table(word, trclass)))
        
        # stop("method=mi (mutual information) not yet implemented.")
    } else stop("method can be only chi2, mi, or frequency.")
    return(result)
}

#feature.select(as.wfm(trainingset, word.margin=2), trainingclass, method="chi2")
#feature.select(as.wfm(trainingset, word.margin=2), trainingclass, method="mi")


