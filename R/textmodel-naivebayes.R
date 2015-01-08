
## make rows add up to one
rowNorm <- function(x) {
    x / outer(rowSums(x), rep(1, ncol(x)))
}

## make cols add up to one
colNorm <- function(x) {
    x / outer(rep(1, nrow(x)), colSums(x))
}


# Naive Bayes text model
#
# \code{textmodel_NB} implements Naive Bayes model for class prediction on a
# set of labelled texts.
# @param trainData the dfm on which the model will be fit.  Does not need to contain
#   only the training documents, since the index of these will be identified in
#   \code{train}.
# @param trainLabels vector of training labels associated with each document
#   identified in \code{train}.  (These will be converted to factors if not
#   already factors.)
# @param smooth a smoothing parameter for word counts, default is 1.0.
# @param distribution either \code{multinomial} to count all word occurrences,
# or \code{bernoulli} to count feature occurrences as binary
# @param classpriors an optional named vector of class priors, corresponding to
#   the levels or values of \code{train.class}.  This does not need to be
#   ordered, but must be named, and the names must match 1:1 to the levels of
#   \code{train.class}.  Default is uniform class priors, not based on the
#   balance of trainind documents, but rather uniform across the possible
#   classes.  See details.
# @section Class priors: Details on this will be provided soon.
# @section Smoothing values: Smoothing values are added to features observed in
#   each class, which consist of all observed features per class, not per
#   document.  More soon.
# @author Kenneth Benoit
# @references Laver, Benoit and Garry (2003); Martin and Vanberg (2007)
# @export
# textmodel_NB <- function(trainData, trainLabels, smooth=1,
#                         distribution=c("multinomial", "bernoulli"),
#                         classpriors=1/length(table(trainLabels))) {
#     thecall <- match.call()
#     distribution <- match.arg(distribution)
#     labels <- levels(as.factor(trainLabels))
#     trainData <- trainData + smooth
#     Pwc <- rowSums(log(trainData))
#     
#     warning("textmodel_NB not implented yet!")
#     
#     # model <- list(featureScores = NULL, train=train, train.class=train.class)
#     class(model) <- c("NB", class(model))
#     return(model)
# }


#' Naive Bayes classifier for texts
#' 
#' Currently working for vectors of texts -- not specially defined for a dfm.
#' 
#' This naive Bayes model works on word counts, with smoothing.
#' @param x the dfm on which the model will be fit.  Does not need to contain 
#'   only the training documents.
#' @param y vector of training labels associated with each document 
#'   identified in \code{train}.  (These will be converted to factors if not 
#'   already factors.)
#' @param smooth smoothing parameter for feature counts by class
#' @param prior prior distribution on texts, see details
#' @param distribution count model for text features, can be \code{multinomial}
#'   or \code{Bernoulli}
#' @param ... more arguments passed through
#' @return A list of return values, consisting of:
#' @return \item{call}{original function call}
#' @return \item{PwGc}{probability of the word given the class (empirical
#'   likelihood)}
#' @return \item{Pc}{class prior probability}
#' @return \item{PcGw}{posterior class probability given the word}
#' @return \item{Pw}{baseline probability of the word}
#' @return \item{data}{list consisting of \code{x} training class, and \code{y}
#'   test class}
#' @return \item{distribution}{the distribution argument}
#' @return \item{prior}{argument passed as a prior}
#' @return \item{smooth}{smoothing parameter}
#' @author Kenneth Benoit
#' @examples
#' ## Example from 13.1 of _An Introduction to Information Retrieval_
#' trainingset <- matrix(c(1, 2, 0, 0, 0, 0,
#'                         0, 2, 0, 0, 1, 0,
#'                         0, 1, 0, 1, 0, 0,
#'                         0, 1, 1, 0, 0, 1,
#'                         0, 3, 1, 0, 0, 1), 
#'                       ncol=6, nrow=5, byrow=TRUE,
#'                       dimnames = list(docs=paste("d", 1:5, sep=""),
#'                                       features=c("Beijing", "Chinese",  "Japan", "Macao", 
#'                                         "Shanghai", "Tokyo")))
#' trainingclass <- factor(c("Y", "Y", "Y", "N", NA), ordered=TRUE)
#' ## replicate IIR p261 prediction for test set (document 5)
#' nb.p261 <- textmodel_NB(as.dfm(trainingset), trainingclass, smooth=1, prior="docfreq")
#' print(unclass(predict(nb.p261)))
#' @export
textmodel_NB <- function(x, y, smooth=1, prior="uniform", distribution="multinomial", ...) 
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


#' prediction method for Naive Bayes classifier objects
#'
#' implements class predictions using trained Naive Bayes examples 
#' @param scores "reference" values when the wordscores equivalent implementation
#' of Naive Bayes prediction is used.  Default is \code{c(-1, 1)}.
#' @return A list of two data frames, named \code{docs} and \code{words} corresponding
#' to word- and document-level predicted quantities
#' @return \item{docs}{data frame with document-level predictive quantities: 
#' nb.predicted, ws.predicted, bs.predicted, PcGw, wordscore.doc, bayesscore.doc, 
#' posterior.diff, posterior.logdiff.  Note that the diff quantities are currently 
#' implemented only for two-class solutions.}
#' @return \item{words}{data-frame with word-level predictive quantities: 
#' wordscore.word, bayesscore.word}
#' @author Kenneth Benoit
#' @rdname predict.textmodel
#' @export
predict.naivebayes <- function(object, newdata=NULL, scores=c(-1,1), ...) {
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
    result <- list(docs=df.doc, words=df.words)
    class(result) <- c("naivebayes", class(result))
    result
}

logsumexp <- function(x) {
    xmax <- which.max(x)
    log1p(sum(exp(x[-xmax] - x[xmax]))) + x[xmax]
}


# @rdname print.textmodel
#' @export
#' @method print naivebayes
print.naivebayes <- function(x, n=30L, ...) {
    cat("Call:\n\t")
    print(x$call)
    
    cat("\nTraining classes and priors:\n")
    print(x$Pc)
    
    cat("\n\t\t\t  Likelihoods:\tClass Posteriors:\n")
    print(head(t(rbind(x$PwGc, x$PcGw)), n))
        
    cat("\n")
}



