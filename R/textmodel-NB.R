#' Naive Bayes classifier for texts
#' 
#' Currently working for vectors of texts -- not specially defined for a dfm.
#' 
#' This naive Bayes model works on word counts, with smoothing.
#' @param x the dfm on which the model will be fit.  Does not need to contain 
#'   only the training documents.
#' @param y vector of training labels associated with each document identified
#'   in \code{train}.  (These will be converted to factors if not already
#'   factors.)
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
#' trainingset <- as.dfm(matrix(c(1, 2, 0, 0, 0, 0,
#'                         0, 2, 0, 0, 1, 0,
#'                         0, 1, 0, 1, 0, 0,
#'                         0, 1, 1, 0, 0, 1,
#'                         0, 3, 1, 0, 0, 1), 
#'                       ncol=6, nrow=5, byrow=TRUE,
#'                       dimnames = list(docs = paste("d", 1:5, sep = ""),
#'                                       features = c("Beijing", "Chinese",  "Japan", "Macao", 
#'                                                    "Shanghai", "Tokyo"))))
#' trainingclass <- factor(c("Y", "Y", "Y", "N", NA), ordered = TRUE)
#' ## replicate IIR p261 prediction for test set (document 5)
#' (nb.p261 <- textmodel_NB(trainingset, trainingclass)) #, prior = "docfreq"))
#' predict(nb.p261, newdata = trainingset[5, ])
#' 
#' @export
textmodel_NB <- function(x, y, smooth = 1, prior = c("uniform", "docfreq", "termfreq"), 
                         distribution = c("multinomial", "Bernoulli"), ...) {
    call <- match.call()
    prior <- match.arg(prior)
    distribution <- match.arg(distribution)
    
    y <- factor(y) # no effect if already a factor    
    x.trset <- x[which(!is.na(y)), ]
    y.trclass <- y[!is.na(y)]
    types <- colnames(x)
    docs <- rownames(x)  
    levs <- levels(y.trclass)
    
    ## distribution
    if (distribution == "Bernoulli") 
        x <- tf(x, "boolean")
    else
        if (distribution != "multinomial")
            stop("Distribution can only be multinomial or Bernoulli.")
    
    ## prior
    if (prior=="uniform")
        Pc <- rep(1/length(levs), length(levs))
    else if (prior=="docfreq")
        Pc <- prop.table(table(y.trclass))
    else if (prior=="termfreq") {
        # weighted means the priors are by total words in each class
        # (the probability that any given word is in a particular class)
        temp <- stats::aggregate(x.trset, by = list(y.trclass), sum)
        temp2 <- apply(temp[,-1], 1, sum)
        names(temp2) <- temp[,1]
        Pc <- prop.table(as.table(temp2))
    } else stop("Prior must be either docfreq (default), wordfreq, or uniform")
    
    ## multinomial ikelihood: class x words, rows sum to 1
    # d <- t(sapply(split(as.data.frame(x.trset), y.trclass), colSums))
    # combine all of the class counts
    rownames(x.trset) <- y.trclass
    d <- compress(x.trset, margin = "both")

    PwGc <- rowNorm(d + smooth)
    names(Pc) <- rownames(d)
    
    ## posterior: class x words, cols sum to 1
    PcGw <- colNorm(PwGc * outer(Pc, rep(1, ncol(PwGc))))  
    
    ## P(w)
    Pw <- t(PwGc) %*% Pc
    
    ll <- list(call=call, PwGc=PwGc, Pc=Pc, PcGw = PcGw, Pw = Pw, 
               data = list(x=x, y=y), 
               distribution = distribution, prior = prior, smooth = smooth)
    class(ll) <- c("textmodel_NB_fitted", class(ll))
    return(ll)
}


#' prediction method for Naive Bayes classifier objects
#'
#' implements class predictions using trained Naive Bayes examples 
#' @param object a fitted Naive Bayes textmodel 
#' @param newdata dfm on which prediction should be made
#' @param ... not used
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
#' @examples 
#' (nbfit <- textmodel_NB(LBGexample, c("A", "A", "B", "C", "C", NA)))
#' (nbpred <- predict(nbfit))
#' @export
predict.textmodel_NB_fitted <- function(object, newdata = NULL, ...) {
    
    call <- match.call()
    if (is.null(newdata)) newdata <- object$data$x

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
    
    # log P(d|c) class conditional document likelihoods
    log.lik <- newdata %*% t(log(object$PwGc))
    # weight by class priors
    log.posterior.lik <- t(apply(log.lik, 1, "+", log(object$Pc)))
    
    # predict MAP class
    nb.predicted <- colnames(log.posterior.lik)[apply(log.posterior.lik, 1, which.max)]
    
    ## now compute class posterior probabilities
    # initialize posterior probabilities matrix
    posterior.prob <- matrix(NA, ncol = ncol(log.posterior.lik), 
                             nrow = nrow(log.posterior.lik),
                             dimnames = dimnames(log.posterior.lik))

    # compute posterior probabilities
    for (j in 1:ncol(log.posterior.lik)) {
        base.lpl <- log.posterior.lik[, j]
        posterior.prob[, j] <- 1 / (1 + rowSums(exp(log.posterior.lik[, -j, drop = FALSE] - base.lpl)))
    }

    result <- list(log.posterior.lik = log.posterior.lik, 
                   posterior.prob = posterior.prob, 
                   nb.predicted = nb.predicted, 
                   Pc = object$Pc, 
                   classlabels = names(object$Pc), 
                   call = call)
    class(result) <- c("textmodel_NB_predicted", class(result))
    result
}

# not used any more
logsumexp <- function(x) {
    xmax <- which.max(x)
    log1p(sum(exp(x[-xmax] - x[xmax]))) + x[xmax]
}

# @rdname print.textmodel
#' @export
#' @method print textmodel_NB_fitted
print.textmodel_NB_fitted <- function(x, n=30L, ...) {
    cat("Fitted Naive Bayes model:\n")
    cat("Call:\n\t")
    print(x$call)
    cat("\n")
    
    cat("\nTraining classes and priors:\n")
    print(x$Pc)
    
    cat("\n\t\t  Likelihoods:\t\tClass Posteriors:\n")
    print(head(t(rbind(x$PwGc, x$PcGw)), n))
    
    cat("\n")
}


# @param x for print method, the object to be printed
# @param n max rows of dfm to print
# @param digits number of decimal places to print for print methods
# @param ... not used in \code{print.textmodel_wordscores_fitted}

#' @export
#' @method print textmodel_NB_predicted
print.textmodel_NB_predicted <- function(x, n = 30L, digits = 4, ...) {
    
    cat("Predicted textmodel of type: Naive Bayes\n")
    # cat("Call:\n\t")
    # print(x$call)
    if (nrow(x$log.posterior.lik) > n)
        cat("(showing", n, "of", nrow(x$docs), "documents)\n")
    cat("\n")
    
    docsDf <- data.frame(x$log.posterior.lik, x$posterior.prob, x$nb.predicted)
    names(docsDf) <- c(paste0("lp(", x$classlabels, ")"),
                       paste0("Pr(", x$classlabels, ")"),
                       "Predicted")
    k <- length(x$classlabels)
    docsDf[, 1:k] <- format(docsDf[, 1:k], nsmall = digits) #digits = digits + 6)
    docsDf[, (k+1):(2*k)] <- round(docsDf[, (k+1):(2*k)], digits) #, digits = digits)
    docsDf[, (k+1):(2*k)] <- format(docsDf[, (k+1):(2*k)], nsmall = digits) #, digits = digits)
    
    # add a whitespace column for visual padding
    docsDf <- cbind(docsDf[, 1:k], " " = rep("  ", nrow(docsDf)), docsDf[, (k+1):(2*k+1)])
    
    print(docsDf[1:(min(n, nrow(docsDf))), ], digits = digits)
    cat("\n")
}


## some helper functions

## make rows add up to one
rowNorm <- function(x) {
    x / outer(rowSums(x), rep(1, ncol(x)))
}

## make cols add up to one
colNorm <- function(x) {
    x / outer(rep(1, nrow(x)), colSums(x))
}

