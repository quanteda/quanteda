# main methods --------------

#' Naive Bayes classifier for texts
#' 
#' Fit a multinomial or Bernoulli Naive Bayes model, given a dfm and some
#' training labels.
#' @param x the \link{dfm} on which the model will be fit.  Does not need to
#'   contain only the training documents.
#' @param y vector of training labels associated with each document identified 
#'   in \code{train}.  (These will be converted to factors if not already 
#'   factors.)
#' @param smooth smoothing parameter for feature counts by class
#' @param prior prior distribution on texts; one of \code{"uniform"},
#'   \code{"docfreq"}, or \code{"termfreq"}.  See Prior Distributions below.
#' @param distribution count model for text features, can be \code{multinomial}
#'   or \code{Bernoulli}.  To fit a "binary multinomial" model, first convert
#'   the dfm to a binary matrix using \code{\link{tf}(x, "boolean")}.
#' @return 
#' \code{textmodel_nb()} returns a list consisting of the following (where
#' \eqn{I} is the total number of documents, \eqn{J} is the total number of
#' features, and \eqn{k} is the total number of training classes):
#' @return \item{call}{original function call}
#' @return \item{PwGc}{\eqn{k \times J}; probability of the word given the class
#'   (empirical likelihood)}
#' @return \item{Pc}{\eqn{k}-length named numeric vector of class prior
#'   probabilities}
#' @return \item{PcGw}{\eqn{k \times J}; posterior class probability given the
#'   word}
#' @return \item{Pw}{\eqn{J \times 1}; baseline probability of the word}
#' @return \item{x}{the \eqn{I \times J} training dfm \code{x}}
#' @return \item{y}{the \eqn{I}-length \code{y} training class vector}
#' @return \item{distribution}{the distribution argument}
#' @return \item{prior}{the prior argument}
#' @return \item{smooth}{the value of the smoothing parameter}
#' @section Prior distributions:
#' 
#' Prior distributions refer to the prior probabilities assigned to the training
#' classes, and the choice of prior distribution affects the calculation of the
#' fitted probabilities.  The default is uniform priors, which sets the
#' unconditional probability of observing the one class to be the same as
#' observing any other class.
#'
#' "Document frequency" means that the class priors will be taken from the
#' relative proportions of the class documents used in the training set.  This
#' approach is so common that it is assumed in many examples, such as the worked
#' example from Manning, Raghavan, and Schütze (2008) below.  It is not the
#' default in \pkg{quanteda}, however, since there may be nothing informative in
#' the relative numbers of documents used to train a classifier other than the
#' relative availability of the documents.  When training classes are balanced
#' in their number of documents (usually advisable), however, then the
#' empirically computed "docfreq" would be equivalent to "uniform" priors.
#'
#' Setting \code{prior} to "termfreq" makes the priors equal to the proportions
#' of total feature counts found in the grouped documents in each training
#' class, so that the classes with the largest number of features are assigned
#' the largest priors. If the total count of features in each training class was
#' the same, then "uniform" and "termfreq" would be the same.
#' @references Manning, C. D., Raghavan, P., & Schütze, H. (2008). Introduction
#'   to Information Retrieval. Cambridge University Press.
#'   \url{https://nlp.stanford.edu/IR-book/pdf/irbookonlinereading.pdf}
#'   
#'   Jurafsky, Daniel and James H. Martin. (2016) \emph{Speech and Language
#'   Processing.}  Draft of November 7, 2016.
#'   \url{https://web.stanford.edu/~jurafsky/slp3/6.pdf}
#' @seealso \code{\link{predict.textmodel_nb}}
#' @author Kenneth Benoit
#' @examples
#' ## Example from 13.1 of _An Introduction to Information Retrieval_
#' txt <- c(d1 = "Chinese Beijing Chinese",
#'          d2 = "Chinese Chinese Shanghai",
#'          d3 = "Chinese Macao",
#'          d4 = "Tokyo Japan Chinese",
#'          d5 = "Chinese Chinese Chinese Tokyo Japan")
#' trainingset <- dfm(txt, tolower = FALSE)
#' trainingclass <- factor(c("Y", "Y", "Y", "N", NA), ordered = TRUE)
#'  
#' ## replicate IIR p261 prediction for test set (document 5)
#' (nb <- textmodel_nb(trainingset, trainingclass, prior = "docfreq"))
#' summary(nb)
#' coef(nb)
#' predict(nb)
#' 
#' # contrast with other priors
#' predict(textmodel_nb(trainingset, trainingclass, prior = "uniform"))
#' predict(textmodel_nb(trainingset, trainingclass, prior = "termfreq"))
#' 
#' ## replicate IIR p264 Bernoulli Naive Bayes
#' nb_bern <- textmodel_nb(trainingset, trainingclass, distribution = "Bernoulli", 
#'                         prior = "docfreq")
#' predict(nb_bern, newdata = trainingset[5, ])
#' @export
textmodel_nb <- function(x, y, smooth = 1, 
                         prior = c("uniform", "docfreq", "termfreq"), 
                         distribution = c("multinomial", "Bernoulli")) {
    UseMethod("textmodel_nb")
}

#' @export
textmodel_nb.default <- function(x, y, smooth = 1, 
                                 prior = c("uniform", "docfreq", "termfreq"), 
                                 distribution = c("multinomial", "Bernoulli")) {
    stop(friendly_class_undefined_message(class(x), "textmodel_nb"))
}    
    
#' @export
textmodel_nb.dfm <- function(x, y, smooth = 1, 
                             prior = c("uniform", "docfreq", "termfreq"), 
                             distribution = c("multinomial", "Bernoulli")) {
    
    x <- as.dfm(x)
    prior <- match.arg(prior)
    distribution <- match.arg(distribution)
    call <- match.call()
    
    y <- factor(y) # no effect if already a factor    
    x.trset <- x[which(!is.na(y)), ]
    y.trclass <- y[!is.na(y)]
    types <- colnames(x)
    docs <- rownames(x)  
    levs <- levels(y.trclass)
    
    ## distribution
    if (distribution == "Bernoulli") {
        x.trset <- dfm_weight(x.trset, "boolean")
    } else {
        if (distribution != "multinomial")
            stop("Distribution can only be multinomial or Bernoulli.")
    }
    
    ## prior
    if (prior=="uniform") {
        Pc <- rep(1/length(levs), length(levs))
        names(Pc) <- levs
    } else if (prior=="docfreq") {
        Pc <- prop.table(table(y.trclass))
        Pc_names <- names(Pc)
        attributes(Pc) <- NULL
        names(Pc) <- Pc_names
    } else if (prior=="termfreq") {
        # weighted means the priors are by total words in each class
        # (the probability that any given word is in a particular class)
        temp <- x.trset
        rownames(temp) <- y.trclass
        colnames(temp) <- rep("all_same", nfeat(temp))
        temp <- dfm_compress(temp)
        Pc <- prop.table(as.matrix(temp))
        Pc_names <- rownames(Pc)
        attributes(Pc) <- NULL
        names(Pc) <- Pc_names
    }
    
    ## multinomial ikelihood: class x words, rows sum to 1
    # combine all of the class counts
    rownames(x.trset) <- y.trclass
    d <- dfm_compress(x.trset, margin = "both")

    if (distribution == "multinomial") {
        PwGc <- dfm_smooth(d, smooth) %>% dfm_weight(scheme = "prop")
    } else if (distribution == "Bernoulli") {
        # if (smooth != 1) {
        #     warning("smoothing of 0 makes little sense for Bernoulli NB", 
        #             call. = FALSE, noBreaks. = TRUE)
        # }
        # denominator here is same as IIR Fig 13.3 line 8 - see also Eq. 13.7
        PwGc <- (d + smooth) / 
            (as.vector(table(docnames(x.trset))[docnames(d)]) + smooth * ndoc(d))
        PwGc <- as(PwGc, "dgeMatrix")
    }
    
    # order Pc so that these are the same order as rows of PwGc
    Pc <- Pc[rownames(PwGc)]

    ## posterior: class x words, cols sum to 1
    PcGw <- colNorm(PwGc * base::outer(Pc, rep(1, ncol(PwGc))))  

    # rename row dimensions
    names(dimnames(PcGw))[1] <- names(dimnames(PwGc))[1] <- "classes"
    
    ## P(w)
    Pw <- t(PwGc) %*% as.numeric(Pc)

    result <- list(
        call = call,
        PwGc = as.matrix(PwGc),
        Pc = Pc,
        PcGw = as.matrix(PcGw),
        Pw = as.matrix(Pw),
        x = x, y = y,
        distribution = distribution,
        prior = prior,
        smooth = smooth
    )
    class(result) <- c("textmodel_nb", "textmodel", "list")
    result
}

#' Prediction from a fitted textmodel_nb object
#' 
#' \code{predict.textmodel_nb()} implements class predictions from a fitted
#' Naive Bayes model. using trained Naive Bayes examples
#' @param object a fitted Naive Bayes textmodel 
#' @param newdata dfm on which prediction should be made
#' @param ... not used
#' @return \code{predict.textmodel_nb} returns a list of two data frames, named
#'   \code{docs} and \code{words} corresponding to word- and document-level
#'   predicted quantities
#' @return \item{docs}{data frame with document-level predictive quantities:
#'   nb.predicted, ws.predicted, bs.predicted, PcGw, wordscore.doc,
#'   bayesscore.doc, posterior.diff, posterior.logdiff.  Note that the diff
#'   quantities are currently implemented only for two-class solutions.}
#' @return \item{words}{data-frame with word-level predictive quantities: 
#' wordscore.word, bayesscore.word}
#' @examples 
#' # application to LBG (2003) example data
#' (nb <- textmodel_nb(data_dfm_lbgexample, c("A", "A", "B", "C", "C", NA)))
#' predict(nb)
#' @keywords textmodel internal
#' @export
predict.textmodel_nb <- function(object, newdata = NULL, ...) {
    
    call <- match.call()
    if (is.null(newdata)) newdata <- as.dfm(object$x)

    # remove any words for which zero probabilities exist in training set --
    # would happen if smooth=0
    # the condition assigns the index of zero occurring words to vector 
    # "notinref" and only trims the objects if this index has length > 0
    if (length(notinref <- which(colSums(object$PwGc) == 0))) {
        object$PwGc <- object$PwGc[-notinref]
        object$PcGw <- object$PcGw[-notinref]
        object$Pw   <- object$Pw[-notinref]
        object$x <- object$x[, -notinref]
        newdata <- newdata[, -notinref] 
    }

    # make sure feature set is ordered the same in test and training set (#490)
    if (ncol(object$PcGw) != ncol(newdata))
        stop("feature set in newdata different from that in training set")
    if (!identical(colnames(object$PcGw), colnames(newdata)) || 
        setequal(colnames(object$PcGw), colnames(newdata))) {
        # if feature names are the same but diff order, reorder
        newdata <- newdata[, colnames(object$PcGw)]
    } else {
        stop("feature set in newdata different from that in training set")
    }
    
    if (object$distribution == "multinomial") {
        
        # log P(d|c) class conditional document likelihoods
        log.lik <- newdata %*% t(log(object$PwGc))
        # weight by class priors
        log.posterior.lik <- t(apply(log.lik, 1, "+", log(object$Pc)))
        
    } else if (object$distribution == "Bernoulli") {
        
        newdata <- dfm_weight(newdata, "boolean")
        Nc <- length(object$Pc)
        
        # initialize log posteriors with class priors
        log.posterior.lik <- matrix(log(object$Pc), byrow = TRUE, 
                                    ncol = Nc, nrow = nrow(newdata),
                                    dimnames = list(rownames(newdata), names(object$Pc)))
        # APPLYBERNOULLINB from IIR Fig 13.3
        for (c in seq_len(Nc)) {
            tmp1 <- log(t(newdata) * object$PwGc[c, ])
            tmp1[is.infinite(tmp1)] <- 0
            tmp0 <- log(t(!newdata) * (1 - object$PwGc[c, ]))
            tmp0[is.infinite(tmp0)] <- 0
            log.posterior.lik[, c] <- log.posterior.lik[, c] + colSums(tmp0) + colSums(tmp1)
        }
    } 

    
    # predict MAP class
    nb.predicted <- colnames(log.posterior.lik)[apply(log.posterior.lik, 1, which.max)]
    
    ## now compute class posterior probabilities
    # initialize posterior probabilities matrix
    posterior.prob <- matrix(NA, ncol = ncol(log.posterior.lik), 
                             nrow = nrow(log.posterior.lik),
                             dimnames = dimnames(log.posterior.lik))

    # compute posterior probabilities
    for (j in seq_len(ncol(log.posterior.lik))) {
        base.lpl <- log.posterior.lik[, j]
        posterior.prob[, j] <- 1 / 
            (1 + rowSums(exp(log.posterior.lik[, -j, drop = FALSE] - base.lpl)))
    }

    result <- list(
        log.posterior.lik = log.posterior.lik,
        posterior.prob = posterior.prob,
        nb.predicted = nb.predicted,
        Pc = object$Pc,
        classlabels = names(object$Pc),
        call = call
    )
    class(result) <- c("predict.textmodel_nb", "list")
    result
}

#' @export
#' @method print textmodel_nb
print.textmodel_nb <- function(x, ...) {
    cat("\nCall:\n")
    print(x$call)
    cat("\n",
        "Distribution: ", x$distribution, "; ", 
        "prior: ", x$prior, "; ",
        "smoothing value: ", x$smooth, "; ",
        length(na.omit(x$y)), " training documents; ",
        nfeat(na.omit(x)), " fitted features.",
        "\n",
        sep = "")
}

#' summary method for textmodel_nb objects
#' @param object output from \code{\link{textmodel_nb}}
#' @param n how many coefficients to print before truncating
#' @param ... additional arguments not used
#' @keywords textmodel internal
#' @method summary textmodel_nb
#' @export
summary.textmodel_nb <- function(object, n = 30, ...) {
    result <- list(
        'call' = object$call,
        'class.priors' = as.coefficients_textmodel(object$Pc),
        'estimated.feature.scores' = as.coefficients_textmodel(head(coef(object), n))
    )
    as.summary.textmodel(result)
}

#' @noRd
#' @method coef textmodel_nb
#' @export
coef.textmodel_nb <- function(object, ...) {
    t(object$PcGw)
}

#' @noRd
#' @method coefficients textmodel_nb
#' @export
coefficients.textmodel_nb <- function(object, ...) {
    UseMethod("coef")   
}

## make cols add up to one
colNorm <- function(x) {
    x / outer(rep(1, nrow(x)), colSums(x))
}

#' @export
#' @method print predict.textmodel_nb
print.predict.textmodel_nb <- function(x, ...) {
    print(unclass(x))
}
