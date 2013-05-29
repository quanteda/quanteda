## make rows add up to one
rowNorm <- function(x) {
  x / outer(rowSums(x), rep(1, ncol(x)))  
}

## make cols add up to one 
colNorm <- function(x) {
  x / outer(rep(1, nrow(x)), colSums(x))
}

## rescale a vector so that the endpoints match scale.min, scale.max
rescaler <- function(x, scale.min=-1, scale.max=1) {
  scale.width <- scale.max - scale.min
  scale.factor <- scale.width / (max(x) - min(x))
  return((x-min(x)) * scale.factor - scale.max)
}

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


## does not check that we have the same set of words
## log.probs: would you like the class conditional sums of log prior + sum log word given class?
predictold.naivebayes <- function(object, newdata=NULL, log.probs=FALSE, normalise=TRUE) {
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


## does not check that we have the same set of words
## log.probs: would you like the class conditional sums of log prior + sum log word given class?
predict.naivebayes <- function(object, newdata=NULL, scores=c(-1,1)) {
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





wordscore.1d <- function(x, refscores=c(-1,1), smooth=1, scale="classic")
{
  # wfm can only have two rows, two scores
  if (nrow(x)!=2)
    stop("wordscore.1d requires a two-row wfm only")
  if (length(refscores)!=2)
    stop("wordscore.1d requires two reference scores only")
  # check that object type is wfm
  if (!is.wfm(x)) 
    stop("Function not applicable to this object")
  if (length(refscores) != length(docs(x))) 
    stop("There are not the same number of documents as scores")
  if (any(is.na(refscores))) 
    stop("One of the reference document scores is NA\nFit the model with known scores and use 'predict' to get virgin score estimates")
  # check that scale="classic" or "logit"
  if (scale!="classic") {
    if (scale!="logit")
      stop("scale must be either classic or logit")
  }
  thecall <- match.call()    # save the call
  x <- x + smooth            # add one to all word counts
  # CANT DO THIS SINCE PRE-NORMALIZATION, IT MEANS THAT THE WEIGHT IS GREATER FOR SHORTER
  # VERSUS LONGER DOCUMENTS - EVEN WHEN BOTH WORDS ARE ZERO COUNT
  x <- x / apply(x,1,sum)    # normalize words to term frequencies
  if (scale=="classic") {
    p <- t(apply(x, 1, function(w) w/apply(x,2,sum)))  # Pr(this word | document)
    wordscores <- apply(p * refscores, 2, sum)
  } else if (scale=="logit") {
    # x <- x + smooth
    wordscores <- log(x[2,]/x[1,])
  }
  wordscores <- matrix(wordscores, nrow=length(wordscores))
  rownames(wordscores) <- colnames(x)
  colnames(wordscores) <- c("Score")
  val <- list(pi = wordscores, theta = refscores, data = x, call = thecall, type=scale)
  class(val) <- c("ken.wordscores", "wordscores", class(val))
  return(val)
  # DOES NOT TRIM OUT THE MISSING WORDS IN THIS VERSION -- UNLIKE classic.wordscores
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
  #names(result) <- rownames(mY)
  return(result)
}

# feature.select(as.wfm(trainingset, word.margin=2), trainingclass, method="chi2")
# feature.select(as.wfm(trainingset, word.margin=2), trainingclass, method="mi")

## classic.wordscores() from austin
# function (wfm, scores) 
# {
#   if (!is.wfm(wfm)) 
#     stop("Function not applicable to this object")
#   if (length(scores) != length(docs(wfm))) 
#     stop("There are not the same number of documents as scores")
#   if (any(is.na(scores))) 
#     stop("One of the reference document scores is NA\nFit the model with known scores and use 'predict' to get virgin score estimates")
#   thecall <- match.call()
#   C.all <- as.worddoc(wfm)
#   C <- C.all[rowSums(C.all) > 0, ]
#   F <- scale(C, center = FALSE, scale = colSums(C))
#   ws <- apply(F, 1, function(x) {
#     sum(scores * x)
#   })/rowSums(F)
#   pi <- matrix(ws, nrow = length(ws))
#   rownames(pi) <- rownames(C)
#   colnames(pi) <- c("Score")
#   val <- list(pi = pi, theta = scores, data = wfm, call = thecall)
#   class(val) <- c("classic.wordscores", "wordscores", class(val))
#   return(val)
# }

