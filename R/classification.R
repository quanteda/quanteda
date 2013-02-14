naiveBayesText <- function(x, y, smooth=1, prior="docfreq", distribution="multinomial", ...) 
{
  x.trset <- x[!is.na(y),]
  y.trclass <- y[!is.na(y)]
  call <- match.call()
  types <- colnames(x)
  docs <- rownames(x)  
  levs <- levels(y.trclass)

  ## distribution
  if (distribution=="Bernoulli") 
    stop("Bernoulli not implemented yet.")
  else
    if (distribution!="multinomial")
      stop("distribution can only be multinomial or Bernoulli.")
  
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
  } else stop('Prior must be either docfreq (default), wordfreq, or uniform')

  ## likelihood: class x words, rows sum to 1
  d <- aggregate(x.trset, by=list(CLS=y.trclass), sum)
  PwGc <- rowNorm(d[,-1] + smooth)
  rownames(PwGc) <- d[,1]
    
  ## posterior: class x words, cols sum to 1
  PcGw <- colNorm(PwGc * outer(Pc, rep(1, ncol(PwGc))))
  
  ll <- list(call=call, PwGc=PwGc, Pc=Pc, PcGw=PcGw, data=list(x=x, y=y), 
             distribution=distribution, prior=prior, smooth=smooth)
  class(ll) <- c('naivebayes', class(ll))
  return(ll)
}

## make rows add up to one
rowNorm <- function(x) {
  x / outer(rowSums(x), rep(1, ncol(x)))	
}

## make cols add up to one 
colNorm <- function(x) {
  x / outer(rep(1, nrow(x)), colSums(x))
}


## does not check that we have the same set of words
## log.probs: would you like the class conditional sums of log prior + sum log word given class?
predict.naivebayes <- function(object, newdata=NULL, log.probs=FALSE, normalise=TRUE) {
  log.lik   <- t(log(object$PwGc))  
  log.prior <- log(object$Pc)
  if (is.null(newdata))
  	newdata <- object$data$x
  	
  D <- newdata %*% log.lik  ## D_ij = sum^V_w count_w-in-i log P(w-in-j | class=j) 
  lp <- D + outer(rep(1, nrow(newdata)), log.prior)
  if (log.probs)
  	return(lp) 
  else if (normalise)
    return(rowNorm(exp(lp))) 
  else return(exp(lp))
}




#predict.naiveBayesMulti <- function(nbfit) {
#  
#}


predict.NB.ken <- function (object, newdata, type = c("class", "raw"), threshold = 0.001, 
    ...) 
{
    type <- match.arg(type)
    newdata <- as.data.frame(newdata)
    attribs <- which(names(object$tables) %in% names(newdata))
    isnumeric <- sapply(newdata, is.numeric)
    newdata <- data.matrix(newdata)
    L <- sapply(1:nrow(newdata), function(i) {
        ndata <- newdata[i, ]
        L <- log(object$apriori) + apply(log(sapply(attribs, 
            function(v) {
                nd <- ndata[v]
                if (is.na(nd)) rep(1, length(object$apriori)) else {
                  prob <- if (isnumeric[v]) {
                    msd <- object$tables[[v]]
                    msd[, 2][msd[, 2] == 0] <- threshold
                    dnorm(nd, msd[, 1], msd[, 2])
                  } else object$tables[[v]][, nd]
                  prob[prob == 0] <- threshold
                  prob
                }
            })), 1, sum)
        if (type == "class") 
            L
        else {
            L <- exp(L)
            L/sum(L)
        }
    })
    if (type == "class") 
        factor(object$levels[apply(L, 2, which.max)], levels = object$levels)
    else t(L)
}


wordscore.1d <- function(x, refscores=c(-1,1), smooth=.001, scale="classic")
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
  # x <- x + smooth           # add one to all word counts
  # CANT DO THIS SINCE PRE-NORMALIZATION, IT MEANS THAT THE WEIGHT IS GREATER FOR SHORTER
  # VERSUS LONGER DOCUMENTS - EVEN WHEN BOTH WORDS ARE ZERO COUNT
  x <- x / apply(x,1,sum)    # normalize words to term frequencies
  if (scale=="classic") {
    p <- t(apply(x, 1, function(w) w/apply(x,2,sum)))  # Pr(this word | document)
    wordscores <- apply(p * refscores, 2, sum)
  } else if (scale=="logit") {
    x <- x + smooth
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
