#' Detect collocations in a text
#'
#' returns a list of collocations.  Note: Currently works only for pairs (bigram collocations).
#' 
#' @param text a text or vector of texts
#' @param file a filename containing a text
#' @param top threshold number for number of collocations to be returned (in descending order of association value)
#' @param distance distance between pairs of collocations
#' @param method association measure for detecting collocations
#' @param n Only bigrams (n=2) implemented so far.
#' @return A list of collocations, their frequencies, and their test statistics
#' @export 
#' @author Kenneth Benoit
#' @examples
#' collocations(texts(inaugCorpus)[1], top=50)
#' collocations(texts(inaugCorpus)[1], top=50, method="chi2")
collocations <- function(text=NULL, file=NULL, top=NA, distance=2, n=2,
                         method=c("lr", "chi2", "mi")) {
  ## returns the bigrams, frequency, and score as a list
  ##
  method <- match.arg(method)
    if (is.null(text) & is.null(file)) stop("Must specify either text or file.")
  if (n>2) stop("Only bigrams (n=2) implemented so far.")
  clean.txt <- clean(text)
  t <- unlist(tokenize(clean.txt))
  # create a vector of all adjacent bigrams delimited by a space
  bigrams <- paste(t[1:(length(t)-1)], t[2:length(t)])
  # redundantly make them lower case  
  bigrams <- tolower(bigrams)
  # tabulate the bigrams and coerce to a data.frame
  bigrams.tokenized <- as.data.frame(table(bigrams), stringsAsFactors=FALSE)
  # alphabetize the bigrams (why??)
  bigrams.tokenized <- bigrams.tokenized[order(bigrams.tokenized$bigrams), ]
  # separate word 1 from word 2
  bigrams.tokenized$w1 <- sapply(strsplit(unclass(bigrams.tokenized$bigrams), " "), "[", 1)
  bigrams.tokenized$w2 <- sapply(strsplit(unclass(bigrams.tokenized$bigrams), " "), "[", 2)
  bigrams.tokenized$test <- NA   # create an empty vector for test results
  options(warn=-1)               # turn off the warnings
  if (method=="lr") {
    for (i in 1:nrow(bigrams.tokenized)) {
      bigrams.tokenized$test[i] <-
        likelihood.test(table(bigrams.tokenized$w1==bigrams.tokenized$w1[i], bigrams.tokenized$w2==bigrams.tokenized$w2[i]))$LRchi2
    }
  } else if (method=="chi2") {
    for (i in 1:nrow(bigrams.tokenized)) {
      bigrams.tokenized$test[i] <-
        chisq.test(table(bigrams.tokenized$w1==bigrams.tokenized$w1[i], bigrams.tokenized$w2==bigrams.tokenized$w2[i]), correct=FALSE)$statistic
    }
  } else if (method=="mi") {
      require(entropy)
      for (i in 1:nrow(bigrams.tokenized)) {
      bigrams.tokenized$test[i] <-
        entropy(table(bigrams.tokenized$w1==bigrams.tokenized$w1[i])) + entropy(table(bigrams.tokenized$w2==bigrams.tokenized$w2[i])) -
        entropy(table(bigrams.tokenized$w1==bigrams.tokenized$w1[i], bigrams.tokenized$w2==bigrams.tokenized$w2[i]))
    }
  } else stop("method must be from: lr, chi2, or mi")
  options(warn=0)
  bigrams.tokenized <- bigrams.tokenized[order(bigrams.tokenized$test, decreasing=TRUE),]
  if (is.na(top)) top <- nrow(bigrams.tokenized)
  returnval <- bigrams.tokenized[1:top, c("bigrams", "Freq", "test")]
  # rename the statistic as the test
  names(returnval)[3] <- method
  names(returnval)[1] <- "collocation"
  # returns this as a (named) list
  return(as.data.frame(returnval))
}

#' likelihood test for contingency tables
#'
#' returns a list of values
#' 
#' @param x a contingency table or matrix object
#' @return A list of return values
#' @export 
#' @author Kenneth Benoit
likelihood.test = function(x) {
    epsilon <- .000000001  # to offset zero cell counts
    chi.out <- suppressWarnings(chisq.test(x, correct=F))      # do a Pearson chi square test
    lrratio <- 2 * sum(chi.out$observed * log(chi.out$observed / chi.out$expected + epsilon))
    p <- 1 - pchisq(lrratio, chi.out$parameter)
    list(LRchi2=lrratio, df=chi.out$parameter, p=p)
}



#' Fast method to detect collocations in a text
#'
#' Detects collocation bigrams using a fast vectorized/indexed method.
#' @param text a text or vector of texts
#' @param method association measure for detecting collocations
#' @param n Only bigrams (n=2) implemented so far.
#' @return A list of collocations, their frequencies, and the G^2 likelihood ratio Chi^2 
#' statistic.
#' @export 
#' @author Kenneth Benoit
#' @examples
#' collocations(texts(inaugCorpus)[1], top=50)
#' collocations(texts(inaugCorpus)[1], top=50, method="chi2")
collocations2 <- function(text=NULL, n=2,
                          method=c("lr", "chi2", "mi")) {
    ## returns the bigrams, frequency, and score as a list
    ##
    method <- match.arg(method)
    if (is.null(text) & is.null(file)) stop("Must specify either text or file.")
    if (n>2) stop("Only bigrams (n=2) implemented so far.")

    clean.txt <- clean(text)
    t <- unlist(tokenize(clean.txt))
    
    # create a data.table of all adjacent bigrams
    require(data.table)
    wordpairs <- data.table(w1 = t[1:(length(t)-1)], 
                            w2 = t[2:length(t)], 
                            count = 1)

    # set the data.table sort key
    setkey(wordpairs, w1, w2)
    
    # tabulate (count) w1 w2 pairs
    wordpairsTable <- wordpairs[, sum(count), by="w1,w2"]
    setnames(wordpairsTable, "V1", "w1w2n")

    # tabulate all word marginal counts
    w1Table <- wordpairs[, sum(count), by=w1]
    setnames(w1Table, "V1", "w1n")
    setkey(w1Table, w1)
    
    setkey(wordpairsTable, w1)
    suppressWarnings(allTable <- wordpairsTable[w1Table])
    # otherwise gives an encoding warning
    
    # tabulate all w2 counts
    w2Table <- wordpairs[, sum(count), by=w2]
    setnames(w2Table, "V1", "w2n")
    setkey(w2Table, w2)
    
    setkey(allTable, w2)
    suppressWarnings(allTable2 <- allTable[w2Table])
    # otherwise gives an encoding warning
    
    setkey(allTable2, w1, w2)
    
    N <- allTable2[, sum(w2n, na.rm=TRUE)]
    
    # fill in cells of 2x2 tables
    allTable2$w1notw2 <- allTable2$w1n - allTable2$w1w2
    allTable2$notw1w2 <- allTable2$w2n - allTable2$w1w2
    allTable2$notw1notw2 <- N - (allTable2$w1w2 + allTable2$w1notw2 + allTable2$notw1w2)
    
    # calculate expected values
    allTable2$w1w2Exp <- allTable2$w1n * allTable2$w2n / N
    allTable2$w1notw2Exp <- allTable2$w1n * (N - allTable2$w2n) / N
    allTable2$notw1w2Exp <- allTable2$w2n * (N - allTable2$w1n) / N
    allTable2$notw1notw2Exp <- (N - allTable2$w2n) * (N - allTable2$w1n) / N

    # vectorized lr stat
    epsilon <- .000000001  # to offset zero cell counts
    attach(allTable2)
    allTable2$lrratio <- 2 *  ((w1w2n * log(w1w2n / w1w2Exp + epsilon)) +
                               (w1notw2 * log(w1notw2 / w1notw2Exp + epsilon)) +
                               (notw1w2 * log(notw1w2 / notw1w2Exp + epsilon)) +
                               (notw1notw2 * log(notw1notw2 / notw1notw2Exp + epsilon)))
    detach(allTable2)    
    
    allTable2 <- allTable2[order(-lrratio)]
    
    return(data.frame(collocation=paste(allTable2$w1, allTable2$w2),
                      count=allTable2$w1w2n,
                      G2=allTable2$lrratio))
}


