bigrams <- function(text=NULL, file=NULL, top=NA, distance=2, method="lr") {
  ## returns the bigrams, frequency, and score as a list
  ##
  if (is.null(text) & is.null(file)) stop("Must specify either text or file.")
  clean.txt <- clean(text)
  t <- tokenize(clean.txt)
  bigrams <- paste(t[1:(length(t)-1)], t[2:length(t)])
  bigrams <- tolower(bigrams)
  bigrams.tokenized <- as.data.frame(table(bigrams), stringsAsFactors=FALSE)
  bigrams.tokenized <- bigrams.tokenized[order(bigrams.tokenized$bigrams), ]
  bigrams.tokenized$w1 <- sapply(strsplit(unclass(bigrams.tokenized$bigrams), " "), "[", 1)
  bigrams.tokenized$w2 <- sapply(strsplit(unclass(bigrams.tokenized$bigrams), " "), "[", 2)
  bigrams.tokenized$test <- NA
  require(entropy)
  options(warn=-1)
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
  # returns this as a (named) list
  return(as.list(returnval))
}
