#' Detect collocations in a text
#'
#' returns a list of collocations.  Note: Currently works only for pairs (bigram collocations).
#' 
#' @param text a text or vector of texts
#' @param file a filename containing a text
#' @param top threshold number for number of collocations to be returned (in descending order of association value)
#' @param distance distance between pairs of collocations
#' @param method association measure for detecting collocations
#' @return A list of collocations, their frequencies, and their test statistics
#' @export 
#' @author Kenneth Benoit
#' @examples
#' \dontrun{
#' data(iebudgets)
#' collocations(iebudgets$attribs$texts[1], top=50)
#' collocations(iebudgets$attribs$texts[1], top=50, method="chi2")
#' }
collocations <- function(text=NULL, file=NULL, top=NA, distance=2, n=2,
                         method=c("lr", "chi2", "mi")) {
  ## returns the bigrams, frequency, and score as a list
  ##
  method <- match.arg(method)
    if (is.null(text) & is.null(file)) stop("Must specify either text or file.")
  if (n>2) stop("Only bigrams (n=2) implemented so far.")
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
  names(returnval)[1] <- "collocation"
  # returns this as a (named) list
  return(as.list(returnval))
}

#' likelihood test for 2x2 tables
#'
#' returns a list of values
#' 
#' @param x a contingency table or matrix object
#' @return A list of return values
#' @export 
#' @author Kenneth Benoit
likelihood.test = function(x) {
  nrows = dim(x)[1]                      # no. of rows in contingency table
  ncols = dim(x)[2]                      # no. of cols in contingency table
  chi.out = chisq.test(x,correct=F)      # do a Pearson chi square test
  table = chi.out[[6]]                   # get the OFs
  ratios = chi.out[[6]]/chi.out[[7]]     # calculate OF/EF ratios
  sum = 0                                # storage for the test statistic
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      sum = sum + table[i,j]*log(ratios[i,j])
    }
  }
  sum = 2 * sum                          # the likelihood ratio chi square
  df = chi.out[[2]]                      # degrees of freedom
  p = 1 - pchisq(sum,df)                 # p-value
  out = c(sum, df, p, chi.out[[1]])      # the output vector
  names(out) = c("LRchi2","df","p-value","Pearschi2")
  return(as.list(out))                           # done!
}

