
#' extract feature words

#' This function takes type of feature extractor and a word freaquency matrix
#' with binary class (1/0) to select features in class one. 'wsll' and
#' 'wschisq' replicates of 'Keyness' of Wordsmith Tools.
#' 
#' @param type of feature extractor
#' @param word frequency matrix
#' @param biarny class
#' @param smoothing constant
#' @param number of features shown
#' @return data frame of feature words
#' @author Kohei Watanabe
#' @export
#' @examples
#' texts <- getTextDir("/home/kohei/Documents/budget_2010/")
#' class  <- rep(0, length(texts))
#' class[grep("_LAB", names(texts))] <- 1
#' class[grep("_FF", names(texts))] <- 0
#' corpus <- corpusCreate(texts, attribs=list(class=class))
#' dfm <- dfm(corpus)
#' features <- selectFeatures('ll', dfm, corpus$attribs$class, smooth=1)
selectFeatures <- function(extractor, dfm, class, smooth=1, show=10){
 
  if(extractor == 'wsll' || extractor == 'wschisq'){
    smooth <- 0
  }
  df1 <- split(as.data.frame(dfm), class)['1']
  df0 <- split(as.data.frame(dfm), class)['0']
  
  d11 <- sapply(df1, colSums) + smooth
  d1. <- rep(sum(d11), nrow(d11))
  d10 <- d1. - d11 + smooth

  d01 <- sapply(df0, colSums) + smooth
  d0. <- rep(sum(d01), nrow(d01))
  d00 <- d0. - d01 + smooth
  
  d.1 <- d11 + d01
  d.0 <- d10 + d00
  d.. <- d.1 + d.0
  
  if(extractor == 'lr'){
    #Likelihood ratio
    p1 <- d11 / d1.
    p0 <- d01 / d0.
    lr <- p1 / p0
    df <- data.frame(cat1 = d11, cat0 = d01, score = lr[,1])
  }else if(extractor == 'll'){
    #Log-likelihood
    p1 <- d11 / d1.
    p0 <- d01 / d0.
    e1 <- d1. * (d.1 / d..)
    e0 <- d0. * (d.1 / d..)
    l1 <- d11 * log(d11 / e1)
    l2 <- d01 * log(d01 / e0)
    ll <- 2 * (l1 + l2)
    p <- 1 - pchisq(ll, 1, ncp = 0, log = FALSE)
    df <- data.frame(cat1 = d11, cat0 = d01, score = ll[,1], p = p[,1])
  }else if(extractor == 'wsll'){
    #WordSmith replication log-likelihood
    e1 <- d1. * (d.1 / d..)
    e0 <- d0. * (d.1 / d..)
    e1 <- ifelse(e1 == 0, 2.2e-16, e1)
    e0 <- ifelse(e0 == 0, 2.2e-16, e0)
    d11 <- ifelse(d11 == 0, 2.2e-16, d11)
    d01 <- ifelse(d01 == 0, 2.2e-16, d01)
    l1 <- d11 * log(d11 / e1)
    l2 <- d01 * log(d01 / e0)
    ll <- 2 * (l1 + l2)
    p <- 1 - pchisq(ll, 1, ncp = 0, log = FALSE)
    s <- ll * ifelse(d11 / d1. > d01 / d0., 1 , -1)
    df <- data.frame(cat1 = d11, cat0 = d01, score = s[,1], p = p[,1])
  }else if(extractor == 'wschisq'){
    #WordSmith replication Chi-square (with Yates's correction for continuity)
    chisq <- d.. * (abs((d11 * d00) - (d10 * d01)) - (d../2) ) ^ 2 / (d1. * d.1 * d0. * d.0)
    p <- 1 - pchisq(ll, 1, ncp = 0, log = FALSE)
    s <- chisq * ifelse(d11 / d1. > d01 / d0., 1 , -1)
    df <- data.frame(cat1 = d11, cat0 = d01, score = s[,1], p = p[,1])
  }
  if(show){
    print(head(df[order(-df$score),], show))
  }
  return(df)
}
  
