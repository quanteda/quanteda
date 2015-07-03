## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(quanteda)

## ------------------------------------------------------------------------
str(inaugTexts)  # this gives us some information about the object
myCorpus <- corpus(inaugTexts)  # build the corpus
summary(myCorpus, n=5)

## ------------------------------------------------------------------------
docvars(myCorpus, "President") <- substring(names(inaugTexts), 6)
docvars(myCorpus, "Year") <- as.integer(substring(names(inaugTexts), 1, 4))
summary(myCorpus, n=5)

## ------------------------------------------------------------------------
metadoc(myCorpus, "language") <- "english"
metadoc(myCorpus, "docsource")  <- paste("inaugTexts", 1:ndoc(myCorpus), sep="_")
summary(myCorpus, n=5, showmeta=TRUE)

## ------------------------------------------------------------------------
library(quanteda)
mycorpus1 <- corpus(inaugTexts[1:5], note="First five inaug speeches")
mycorpus2 <- corpus(inaugTexts[6:10], note="Next five inaug speeches")
mycorpus3 <- mycorpus1 + mycorpus2
summary(mycorpus3)

## ----eval=FALSE----------------------------------------------------------
#  # Basic file import from directory
#  d <- textfile('~/Dropbox/QUANTESS/corpora/inaugural/*.txt')
#  myCorpus <- corpus(d)

## ----eval=FALSE----------------------------------------------------------
#  # File import reading document variables from filenames
#  d <- textfile('~/Dropbox/QUANTESS/corpora/inaugural/*.txt')
#  
#  # In this example the format of the filenames is `Year-President.txt`.
#  # Because there are two variables in the filename, docvarnames must contain two names
#  myCorpus <- corpus(d, docvarsfrom="filenames", sep="-", docvarnames=c("Year", "President") )

## ----eval=FALSE----------------------------------------------------------
#  # These keys are examples and may not work! Get your own key at dev.twitter.com
#  consumer_key="vRLy03ef6OFAZB7oCL4jA"
#  consumer_secret="wWF35Lr1raBrPerVHSDyRftv8qB1H7ltV0T3Srb3s"
#  access_token="1577780816-wVbOZEED8KZs70PwJ2q5ld2w9CcvcZ2kC6gPnAo"
#  token_secret="IeC6iYlgUK9csWiP524Jb4UNM8RtQmHyetLi9NZrkJA"
#  
#  
#  tw <- getTweets('quantitative', numResults=20, consumer_key, consumer_secret, access_token, token_secret)

## ----eval=FALSE----------------------------------------------------------
#  twCorpus <- corpus(tw)
#  names(docvars(twCorpus))

