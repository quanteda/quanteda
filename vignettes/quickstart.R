## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----eval=FALSE----------------------------------------------------------
#  library(devtools)
#  if (!require(quanteda)) install_github("kbenoit/quanteda")

## ----eval=FALSE----------------------------------------------------------
#  # to install the latest dev branch version quanteda from Github use:
#  install_github("kbenoit/quanteda", dependencies=TRUE, quick=TRUE, ref="dev")

## ------------------------------------------------------------------------
# make sure quanteda is loaded and load the corpus of inaugural addresses
library(quanteda)
data(inaugCorpus)
summary(inaugCorpus, n=3)

## ----results='hide'------------------------------------------------------
tokenInfo <- summary(inaugCorpus)

## ------------------------------------------------------------------------
if (require(ggplot2))
    ggplot(data=tokenInfo, aes(x=Year, y=Tokens, group=1)) + geom_line() + geom_point() +
        scale_x_discrete(labels=c(seq(1789,2012,12)), breaks=seq(1789,2012,12) ) 


tokenInfo[which.max(tokenInfo$Tokens),] # Longest inaugural address: William Henry Harrison

## ------------------------------------------------------------------------

ttr <- tokenInfo$Types/tokenInfo$Tokens
if (require(ggplot2))
    ggplot(data=tokenInfo, aes(x=Year, y=ttr, group=1)) + geom_line() + geom_point() +
        scale_x_discrete(labels=c(seq(1789,2012,12)), breaks=seq(1789,2012,12) )

tokenInfo[which.max(ttr),]

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 200)
kwic(inaugCorpus, "terror")
kwic(inaugCorpus, "terror", wholeword=TRUE)
kwic(inaugCorpus, "communist")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# check the document-level variable names
names(docvars(inaugCorpus))

# list the first few values
head(docvars(inaugCorpus))

# check the corpus-level metadata
metacorpus(inaugCorpus)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(inaugTexts)
myCorpus <- corpus(inaugTexts)

## ----eval=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  # Basic file import from directory
#  d <- textfile('~/Dropbox/QUANTESS/corpora/inaugural/*.txt')
#  myCorpus <- corpus(d)
#  myCorpus

## ----eval=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  # File import reading document variables from filenames
#  d <- textfile('~/Dropbox/QUANTESS/corpora/inaugural/*.txt')
#  
#  # In this example the format of the filenames is `Year-President.txt`.
#  # Because there are two variables in the filename, docvarnames must contain two names
#  myCorpus <- corpus(d, docvarsfrom="filenames", sep="-", docvarnames=c("Year", "President") )

## ----eval=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  # These keys are examples and may not work! Get your own key at dev.twitter.com
#  consumer_key="vRLy03ef6OFAZB7oCL4jA"
#  consumer_secret="wWF35Lr1raBrPerVHSDyRftv8qB1H7ltV0T3Srb3s"
#  access_token="1577780816-wVbOZEED8KZs70PwJ2q5ld2w9CcvcZ2kC6gPnAo"
#  token_secret="IeC6iYlgUK9csWiP524Jb4UNM8RtQmHyetLi9NZrkJA"
#  
#  
#  tw <- getTweets('quantitative', numResults=20, consumer_key, consumer_secret, access_token, token_secret)

## ----eval=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  twCorpus <- corpus(tw)
#  names(docvars(twCorpus))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(inaugCorpus)
myCorpus <- subset(inaugCorpus, Year > 1990)

# make a dfm
myDfm <- dfm(myCorpus)
myDfm [,1:5]

# make a dfm, removing stopwords and applying stemming
myStemMat <- dfm(myCorpus, stopwords=TRUE, stem=TRUE)
myStemMat [,1:5]

## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(myStemMat)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
byPresMat <- dfm(myCorpus, groups=c('President'), stopwords=TRUE)
byPresMat[,1:5] # the counts here are sums of counts from speeches by the same President.

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(inaugCorpus)
recentCorpus <- subset(inaugCorpus, Year > 1991)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myDict<-list(terror=c("terrorism", "terrorists", "threat","a"),
             economy=c("jobs", "business", "grow","work"))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# I don't think this is working
byPresMat <- dfm(myCorpus, dictionary=myDict)

## ----warning=FALSE, message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(quanteda)
# create a corpus from the immigration texts from UK party platforms
uk2010immigCorpus <- corpus(ukimmigTexts,
                            docvars=data.frame(party=names(ukimmigTexts)),
                            notes="Immigration-related sections of 2010 UK party manifestos",
                            enc="UTF-8")
uk2010immigCorpus
summary(uk2010immigCorpus, showmeta=TRUE)

# key words in context for "deport", 3 words of context
kwic(uk2010immigCorpus, "deport", 3)

# create a dfm, removing stopwords
mydfm <- dfm(uk2010immigCorpus, stopwords=TRUE)
dim(mydfm)              # basic dimensions of the dfm
topfeatures(mydfm, 15)  # 15 top words
# if (Sys.info()["sysname"] == "Darwin") quartz(width=8, height=8)
plot(mydfm)             # word cloud  

