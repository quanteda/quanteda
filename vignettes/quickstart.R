## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----eval=FALSE----------------------------------------------------------
#  install.packages("quanteda")

## ----eval=FALSE----------------------------------------------------------
#  devtools::install_github("kbenoit/quanteda")

## ----eval=FALSE----------------------------------------------------------
#  ## devtools required to install quanteda from Github
#  devtools::install_github("kbenoit/quantedaData")

## ----show=FALSE----------------------------------------------------------
require(quanteda)

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

