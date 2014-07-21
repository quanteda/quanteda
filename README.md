quanteda: Quantitative Analysis of Textual Data
===============================================

An R library for managing and analyzing text, by Ken Benoit and Paul Nulty.

How to Install
--------------

You can download the files and build the package from source, or you can use the devtools library to install the package directly from github.

This is done as follows:

```S
# devtools required to install quanteda from Github
if (!require(devtools)) install.packages("devtools")
library(devtools)
# install the latest (master) version quanteda from Github
install_github("quanteda", username="kbenoit", dependencies=TRUE, quick=TRUE)
# ALTERNATIVELY: install the latest dev branch version quanteda from Github
install_github("quanteda", username="kbenoit", ref="dev", quick=TRUE)
```
More Documentation
------------------

Is on its way.  An example for now, to create a term-frequency matrix from a collection of documents (what quanteda calls a corpus) and analyze that using a Naive Bayes classifier:

```S
library(quanteda)

#### Analyze Bollinger texts from Evans et al JELS 2007
# load in Amicus texts from a zipped web archive

# download and unzip texts
amicusFile <- "http://www.kenbenoit.net/courses/tcd2014qta/exercises/amicus_curiae.zip"
download.file(amicusFile, basename(amicusFile))
unzip(basename(amicusFile))

# load in the texts to a vector of texts using quanteda's getTextDir()
amicusTexts <- c(getTextDir("./amicus/training"), getTextDir("./amicus/testing"))
# change the encoding (because texts contain special symbols such as §)
amicusTexts <- iconv(amicusTexts, from="latin1", to="UTF-8")

# set training class
trainclass <- factor(c("P", "R", rep(NA, length(amicusTexts)-2)))

# set test class
testclass  <- rep(NA, length(amicusTexts))
testclass[grep("AP", names(amicusTexts))] <- "AP"
testclass[grep("AR", names(amicusTexts))] <- "AR"

# make a corpus object with texts and training and test labels
# note: formerly corpusCreate() !!
amicusCorpus <- 
  corpusCreate(amicusTexts, attribs=list(trainclass=trainclass, testclass=testclass))
summary(amicusCorpus)
# extract a word by document matrix
amicusDfm <- dfm(amicusCorpus)
# train the NB classifier 
amicusNb <- naiveBayesText(as.matrix(amicusDfm), amicusCorpus$attribs$trainclass, smooth=1, prior="uniform")
# predict test class
amicusNbp <- predict(amicusNb)
# compare the predicted class (rows) versus the actual class (columns)
table(amicusNbp$docs$nb.predicted, amicusCorpus$attribs$testclass)
```
