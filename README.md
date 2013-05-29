quanteda: Quantitative Analysis of Textual Data
===============================================

An R library for managing and analyzing text, by Ken Benoit and Paul Nulty.

How to Install
--------------

You can download the files and build the package from source, or you can use the devtools library to install the package directly from github.

This is done as follows:

```S
install.packages("devtools")
library(devtools)
install_github("quanteda", username="kbenoit", dependencies=TRUE)
```
More Documentation
------------------

Is on its way.  An example for now, to create a term-frequency matrix from a collection of documents (what quanteda calls a corpus) and analyze that using a Naive Bayes model:

```S
#### Analyze Bollinger texts from Evans et al JELS 2007
# load in Amicus texts
amicus.texts <- c(getTextDir("~/texts/amicus/training"),
                  getTextDir("~/texts/amicus/testing"))
# set training class
trainclass <- factor(c("P", "R", rep(NA, length(amicus.texts)-2)))
# set test class
testclass  <- rep(NA, length(amicus.texts))
testclass[grep("AP", names(amicus.texts))] <- "AP"
testclass[grep("AR", names(amicus.texts))] <- "AR"
amicus.corpus <- 
  corpus.create(amicus.texts, attribs=list(trainclass=trainclass, testclass=testclass))
summary(amicus.corpus)
# extract a word by document matrix
amicus.fvm <- create.fvm.corpus(amicus.corpus)
# train the NB classifier 
amicus.nb <- naiveBayesText(t(amicus.fvm), amicus.corpus$attribs$trainclass, smooth=1, prior="uniform")
# predict test class
amicus.nbp <- predict(amicus.nb)
```
