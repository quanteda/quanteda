quanteda: Quantitative Analysis of Textual Data
===============================================

An R library for managing and analyzing text, by Ken Benoit and Paul Nulty.

How to Install
--------------

You can download the files and build the package from source, or you can use the devtools library to install the package directly from github.

This is done as follows:

```S
# required to install quanteda from Github
install.packages("devtools")
library(devtools)
# install the latest version quanteda from Github
install_github("quanteda", username="kbenoit")
# needed by quanteda - Will Lowe's austin package
if (!require(austin)) {
    install.packages("austin", repos="http://r-forge.r-project.org", type="source", dependencies=TRUE)
}
```
More Documentation
------------------

Is on its way.  An example for now, to create a term-frequency matrix from a collection of documents (what quanteda calls a corpus) and analyze that using a Naive Bayes classifier:

```S
#### Analyze Bollinger texts from Evans et al JELS 2007
# load in Amicus texts from a zipped web archive
amicusFile <- "http://www.kenbenoit.net/courses/tcd2014qta/exercises/amicus_curiae.zip"
download.file(amicusFile, basename(amicusFile))
unzip(basename(amicusFile))
# load in the texts to a vector of texts using quanteda's getTextDir()
amicus.texts <- c(getTextDir("./amicus/training"), getTextDir("./amicus/testing"))
# change the encoding (because texts contain special symbols such as §)
amicus.texts <- iconv(amicus.texts, from="latin1", to="UTF-8")
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
# compare the predicted class (rows) versus the actual class (columns)
table(amicus.nbp$docs$nb.predicted, amicus.corpus$attribs$testclass)
```
