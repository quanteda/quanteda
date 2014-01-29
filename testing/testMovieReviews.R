library(quanteda)

#source("~/Dropbox/code/quanteda/R/languagetools.R")
source("~/Dropbox/code/quanteda/R/corpustools.R")

neg_texts <- getTextDir("~/Dropbox/QUANTESS/corpora/movieReviews/smaller/neg/")
pos_texts <- getTextDir("~/Dropbox/QUANTESS/corpora/movieReviews/smaller/pos/")
texts <- c(neg_texts,pos_texts)
vals <-vector()
vals[1:1000] <- "neg"
vals[1001:2000] <- "pos"
atts <- data.frame(vals)
names(atts)<-c("label")
movies <- corpus.create(texts, attribs=atts)

contexts <- kwic.corpus("great",movies)

#fvm <- create.fvm.corpus(movies, group="label")