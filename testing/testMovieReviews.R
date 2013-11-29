library(quanteda)
library(austin)
source("/home/paul/Dropbox/code/quanteda/R/corpustools.R")
source("/home/paul/Dropbox/code/quanteda/R/languagetools.R")
texts <- getTextDir("~/Dropbox/QUANTESS/corpora/movieReviews/smaller/neg/")
vals <-vector()

vals[1:500] <- "neg"

atts <- data.frame(vals)
names(atts)<-c("lab")
movies <- corpus.create(texts[1:500], attribs=atts)

texts <- getTextDir("~/Dropbox/QUANTESS/corpora/movieReviews/smaller/pos/")
vals <-vector()
vals[1:500] <- "pos"

atts <- data.frame(vals)
names(atts)<-c("lab")

movies <- corpus.append(movies, texts[1:500], atts)


Rprof(append = FALSE)
toks <- create.fvm.new.corpus(movies)
Rprof(NULL)