#library(quanteda)
#library(austin)
source("/home/paul/Dropbox/code/quanteda/R/corpustools.R")
source("/home/paul/Dropbox/code/quanteda/R/languagetools.R")
texts <- getTextDir("~/Dropbox/QUANTESS/corpora/movieReviews/smaller/neg/")
vals <-vector()

vals[1:1000] <- "neg"

atts <- data.frame(vals)
names(atts)<-c("lab")
movies <- corpus.create(texts, attribs=atts)

texts <- getTextDir("~/Dropbox/QUANTESS/corpora/movieReviews/smaller/pos/")
vals <-vector()
vals[1:1000] <- "pos"

atts <- data.frame(vals)
names(atts)<-c("lab")
movies <- corpus.append(movies, texts, atts)

fvm <- create.fvm.corpus(movies, group="lab")
#datamat <-  wfm(fvm, word.margin=1)
#wf <- wordfish(datamat, dir=c(4,1), group=class)

