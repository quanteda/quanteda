library(quanteda)
library(austin)
library(plyr)
library(ggplot2)
library(data.table)
library(reshape2)
options(error=dump.frames)
source("/home/paul/Dropbox/code/quanteda/R/corpustools.R")
source("/home/paul/Dropbox/code/quanteda/R/languagetools.R")
texts <- getTextDir("~/Dropbox/QUANTESS/corpora/movieReviews/smaller/neg/")
numDocs <- 200
vals <-vector()
vals[1:(numDocs/2)] <- "neg"
atts <- data.frame(vals)
names(atts)<-c("lab")
negTexts <- texts[1:(numDocs/2)]
movies <- corpus.create(negTexts, attribs=atts)

texts <- getTextDir("~/Dropbox/QUANTESS/corpora/movieReviews/smaller/pos/")
vals <-vector()
vals[1:(numDocs/2)] <- "pos"

atts <- data.frame(vals)
names(atts)<-c("lab")
posTexts <- texts[1:(numDocs/2)]
movies <- corpus.append(movies, posTexts, atts)

texts <- movies$attribs$texts
names(texts) <- rownames(movies$attribs)
tokenizedTexts <- sapply(texts, tokenize, simplify=TRUE)
print(names(tokenizedTexts))
tokens <- unlist(tokenizedTexts)
types <- unique(tokens)
dnames<-list(c(docs=names(texts)), c(words=types))
fvm <- matrix(0,nrow=length(texts), ncol=length(types), dimnames=dnames)
i=1
while(i<=length(texts)){
  curTable = table(tokenizedTexts[i])
  curTypes <- names(curTable)
  print(i)
  j<-1
  while(j<=length(types)){
    word <- types[j]
    fvm[i,j]<- curTable[word]
    j <- j+1
  }
  i <- i+1
}
fvm[is.na(fvm)] <-0             