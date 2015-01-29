library(quanteda)
library(compiler)
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
Rprof(append = FALSE)
texts <- movies$attribs$texts
names(texts) <- rownames(movies$attribs)
tokenizedTexts <- sapply(texts, tokenize, simplify=TRUE)
tokens <- unlist(tokenizedTexts)
types <- unique(tokens)
dnames<-list(c(docs=names(texts)), c(words=types))
fvm <- matrix(0,nrow=length(texts), ncol=length(types), dimnames=dnames)
numTypes <- length(types)
i=1
while(i<=length(texts)){
  curTable = table(tokenizedTexts[i])
  curTypes <- names(curTable)
  temp <- as.matrix(curTable)
  curMatrix <- t(temp)
  tempfvm <- matrix(0,nrow=length(texts), ncol=length(types), dimnames=dnames)
  tempfvm <- as.matrix(rbind.fill(as.data.frame(tempfvm), as.data.frame(curMatrix)))
  j<-1
   while (j<=numTypes){
     if(types[j] %in% curTypes )
     {
       fvm[i,j]<- curTable[j]
     }
     j <- j+1
   }
  print(i)
  i <- i+1
}
fvm[is.na(fvm)] <-0      
Rprof(NULL)