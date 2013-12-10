library(quanteda)
library(austin)
library(ggplot2)
library(reshape2)
library(plyr)
#devtools::install_github("dplyr")
#library(dplyr)
options(error=dump.frames)
source("/home/paul/Dropbox/code/quanteda/R/corpustools.R")
source("/home/paul/Dropbox/code/quanteda/R/languagetools.R")
texts <- getTextDir("~/Dropbox/QUANTESS/corpora/movieReviews/smaller/neg/")

plyTimes <- vector()
matTimes <- vector()
testSizes <- c(100, 2000)
insp<-matrix()
for (numDocs in testSizes){
  print(numDocs)
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
  fvm <- create.fvm.matrix.corpus(movies)
  Rprof(NULL)
  insp<-fvm
  matTimes <- c(matTimes, summaryRprof()$sampling.time)
  
   Rprof(append = FALSE)
   fvm <- create.fvm.plyr.corpus(movies)
   Rprof(NULL)
   plyTimes <- c(plyTimes, summaryRprof()$sampling.time)
}
df <- data.frame(testSizes, matTimes, plyTimes)
results <- melt(data = df, id.vars = "testSizes", value.name="seconds")
ggplot(data = results, aes(x = testSizes , y = seconds, colour = variable)) + geom_line()
