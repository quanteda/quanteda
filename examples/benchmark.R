library(quanteda)
library(austin)
library(ggplot2)
library(reshape2)

source("~/Dropbox/code/quanteda/R/languagetools.R")
source("~/Dropbox/code/quanteda/R/corpustools.R")

options(error=dump.frames)

texts <- getTextDir("~/Dropbox/QUANTESS/corpora/movieReviews/smaller/neg/")
oldTimes <- vector()
newTimes <- vector()
testSizes <- c(500, 800)
for (numDocs in testSizes){
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
  toks <- create.fvm.corpus(movies)
  Rprof(NULL)
  oldTimes <- c(oldTimes, summaryRprof()$sampling.time)
  
  Rprof(append = FALSE)
  toks <- create.fvm.corpus(movies)
  Rprof(NULL)
  newTimes <- c(newTimes, summaryRprof()$sampling.time)
}
df <- data.frame(testSizes, oldTimes, newTimes)
results <- melt(data = df, id.vars = "testSizes", value.name="seconds")
ggplot(data = results, aes(x = testSizes , y = seconds, colour = variable)) + geom_line()
