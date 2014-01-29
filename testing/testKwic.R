library(quanteda)
library(austin)
library(ggplot2)
library(reshape2)

source("~/Dropbox/code/quanteda/R/languagetools.R")
source("~/Dropbox/code/quanteda/R/corpustools.R")

options(error=dump.frames)

texts <- getTextDir("~/Dropbox/QUANTESS/corpora/movieReviews/smaller/neg/")
times <- vector()
testSizes <- c(50,100)
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
  kwics <- kwic.corpus("great", movies)
  Rprof(NULL)
  times <- c(times, summaryRprof()$sampling.time)
}
df <- data.frame(testSizes, times)
results <- melt(data = df, id.vars = "testSizes", value.name="seconds")
ggplot(data = results, aes(x = testSizes , y = seconds, colour = variable)) + geom_line()
