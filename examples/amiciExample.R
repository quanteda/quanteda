library(quanteda)
library(austin)

#testing rstudio commit
#testing 2 3

getLabel <- function(filename){
  len <- nchar(filename)
  second <- substr(filename,3,3)
  return(second)
}
trtexts <- getTextDir("~/Dropbox/QUANTESS/corpora/amicus/training")
labels <- sapply(names(trtexts), getLabel)
newattribs <-
  data.frame((labels), nrow=length(labels))
names(newattribs) <-  c("label")
trainingbriefs <- corpusCreate(trtexts, attribs=newattribs)
fvm <- dfm(trainingbriefs)
wfm <- as.wfm(fvm,word.margin=2)
ws <- classic.wordscores(wfm, scores=c(-1.0, -1.0, 1.0, 1.0))

print("done")
tetexts <- getTextDir("~/Dropbox/QUANTESS/corpora/amicus/testing")
tetexts <- sample(tetexts, length(tetexts),replace=FALSE, prob=NULL)
labels <- sapply(names(tetexts), getLabel)
newattribs <-
  data.frame((labels), nrow=length(labels))
names(newattribs) <-  c("label")
testingbriefs <- corpusCreate(tetexts, attribs=newattribs)
tefvm <- dfm(testingbriefs)
tewfm <- as.wfm(tefvm,word.margin=2)
resu<- predict(ws, newdata=tewfm)

resu$labels <- sapply(names(tetexts), getLabel)
resu$lf <- factor(labels)
resu$color[resu$lf=="R"] <- "red"
resu$color[resu$lf=="P"] <- "blue"

resu$pred[resu$Score > 0.0 ] <- "R"
resu$pred[resu$Score < 0.0 ] <- "P"

resu$acc[resu$lf==resu$pred] <- "correct"
resu$acc[resu$lf!=resu$pred] <- "incorrect"

#fvm <- create.fvm.corpus(judges, group="label")

