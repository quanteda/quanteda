library(quanteda)
library(austin)
# the classifier functions to soon be incoporated into QUANTEDA
source('~/Dropbox/QUANTESS/Ken local code/quanteda/R/classification.R')


getLabel <- function(filename){
  len <- nchar(filename)
  second <- substr(filename,3,3)
  return(second)
}

trtexts <- getTextDir("~/Dropbox/QUANTESS/corpora/amicus curae/training")
labels <- sapply(names(trtexts), getLabel)
newattribs <-
  data.frame((labels), nrow=length(labels))
names(newattribs) <-  c("label")
trainingbriefs <- corpus.create(trtexts, attribs=newattribs)
fvm <- create.fvm.corpus(trainingbriefs)
wfm <- as.wfm(fvm,word.margin=1)
wfm <- t(wfm)




print("finished")

tetexts <- getTextDir("~/Dropbox/QUANTESS/corpora/amicus curae/testing")
tetexts <- sample(tetexts, length(tetexts),replace=FALSE, prob=NULL)
labels <- sapply(names(tetexts), getLabel)
newattribs <-
  data.frame((labels), nrow=length(labels))
names(newattribs) <-  c("label")
testingbriefs <- corpus.create(tetexts, attribs=newattribs)
tefvm <- create.fvm.corpus(testingbriefs)
tewfm <- (as.wfm(tefvm,word.margin=1))
tewfm <- t(tewfm)
tewfm <- tewfm[,colnames(tewfm)%in% colnames(wfm)]

wfm <- wfm[,colnames(wfm)%in% colnames(tewfm)]

test <- c()
length(test) <- 98
for (i in 98) {
  test[[i]] <- NA
}

trainingclass <- c(c("P", "P", "R", "R"), test)
trainingclass <- factor(trainingclass, ordered=TRUE)
## multinomial NB model
totwfm <- rbind(wfm,tewfm)
nb.ami <- naiveBayesText(totwfm, trainingclass, smooth=1, prior="docfreq")
resu<-predict(nb.ami, newdata=tewfm, log.probs=TRUE, normalise=FALSE)
resu <- as.data.frame(resu)
resu$score <- resu$R - resu$P
resu$lf <- factor(labels)
resu$color[resu$lf=="R"] <- "red"
resu$color[resu$lf=="P"] <- "blue"

resu$pred[resu$score > 0.0 ] <- "R"
resu$pred[resu$score < 0.0 ] <- "P"

resu$acc[resu$lf==resu$pred] <- "correct"
resu$acc[resu$lf!=resu$pred] <- "incorrect"



