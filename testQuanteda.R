source("~/Dropbox/QUANTESS/QUANTEDA/QUANTEDA_0.11.R")
#source("~/Dropbox/QUANTESS/alphaQuanteda/alphaQUANTEDA.R")
library(austin)
# make a list of text objects from a directory
print("making texts")
texts <- getTextDir("~/Dropbox/QUANTESS/corpora/iebudgets/budget_2008")
parts <- strsplit(getRootFileNames(names(texts)), "_")
newattribs <-
  data.frame(matrix(unlist(parts), nrow=length(parts), byrow=TRUE))
names(newattribs) <- 
  c("year", "debate", "no", "fname", "speaker", "party")
newattribs$party <- gsub(".txt", "", newattribs$party)
budgets <- corpus.create(texts, attribs=newattribs)


texts <- getTextDir("~/Dropbox/QUANTESS/corpora/iebudgets/budget_2009")
print(names(texts))
parts <- strsplit(getRootFileNames(names(texts)), "_")
if(2011 %in% parts){
  print(parts)
}

print(parts)
newattribs <-
  data.frame(matrix(unlist(parts), nrow=length(parts), byrow=TRUE))
names(newattribs) <- 
  c("year", "debate", "no", "fname", "speaker", "party")
newattribs$party <- gsub(".txt", "", newattribs$party)
temp <- corpus.create(texts,newattribs)
budgets$attribs <- rbind(budgets$attribs, temp$attribs)

texts <- getTextDir("~/Dropbox/QUANTESS/corpora/iebudgets/budget_2009sup")
print(names(texts))
parts <- strsplit(getRootFileNames(names(texts)), "_")
newattribs <-
  data.frame(matrix(unlist(parts), nrow=length(parts), byrow=TRUE))
names(newattribs) <- 
  c("year", "debate", "no", "fname", "speaker", "party")
newattribs$party <- gsub(".txt", "", newattribs$party)
budgets <- corpus.append(budgets, texts, newattribs)

texts <- getTextDir("~/Dropbox/QUANTESS/corpora/iebudgets/budget_2010")
print(names(texts))
parts <- strsplit(getRootFileNames(names(texts)), "_")
newattribs <-
  data.frame(matrix(unlist(parts), nrow=length(parts), byrow=TRUE))
names(newattribs) <- 
  c("year", "debate", "no", "fname", "speaker", "party")
newattribs$party <- gsub(".txt", "", newattribs$party)
budgets <- corpus.append(budgets, texts, newattribs)



texts <- getTextDir("~/Dropbox/QUANTESS/corpora/iebudgets/budget_2011")
print(names(texts))
parts <- strsplit(getRootFileNames(names(texts)), "_")
newattribs <-
  data.frame(matrix(unlist(parts), nrow=length(parts), byrow=TRUE))
names(newattribs) <- 
  c("year", "debate", "no", "fname", "speaker", "party")
newattribs$party <- gsub(".txt", "", newattribs$party)
budgets <- corpus.append(budgets, texts, newattribs)

texts <- getTextDir("~/Dropbox/QUANTESS/corpora/iebudgets/budget_2012")
print(names(texts))
parts <- strsplit(getRootFileNames(names(texts)), "_")
newattribs <-
  data.frame(matrix(unlist(parts), nrow=length(parts), byrow=TRUE))
names(newattribs) <- 
  c("year", "debate", "no", "fname", "speaker", "party")
newattribs$party <- gsub(".txt", "", newattribs$party)
budgets <- corpus.append(budgets, texts, newattribs)


fvm <- create.fvm.corpus(budgets, group="party")
datamat <-  wfm(fvm, word.margin=1)
wf <- wordfish(datamat, dir=c(5,1))