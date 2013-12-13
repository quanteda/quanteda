library(quanteda)
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
iebudgets <- corpus.create(texts, attribs=newattribs)

texts <- getTextDir("~/Dropbox/QUANTESS/corpora/iebudgets/budget_2009sup")
parts <- strsplit(getRootFileNames(names(texts)), "_")
newattribs <-
  data.frame(matrix(unlist(parts), nrow=length(parts), byrow=TRUE))
names(newattribs) <- 
  c("year", "debate", "no", "fname", "speaker", "party")
newattribs$party <- gsub(".txt", "", newattribs$party)
iebudgets <- corpus.append(iebudgets, texts, newattribs)

texts <- getTextDir("~/Dropbox/QUANTESS/corpora/iebudgets/budget_2010")
parts <- strsplit(getRootFileNames(names(texts)), "_")
newattribs <-
  data.frame(matrix(unlist(parts), nrow=length(parts), byrow=TRUE))
names(newattribs) <- 
  c("year", "debate", "no", "fname", "speaker", "party")
newattribs$party <- gsub(".txt", "", newattribs$party)
iebudgets <- corpus.append(iebudgets, texts, newattribs)

texts <- getTextDir("~/Dropbox/QUANTESS/corpora/iebudgets/budget_2011")
parts <- strsplit(getRootFileNames(names(texts)), "_")
newattribs <-
  data.frame(matrix(unlist(parts), nrow=length(parts), byrow=TRUE))
names(newattribs) <- 
  c("year", "debate", "no", "fname", "speaker", "party")
newattribs$party <- gsub(".txt", "", newattribs$party)
iebudgets <- corpus.append(iebudgets, texts, newattribs)

texts <- getTextDir("~/Dropbox/QUANTESS/corpora/iebudgets/budget_2012")
parts <- strsplit(getRootFileNames(names(texts)), "_")
newattribs <-
  data.frame(matrix(unlist(parts), nrow=length(parts), byrow=TRUE))
names(newattribs) <- 
  c("year", "debate", "no", "fname", "speaker", "party")
newattribs$party <- gsub(".txt", "", newattribs$party)
iebudgets <- corpus.append(iebudgets, texts, newattribs)

Rprof(append = FALSE)
fvm <- create.fvm.corpus(iebudgets, group="party")
Rprof(NULL)

summaryRprof()
