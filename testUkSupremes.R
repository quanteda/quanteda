source("~/Dropbox/QUANTESS/alphaQuanteda/alphaQUANTEDA.R")
library(austin)

getParts <- function(casename){
  len <- nchar(casename)
  year <- substr(casename, 1, 4)
  x <- substr(casename,10,10)
  if(!is.na(as.numeric(x))) {
    judge <- substr(casename, 11, len)
    id <- substr(casename,5,11)
  }else{
    judge <- substr(casename,10,len)
    id <- substr(casename,5,9)
  }
  return(c(year, id, judge))
}

texts <- getTextDir("~/Dropbox/QUANTESS/corpora/UkSupreme/2011-12cases")
parts <- (sapply(names(texts), getParts))
parts <- t(parts)
newattribs <-
  data.frame((parts), nrow=length(parts))
names(newattribs) <- 
  c("year", "id","judge")
newattribs$judge <- gsub(".txt", "", newattribs$judge)

newattribs$judge[grep("[,&]",newattribs$judge)]<- "joint"

judges <- corpus.create(texts, attribs=newattribs)

fvm <- create.fvm.corpus(judges, group="judge")
datamat <-  wfm(fvm, word.margin=1)
wf <- wordfish(datamat, dir=c(4,1))