# testing and optimization for common language functions

#source("~/Dropbox/code/quanteda/R/languagetools.R")



library(quanteda)

# remove punctuation, make everything lower case.
clean <- function(s, langNorm=FALSE){
  s <- gsub("[[:punct:]]", "", s)
  s <- tolower(s)
  # optionally do some language specific normalisation
  if(langNorm){
    # for French, make "l'" into "l"
    s <- gsub("l'", "l ", s)
    # make all "Eszett" characters in Hochdeutsche into "ss" as in Swiss German
    s <- gsub("ß", "ss", s)
  }
  return(s)
  
}

tokenize_new1 <- function(s){
  s <- clean(s)
  tokens <-strsplit(s, split="\\s+")[[1]]
  return(tokens)
}

tokenize_new2 <- function(s){
  s <- clean(s)
  tokenized.txt <- scan(what="char", text=s, quiet=TRUE)
  tokenized.txt <- tokenized.txt[tokenized.txt!=""]
  return(tokenized.tx)
}

data(iebudgets)
Rprof(append = FALSE)
tokenStrings <- iebudgets$attribs$texts
words <- sapply(tokenStrings, tokenize_new2)


Rprof(NULL)
