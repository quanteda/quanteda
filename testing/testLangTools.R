# testing and optimization for common language functions

#source("~/Dropbox/code/quanteda/R/languagetools.R")



data(iebudgets)

tokenize_new.corpus <- function(c){
  c$attribs$texts <- sapply(c$attribs$texts, scan(what="char",  c$attribs$texts, quiet=TRUE))
}

tokenize_new <- function(s){
  tokens <-strsplit(s, "\\s+")[[1]]
  return(tokens)
}


passby <- function(x){
  x <- x+1
}

Rprof(append = TRUE)
#tokenString <- iebudgets
Rprof(NULL)