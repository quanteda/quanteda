source('~/quanteda/R/kohei_tokenize2.R')

#' Get lemmas
#' This depends on Kohei's tokenizer
#' 
#' @param texts
#' @return lemmatized texts
#' @export
#' @examples
#' data(ieTexts)
#' texts <- cleanText(ieTexts, lower=TRUE)
#' lemmas <- lemmatizeTexts(texts)
lemmatizeTexts <- function(texts){
  
  if(file.exists("lemmas_EN.RData")){
    load("lemmas_EN.RData")
  }else{
    lemmas_EN <- createLemmaIndex()
    save(lemmas_EN, file="lemmas_EN.RData")
  }
  texts2 <- unlist(lapply(texts, function(x) findLemmas(x, lemmas_EN)))
  return(texts2)
  
}

findLemmas <- function(text, index){
  tokens <- tokenizeText(text, clean = FALSE)
  tokens2 <- sapply(tokens, function(x) ifelse(x == '', x, ifelse(!is.null(index[[x]]), index[[x]], x)))
  text2 <- paste(tokens2, collapse = ' ')
  return(text2)
}

createLemmaIndex <- function(){
  #Based on Someya's (1998) e-lemma 
  #http://www.lexically.net/downloads/BNC_wordlists/e_lemma.txt.
  lemma.df <- read.delim('/home/kohei/Documents/R/lemma.tsv', quote = "", na.strings = "", stringsAsFactors = FALSE, fileEncoding = 'UTF-8') 
  lemmas <- unlist(rownames(lemma.df), use.names=FALSE)
  forms <- unlist(sapply(lemma.df['forms'], function(x) strsplit(x, ',', fixed = TRUE), simplify=FALSE), recursive = FALSE)
  lemmas.all <- rep(lemmas, sapply(forms, length))
  forms.all <- unlist(forms, use.names=FALSE)
  
  index <- new.env(hash = TRUE, parent = emptyenv(), size = length(forms.all))
  for(i in 1:length(forms.all)){
    index[[forms.all[i]]] <- lemmas.all[i]
  }
  return(index)
}
