source('~/quanteda/R/kohei_tokenize2.R')

#' Alternative to DFM
#' This depends on Kohei's tokenizer
#' An attempt to make DFM simple and more customizeable
#' Do not use corpus objects for simple usage, expecially for those who store texts in data frames
#' 
#' @param texts
#' @param class class of texts
#' @param limit mininum frequency of tokens
#' @return token by class frequency matrix
#' @export
#' @examples
#' data(ieTexts)
#' #Example1: Only clean text
#' tfmx <- getTFMX(ieTexts, names(ieTexts), cleanText)
#' 
#' #Example2: Add more preprocessing as unnamed function
#' tfmx2 <- getTFMX(ieTexts, names(ieTexts), function(x){
#'                                                  t <- cleanText(x, lower=TRUE)
#'                                                  t <- removeStopwords(t)
#'                                                  b <- bigrams(t, window = 5)
#'                                                  return(b)
#'                                                  })
#'                                                  
#' #Example3: Create a custom tokinizer
#' customTokenizer <- function(x){
#'    t <- cleanText(x, lower=TRUE, min = 2)
#'    t <- removeStopwords(t)
#'    b <- ngrams(t, n = 3)
#'    return(b)
#' }
#' tfmx3 <- getTFMX(ieTexts, names(ieTexts), customTokenizer, limit = 3)
#' 
getTFMX <- function(texts, class = NULL, tokenizer = NULL, limit = 0){
  if(is.null(class)) class <- rep('all', length(texts))
  if(length(texts) != length(class)) stop('Vector lengths are different')
  texts <- texts[!is.na(class)]
  class <- class[!is.na(class)]
  
  if(is.null(tokenizer)) tokenizer <- tokenizeText
  tokens <- c()
  tokens.class <- c()
  
  texts2 <- sapply(texts, tokenizer, simplify=FALSE)
  class.all <- rep(class, sapply(texts2, length))
  tokens.all <- unlist(texts2, use.names=FALSE)
  
  tfmx <- as.data.frame.matrix(table(class.all, tokens.all))
  tfmx2  <- t(subset(t(tfmx), colSums(tfmx) >= limit))
  #View(tfmx)
  return(tfmx2)
}
