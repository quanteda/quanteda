removeStopwords <-
function(text, stopwords=NULL){
  if(is.null(stopwords)) data('stopwords_EN')
  tokens <- tokenize(text)
  print(tokens)
  newTokens <- (subset(tokens, !tokens %in% stopwords_EN))
  newString <- paste(newTokens, sep=" ")
  return(newString)
}
