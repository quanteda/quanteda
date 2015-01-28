require(SnowballC, quietly=TRUE)

stemText <- function(text){
  tokens <- unlist(strsplit(text, ' '))
  text2 <- paste(wordStem(tokens), collapse = ' ')
  return(text2)
}
