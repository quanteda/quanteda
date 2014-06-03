#' remove common or 'semantically empty' words from a text.
#'
#' This function takes a character vector 'text' and removes words in the
#' list provided in 'stopwords'. If no list of stopwords is provided a 
#' default list for English is used.
#' 
#' @param text Text to alter
#' @param kind A stopword dictionary from the stopwords directory (e.g. 'italian')
#' @param stopwords List of stopwords to remove
#' @return a character vector of text with stopwords removed
#' @export
#' @examples
#' someText <- "Here is an example of text containing some stopwords we want to remove. "
#' itText <- "Ecco un esempio di testo contenente alcune parole non significative che vogliamo rimuovere."
#' removeStopwords(someText)
#' removeStopwords(someText, kind="italian")
#' removeStopwords(someText, stopwords = c("containing", "example"))
removeStopwords <- function(text, kind='english', stopwords=NULL){
  curStopwords <- list()
  if (is.null(stopwords)) {
      data(stopwords,envir = environment())
      if (!(kind %in% names(stopwords))){
        message <- sprintf("%s was not found in the list of stopwords", kind)
        stop(message)
      }
      curStopwords <- stopwords[[kind]]
  }else{
    curStopwords <- stopwords
  }
  tokens <- tokenize(text, lower=FALSE)
  newTokens <- tokens[which(!(tolower(tokens) %in% tolower(curStopwords)))]
  newString <- paste(newTokens, collapse=" ")
  return(newString)
}
