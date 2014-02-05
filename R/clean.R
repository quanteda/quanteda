#' Perform basic cleanup on a string

#' Simple cleanup for strings, removing punctuation, converting to lowercase
#' and optionally replacing some language-specific characters
#' 
#' @param s String to be cleaned
#' @return character vector in lowercase with punctuation (and optionally digits) removed
#' @export
#' @examples
#' s <-"test"
#' clean(s)
clean <- function(s, langNorm=FALSE, removeDigits=TRUE){
  # optionally do some language specific normalisation
  if(langNorm){
    # for French, make "l'" into "l"
    s <- gsub("l'", "l ", s)
    # make all "Eszett" characters in Hochdeutsche into "ss" as in Swiss German
    s <- gsub("ß", "ss", s)
  }
  if(removeDigits){
    #print("called")
    s <- gsub("[[:digit:][:punct:]]", "", s, perl = FALSE)
  }else{
    s <- gsub("[[:punct:]]", "", s, perl=TRUE)
  }
  s <- s[s!=""]  # remove empty strings
  s <- tolower(s) 
  return(s)
}