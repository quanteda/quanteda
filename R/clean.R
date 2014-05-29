#' Perform basic cleanup on a character object
#'
#' Simple cleanup for strings, removing punctuation, converting to lowercase
#' and optionally replacing some language-specific characters
#' 
#' @param s character object to be cleaned
#' @param langNorm If true, French and German special characters are normalized.
#' @param removeDigits If true, digits are removed. Default is TRUE
#' @param lower If true, string is converted to lowercase. Default is TRUE
#' @return character object in lowercase with punctuation (and optionally digits) removed
#' @export
#' @examples
#' \dontrun{
#' s <- "A cursed £$&^!€ Exclamation! point; paragraph §1.2, which I wrote."
#' clean(s)
#' }
clean <- function(s, langNorm=FALSE, removeDigits=TRUE, lower=TRUE, removePunct=TRUE) {
  # optionally do some language specific normalisation
  if (langNorm) {
    # for French, make "l'" into "l"
    s <- gsub("l'", "l ", s)
    # make all "Eszett" characters in Hochdeutsche into "ss" as in Swiss German
    s <- gsub("ß", "ss", s)
  }
  if (removeDigits) {
    s <- gsub("[[:digit:]]", "", s, perl = FALSE)
  } 
  if (removePunct){
    s <- gsub("[[:punct:]]", "", s, perl=TRUE)
  }
  s <- s[s != ""]  
  if(lower){
    s <- tolower(s)
  }
  # replace multiple space padding with single space
  s <- gsub(" {2,}", " ", s)
  return(s)
}
