#' Perform basic cleanup on a character object
#'
#' Simple cleanup for strings, removing punctuation, converting to lowercase
#' and optionally replacing some language-specific characters
#' 
#' @param s character object to be cleaned
#' @return character object in lowercase with punctuation (and optionally digits) removed
#' @export
#' @examples
#' s <- "A cursed £$&^!€ Exclamation! point; paragraph §1.2, which I wrote."
#' clean(s)
clean <- function(s, langNorm=FALSE, removeDigits=TRUE) {
  # optionally do some language specific normalisation
  if (langNorm) {
    # for French, make "l'" into "l"
    s <- gsub("l'", "l ", s)
    # make all "Eszett" characters in Hochdeutsche into "ss" as in Swiss German
    s <- gsub("ß", "ss", s)
  }
  if (removeDigits) {
    s <- gsub("[[:digit:][:punct:]]", "", s, perl = FALSE)
  } else {
    s <- gsub("[[:punct:]]", "", s, perl=TRUE)
  }
  s <- s[s != ""]  # remove empty strings
  s <- tolower(s) 
  # replace multiple space padding with single space
  s <- gsub(" {2,}", " ", s)
  return(s)
}
