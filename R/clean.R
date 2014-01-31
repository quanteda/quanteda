clean <-
function(s, langNorm=FALSE, removeDigits=TRUE){
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
