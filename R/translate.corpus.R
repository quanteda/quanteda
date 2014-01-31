translate.corpus <-
function(corpus, targetlanguageString, 
                             textvar="texts", languagevar="language", key=NULL) {
  ## function to translate the text from a corpus into another language
  ## wrapper for translate
  # initialize the translated text vector
  if (is.null(key)) stop("Error: Must supply a key for Google Translate.")
  translatedTextVector <- rep(NA, nrow(corpus$attribs))
  for (i in 1:nrow(corpus$attribs)) {
    if (corpus$attribs[i,textvar]=="" | is.na(corpus$attribs[i,textvar])) next
    if (corpus$attribs[i,languagevar]==targetlanguageString) next
    translatedTextVector[i] <- translate(corpus$attribs[i,textvar], 
                                         corpus$attribs[i,languagevar],
                                         targetlanguageString, key)
  }
  return(translatedTextVector)
}
