corpus.add.attributes <-
function(corpus, newattribs, name=newattribs) {
  # attribs should be a named list of length(corpus$texts)
  # can be one or more variables
  newattribs <- as.data.frame(newattribs, stringsAsFactors=FALSE)
  names(newattribs) <- name
  corpus$attribs <- cbind(corpus$attribs, newattribs)
  return(corpus)
}
