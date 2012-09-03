getTextFiles <-
function(filenames, textnames=NULL, verbose=FALSE) {
  # points to files, reads them into a character vector of the texts
  # with optional names, default being filenames
  # return a named vector of complete, unedited texts
  # TODO detect encoding; verbose=TRUE (progress bar?)
  textsvec <- c()   # initialize text vector
  # changed from readChar to readLines
  for (f in filenames) {
    textsvec = c(textsvec, paste(readLines(file(f)), collapse="\n")) 
  }
  # name the vector with the filename by default, otherwise assign "names"
  ifelse(is.null(textnames), 
         names(textsvec) <- getRootFileNames(filenames),
         names(textsvec) <- textnames)
  return(textsvec)
}
