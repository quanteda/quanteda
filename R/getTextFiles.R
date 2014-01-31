getTextFiles <-
function(filenames, textnames=NULL) {
  # TODO detect encoding; verbose=TRUE (progress bar?)
  print(filenames)
  textsvec <- c()  
  # changed from readChar to readLines
  for (f in filenames) {
    textsvec = c(textsvec, paste(readLines(f), collapse="\n"))
  }
  # name the vector with the filename by default, otherwise assign "names"
  ifelse(is.null(textnames), 
         names(textsvec) <- getRootFileNames(filenames),
         names(textsvec) <- textnames)
  return(textsvec)
}
