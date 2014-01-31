getTextDir <-
function(dirname) {
  # get all files from a directory
  return(getTextFiles(list.files(dirname, full.names=TRUE)))
}
