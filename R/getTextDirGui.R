getTextDirGui <-
function() {
  files <- choose.files()
  #get all files from a directory
  return(getTextFiles(files))
}
