#' Truncate absolute filepaths to root filenames
#'
#' This function takes an absolute filepath and returns just the 
#' document name
#'
#' @param longFilenames Absolute filenames including a full path with directory
#' @return character vector of filenames withouth directory path
#' @export
#' @examples
#' getRootFileNames('/home/paul/documents/libdem09.txt')
getRootFileNames <- function(longFilenames) {
  ## function to return just the filename, path not included
  ## might need to detect .Platform$OS.type to change the delimiter
  delim <- "/"
  osName <- (Sys.info()[['sysname']] )
  if(osName=="Windows") { delim <- "\\\\" }
  splitFilenames <- strsplit(longFilenames, delim)
  return(sapply(splitFilenames, tail, n=1))
}


#' load text files from disk into a vector of character vectors

#' points to files, reads them into a character vector of the texts
#' with optional names, default being filenames
#' returns a named vector of complete, unedited texts
#' 
#' @param filenames
#' @return character vector of texts read from disk
#' @export
#' @examples
#' \dontrun{
#' getTextFiles('/home/paul/documents/libdem09.txt')
#' }
getTextFiles <- function(filenames, textnames=NULL) {
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


#' loads all text files from a given directory 
#'
#' given a directory name, get a list of all files in that directory
#' and load them into a character vector using getTextFiles
#' 
#' 
#' @param dirname A directory path
#' @return character vector of texts read from disk
#' @export
#' @examples
#' \dontrun{
#' getTextDir('/home/paul/documents/')
#' }
getTextDir <- function(dirname) {
  # get all files from a directory
  return(getTextFiles(list.files(dirname, full.names=TRUE)))
}


#' provides a gui interface to choose a gui to load texts from
#'
#' launches a GUI to allow the user to choose a directory from
#' which to load all files.
#' @return character vector of texts read from disk
#' @export
#' @examples
#' \dontrun{
#' getTextFiles('/home/paul/documents/libdem09.txt')
#' }
getTextDirGui <- function() {
  files <- choose.files()
  #get all files from a directory
  return(getTextFiles(files))
}
