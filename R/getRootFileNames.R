getRootFileNames <-
function(longFilenames) {
  ## function to return just the filename, path not included
  ## might need to detect .Platform$OS.type to change the delimiter
  delim <- "/"
  osName <- (Sys.info()[['sysname']] )
  if(osName=="Windows") { delim <- "\\\\" }
  
  splitFilenames <- strsplit(longFilenames, delim)
  return(sapply(splitFilenames, tail, n=1))
}
