# read a two-level Wordstat dictionary
#
readWStatDict <- function(path){
  lines <- readLines(path)
  allDicts=list()
  curDict=list()
  n <- list()
  for(i in 1:length(lines)){
    word = lines[i]
    #if it doesn't start with a tab, it's a category
    if(substr(word,1,1) != "\t"){
      n <- c(n,word)
      if(length(curDict) >0) allDicts = c(allDicts, list(word=c(curDict)))
      curDict = list()
    }else{
      word <- gsub(' ','', word)
      curDict = c(curDict, gsub('\t','',(word)))
    } 
  }
  # add the last dicationary
  allDicts = c(allDicts, list(word=c(curDict)))
  names(allDicts) <- n
  return(allDicts)
}