# Example that takes dictionary input from Wordstat dictionary file
#


lines= readLines('~/Dropbox/code/quanteda/examples/LoughranMcDonald.cat')

readWStatDict(path){
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
}