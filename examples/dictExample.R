# Example that takes dictionary input from Wordstat dictionary file
#


lines= readLines('~/Dropbox/code/quanteda/examples/LoughranMcDonald.cat')

allDicts=list()
curDict=list()
n <- list()
for(i in 1:length(lines)){
  word = lines[i]
  #if it doesn't start with a tab, it's a category
  if(substr(word,1,1) != "\t"){
    n <- c(n,word)
    allDicts = c(allDicts, list(word=c(curDict)))
    print(allDicts)
    curDict = list()
  }else{
    word <- gsub(' ','', word)
    curDict = c(curDict, gsub('\t','',clean(word)))
  } 
}
names(allDicts) <- n