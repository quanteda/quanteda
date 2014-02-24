
# functions to parse the dictionaries that are in the wordstat .cat dictionary format, e.g.
# the Regressive Imagary Dictionary. Partially based on rid.py by John Wiseman
# https://github.com/wiseman/rid

countTabs <- function(line){
  i = 1
  while(i < nchar(line)){
    if(substr(line,i,i) != '\t')  return(i-1)
    i<-i+1
  }
}
# 
# allDicts=list()
# curDict=list()
# numTabs=0
# word <- ''
# for(i in 1:length(lines)){
#   lines= readLines('~/Dropbox/code/quanteda/data/LoughranMcDonald.cat')
#   prevWord <- word
#   word <- lines[i]
#   prevTabs <- numTabs
#   numTabs <- countTabs(word)
#   if(numTabs > prevTabs & grepl('\\(', word)){
#     allDicts<- c(allDicts, curDict)
#     curDict <- c()
#     assign(prevWord,curDict)
#     curDict <- c(curDict,word)
#   }
# }
