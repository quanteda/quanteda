#' This function makes a flattened dictionary from a Wordstat hierarchical
#' dictionary
#' @export
readWStatDict <- function(path){
  d <- read.delim(path, header=FALSE)
  d <- data.frame(lapply(d, as.character), stringsAsFactors=FALSE)
  thismajorcat <- d[1,1]
  # this loop fills in blank cells in the category|term dataframe
  for (i in 1:nrow(d)) {
    if (d[i,1] == "") {
      d[i,1] <- thismajorcat
    } else {
      thismajorcat <- d[i,1]
    }
    for(j in 1:ncol(d)){
      if(d[i,j] == "" & length(d[i,j-1])!=0){
        d[i,j] <- d[i,j-1] 
      }
    }
  }
  flatDict <- list()
  prevCateg <- ''
  curWords <- c()
  ns <- c()
  # this loop collapses the category cells together and
  # makes the list of named lists compatible with dfm
  for (i in 1:nrow(d)){
    categ <- unlist(paste(d[i,1:(ncol(d)-1)], collapse="."))
    if(categ != prevCateg){
      if(!is.null(curWords)){
        ns <- c(ns, categ)
        print(categ)
        flatDict <- c(flatDict, categ=list(curWords))
        curWords <- c()
      }
      prevCateg <- categ
    }else{
      w <- d[i,ncol(d)]
      w <- clean(unlist(strsplit(w, '\\('))[[1]])
      w <- gsub( " ", "", w)
      curWords<-c(curWords, w)
    }
  }
  names(flatDict)<-ns
  return(flatDict)
}




# old code:
# makes a list of lists from a two-level wordstat dictionary
readWStatDictNested <- function(path){
  lines <- readLines(path)
  allDicts=list()
  curDict=list()
  n <- list()
  for(i in 1:length(lines)){
    word <- unlist(strsplit(lines[i], '\\('))[[1]]
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




