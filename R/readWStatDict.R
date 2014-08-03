#' Make a flattened list from a hierarchical wordstat dictionary
#'
#' Make a flattened list from a hierarchical wordstat dictionary
#' 
#' @param path path to the wordstat dictionary file
#' @return flattened dictionary as a list
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
    for(j in 1:(ncol(d)-1)){
      if(d[i,j] == "" & length(d[i-1,j])!=0){
        d[i,j] <- d[i-1,j] 
      }
    }
    if (nchar(d[i,ncol(d)-1]) > 0){
      pat<- c("\\(")
      if( !length(grep(pat,  d[i,ncol(d)-1] ) )==0 ){
        d[i,ncol(d)] <- d[i,ncol(d)-1]
        d[i,ncol(d)-1] <- "_"
      }
    }
  }
  flatDict <- list()
  categ <- list()
  # this loop collapses the category cells together and
  # makes the list of named lists compatible with dfm
  for (i in 1:nrow(d)){
    if( d[i,ncol(d)]=='') next
    categ <- unlist(paste(d[i,(1:(ncol(d)-1))], collapse="."))
    w <- d[i,ncol(d)]
    w <- clean(unlist(strsplit(w, '\\('))[[1]])
    w <- gsub( " ", "", w)
    flatDict[[categ]] <- append(flatDict[[categ]],c(w))
  }
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




