source('~/quanteda/R/kohei_tokenize2.R')

#' Identify name entities
#' This depends on Kohei's tokenizer
#' Needs a lot of texts for minor name entities
#' 
#' @param texts
#' @param cluster allows to use multi-threding to speed up 
#' @return texts in which names are capitalized
#' @export
#' @examples
#' data(ieTexts)
#' texts <- identifyNames(ieTexts)
#' names <- extractNames(texts)
#Identify names and capitalize names in texts
identifyNames <- function(texts, cluster = 0){
  #start_time = proc.time()["elapsed"]
  if(cluster > 0){
    library(snow)
    cl <- makeCluster(cluster, type = "SOCK") 
    clusterExport(cl, "tokenizeText") 
    clusterExport(cl, "cleanText") 
  }
  
  cat('Cleaning text... ')
  texts2 <- lapply(texts, function(x) cleanText(x))
  
  cat('Creating index of multi-part names... ')
  MPNameIndex <- createMPNameIndex(texts2, limit = 1000)
  
  cat('Concatnating multi-part names... ')
  if(cluster > 0){
    texts3 <-  unlist(clusterApply(cl, texts2, joinMPNames, MPNameIndex))
  }else{
    texts3 <- lapply(texts2, function(x) joinMPNames(x, MPNameIndex))
  }
  
  cat('Creating index of names... ')
  nameIndex <- createNameIndex(texts3)
  nameIndex2 <- setdiff(nameIndex, createDateIndex())
  
  cat('Capitalizing names... ')
  if(cluster > 0){
    texts4 <- unlist(clusterApply(cl, texts3, capitalizeNames, nameIndex2))
  }else{
    texts4 <- unlist(lapply(texts3, function(x) capitalizeNames(x, nameIndex2)))
  }
  cat('Done.\n')
  if(cluster > 0){
    stopCluster(cl)
  }
  #print(proc.time()[ "elapsed" ] - start_time)
  return(texts4)
  
}

#Extract only capitalize names from texts
extractNames <- function(texts, distance = FALSE, reverse = FALSE){
  if(reverse){
    cat('Extracting non-names')
  }else{
    cat('Extracting names')
  }
  if(distance){
    cat(' keeping distance... ')
  }else{
    cat('... ')
  }
  texts2 <- unlist(lapply(texts, function(x) selectNames(x, distance, reverse)))
  cat('Done.\n')
  return(texts2)
}

selectNames <- function(text, distance, reverse){
  tokens <- tokenizeText(text, clean = FALSE)
  tokens2 <- tokens
  if(reverse){
    tokens2[grepl('^[A-Z]', tokens2)] <- ''
  }else{
    tokens2[!grepl('^[A-Z]', tokens2)] <- ''
  }
  if(!distance){
    tokens2 <- tokens2[tokens2 != '']
  }
  text2 <- paste(tokens2, collapse = ' ')
  text2 <- gsub("^\\s+|\\s+$", "", text2)
  return(text2)
}

createNameIndex <- function(texts, factor = 1){
  text <- paste(texts, collapse = ' ')
  tokens <- tokenizeText(text, clean = FALSE)
  tokens.form <- grepl('^[A-Z][a-zA-Z0-9]+', tokens)
  tokens.name <- toupper(tokens)
  df <- as.data.frame.matrix(table(tokens.name, tokens.form))
  #print(df)
  index <- rownames(subset(df, 1 + df[,1] * factor < df[,2])) #ignore tokens freq. is 1
  return(index)
}

capitalizeNames <- function(text, index){
  tokens <- toupper(tokenizeText(text, clean = FALSE))
  tokens2 <- ifelse(tokens %in% index, tokens, tolower(tokens))
  text2 <- paste(tokens2, collapse = ' ')
  text2 <- gsub("^\\s+|\\s+$", "", text2)
  return(text2)
}

createMPNameIndex <- function(texts, limit = 1000){
  
  text <- paste(texts, collapse = ' ')
  tokens <- tokenizeText(text, clean = FALSE)
  tokens[!grepl('^[A-Z][a-zA-Z0-9]+', tokens)] <- ''
  len <- length(tokens)
  
  #write(tokens, 'tokens.txt')
  #print(tokens)
  
  monos <- tokens
  monos <- monos[nchar(monos) > 0]
  mono.total <- length(monos)
  mono.aggre <- table(monos)
  mono.hash <- new.env(hash = TRUE, parent = emptyenv(), size = length(mono.aggre))
  for(mono in names(mono.aggre)){
    mono.hash[[mono]] <- as.vector(mono.aggre[mono])
  }
  
  tokens.diff1 <- tokens[1:len-1]
  tokens.diff2 <- tokens[2:len]
  bis <- paste(tokens.diff1, tokens.diff2, sep='-')
  bis <- bis[!grepl('^-|-$', bis)]
  
  bi.total <- length(bis)
  bi.aggre <- table(bis)
  bi.counts <- sort(bi.aggre, decreasing = TRUE)
  bi.counts <- bi.counts[bi.counts > 1 & names(bi.counts) != '']
  bi.counts <- head(bi.counts, limit)
  #print(length(bi.counts))
  #write.table(bi.aggre, file = "./token.bi.tsv", sep='\t', eol = "\n")
  
  mpnames <- c()
  for(bi.index in 1:length(bi.counts)){
    #print(paste(bi.index, length(bi.counts)))
    bi.count <- as.vector(bi.counts[bi.index])
    bi.parts <- unlist(strsplit(names(bi.counts[bi.index]), '-', fixed = TRUE))
    mono.count1 <- mono.hash[[bi.parts[1]]]
    mono.count2 <- mono.hash[[bi.parts[2]]]
    
    p1 <- bi.count / mono.count1 #1's association with 2
    p2 <- bi.count / mono.count2 #2's association with 1
    
    #if(bi.parts[2] == 'Kong'){
    #  print(paste(names(bi.counts[bi.index]), p1, p2, mono.count1, mono.count2)) 
    #}
    
    if(bi.count > 1 & (p1 > 0.5 | p2 > 0.5)){
      #print(paste(names(bi.counts[bi.index]), p1, p2, mono.count1, mono.count2)) 
      mpnames <- c(mpnames, names(bi.counts[bi.index]))
    }
  }
  return(mpnames)
}

joinMPNames <- function(text, index){
  
  text2 <- text
  for(mpn in index){
    pat <- gsub('-', ' ', mpn, fixed = TRUE)
    text2 <- gsub(pat, mpn, text2, fixed = TRUE)
    text2 <- gsub(toupper(pat), mpn, text2, fixed = TRUE)
  }
  return(text2)
}

mergeNames <- function(index, name){
  
}

createDateIndex <- function(){
  month1 <- unlist(strsplit('January|February|March|April|May|June|July|August|September|October|November|December', '|', fixed = TRUE))
  month2 <- unlist(strsplit('Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sept|Sep|Oct|Nov|Dec', '|', fixed = TRUE))
  day1 <- unlist(strsplit('Sunday|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday', '|', fixed = TRUE))
  day2 <- unlist(strsplit('Sun|Mon|Tue|Wed|Thu|Fri|Sat', '|', fixed = TRUE))  
  dates <- toupper(c(month1, month2, day1, day2))
  return(dates)
}
