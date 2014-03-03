
#' This function creates a index for faster access to matricis
#' 
#' @param keys All words to be indexed as vector fromat
#' @return index List that allows fast referece to column/row number
#' @author Kohei Watanabe
#' @export
#' @examples
#' index <- collocates.createIndex(row.names(dfm))
collocates.createIndex <- function(keys) {
  result <- new.env(hash = TRUE, parent = emptyenv(), size = length(keys))
  i <- 1
  for(key in keys) {
    result[[key]] <- i
    i <- i + 1
  }
  return(result)
}

#' This function converts a collocation matrix to a distance matrix that can be used in hclust() in 'graphics' pachage
#' 
#' @param mx collocation matrix created by collocates.findCollocation()
#' @return distance matrix for hclust()
#' @author Kohei Watanabe
#' @export
#' @examples
#' dist <- collocates.mx2dist(mx)
#' hc <- hclust(dist, "ave")
#' plot(hc, hang = -1)
collocates.mx2dist <- function(mx){
  len <- dim(mx)[1]
  mx[upper.tri(mx, diag = TRUE)] <- NA
  dist <- as.vector(mx)
  dist <- dist[!is.na(dist)]
  attr(dist, 'Size') <- len
  #attr(dist, 'class') <- 'dist'
  #attr(dist, 'Diag') <- 'FALSE'
  #attr(dist, 'Upper') <- 'FALSE'
  return(dist)
}


#' This function converts a collocation matrix to a list of collocates as data frame
#' 
#' @param thresh Threshhold of scores
#' @param index Index created by collocates.createIndex()
#' @param mx Collocation matrix created by collocates.findCollocation()
#' @return df2 List of collocates as data frame
#' @author Kohei Watanabe
#' @export
#' @examples 
#' collocates <- collocates.mx2df.score(3, index, mx)
collocates.mx2df.score <- function(thresh = 0, index, mx){
  sv <- c()
  nv <- c()
  toks <- collocates.flipIndex(index)
  mx[upper.tri(mx, diag = TRUE)] <- NA
  elms <- which(mx>=thresh, arr.ind=T)
  
  if(length(elms) == 0){
    print('No item above the thresh')
    return()
  }
  #print(elms)
  for(k in 1:dim(elms)[1]){
    #print(elms[k,])
    i <- elms[k,][[1]]
    j <- elms[k,][[2]]
    sv <- append(sv, mx[i, j])
    nv <- append(nv, sprintf("%s %s", toks[[i]], toks[[j]]))
  }
  
  df <- data.frame(score = sv, row.names = nv)
  #print(df)
  #row.names(df) <- nv
  #print(nv)
  df2 <- df[order(-df$score), , drop=FALSE]
  return(df2)
}

#' This function extract collocates of a specific word from a collocation matrix
#' 
#' @param token Word of interest
#' @param index Index created by collocates.createIndex()
#' @param mx Collocation matrix created by collocates.findCollocation()
#' @param thresh Threshhold of scores
#' @return df2 List of collocates as data frame
#' @author Kohei Watanabent
#' @export
#' @examples 
#' collocates <- collocates.mx2df.token('war', index, mx, 3)
collocates.mx2df.token <- function(token, index, mx, thresh = 0){
  sv <- c()
  nv <- c()
  toks <- collocates.flipIndex(index)
  i <- index[[token]]
  len <- length(index)
  for(j in 1:len){
    if(mx[i, j] >= thresh){
      sv <- append(sv, mx[i, j])
      nv <- append(nv, sprintf("%s %s", toks[[i]], toks[[j]]))
    }
  }
  df <- data.frame(score = sv, row.names = nv)
  df2 <- df[order(-df$score), , drop=FALSE]
  return(df2)
}

#' This function flips index to obtain words from column/row number of collocate matricis
#' 
#' @param index Index created by collocates.createIndex()
#' @return list List of words with column/row number as keys 
#' @author Kohei Watanabent
#' @export
#' @examples 
#' words <- collocates.flipIndex(index)
collocates.flipIndex <- function(index){
  list <- list()
  keys <- names(as.list(index))
  for(key in keys){
    val <- as.integer(index[[key]])
    list[[val]] <- key
  }
  return(list)
}

#' This function finds collocates in a given texts
#' 
#' @param method Method to calculate collocation scores
#' @param index Index created by collocates.createIndex()
#' @param text Text files
#' @param window Window span of collocation
#' @param smooth Constant to be added to word frequency
#' @param segment Separation by sentences
#' @return smx Collocaton matrix
#' @author Kohei Watanabent
#' @export
#' @examples 
#' mx <- collocates.findCollocation('pmi', index, texts, 2.2e-16, TRUE)
collocates.findCollocation <- function(method, index, texts, window = 5, smooth = 1, segment = TRUE){
  len <- length(index)
  mx <- matrix(0, nrow = len, ncol = len)
  cmx <- matrix(0, nrow = len, ncol = 1)
  #print(mx)
  if(segment){
    texts <- unlist(sentenceSeg(paste(texts, sep="\n")))
  }
  for(text in texts){
    toks <- tokenize(text)
    for(i in 1:length(toks)){
      if(length(toks[i]) == 0) next
      start <- i - window
      end <- i + window
      if(start < 0) start <- 1
      if(end > length(toks)) end <- length(toks)
      ti1 <- index[[toks[i]]]
      cmx[ti1,1] <- cmx[ti1,1] + 1
      for(j in start:end){
        #if(i == j) next
        if(length(toks[j]) == 0) next
        ti2 <- index[[toks[j]]]
        mx[ti1, ti2] <- mx[ti1, ti2] + 1
      }
    }
  }
  mx <- mx + smooth
  cmx <- cmx + smooth
  
  if(method == 'mi'){
    #Mutual information
    smx <- log(mx * sum(cmx), 2) / (cmx %*% t(cmx))
  }else if(method == 'pmi'){
    #Pointwise (specific) mutual information (Wordsmith replication)
    mxsum <- sum(mx[lower.tri(mx, diag = TRUE)])
    smx <- log((mx / mxsum) / ((cmx %*% t(cmx)) / (sum(cmx) ^ 2)), 2)
  }else if(method == 'count'){
    smx <- mx
  }
  return(smx)
}

#' This function create a data frame that can be used for selectFeatures()
#' 
#' @param file1 Target text
#' @param file2 Reference text
#' @param thresh Threshold for collocates.mx2df.score()
#' @param window Window span for collocates.findCollocation()
#' @param stopwords Stopwords removal for dfm()
#' @return cfvm2 Collocatons as data frame
#' @author Kohei Watanabent
#' @examples 
#' cfvm <- collocates.compare('Con 2010.txt', 'Con 2010.txt', window = 5, stopwords = TRUE)
#
collocates.compare <- function(file1, file2, thresh = 1, window, stopwords = FALSE){
  cfvs <- list()
  i <- 1
  cfvm <- data.frame()
  for(file in c(file1, file2)){
    text <- getTextFiles(file)
    corpus <- corpusCreate(text)
    dfm <- dfm(corpus, stopwords = stopwords)
    index <- collocates.createIndex(colnames(dfm))
    mx <- collocates.findCollocation('count', index, text, window, 0)
    cfvs[[i]] <- collocates.mx2df.score(thresh, index, mx)
    i <- i + 1
  }
  cfvm <- merge(cfvs[[1]]['score'], cfvs[[2]]['score'], by=0, all=TRUE, incomparables = NA)
  v1 <- cfvm[,2]
  v1[is.na(v1)] <- 0
  v0 <- cfvm[,3]
  v0[is.na(v0)] <- 0
  cfvm2 <- data.frame(cat1 = v1, cat0 = v0, row.names = cfvm[,1]) 
  return(cfvm2)
}
