source('~/quanteda/R/kohei_tokenize2.R')

#' Find collocation
#' This depends on Kohei's tokenizer
#' 
#' @param texts
#' @param method 
#' @param limit Limited the words to top most frequenty words
#' @return collocation matrix
#' @export
#' @examples
#' data(ieTexts)
#' texts <- cleanText(ieTexts, lower=TRUE)
#' mx <- findCollocation(texts)
#' plotCollocation(mx)

findCollocation <- function(texts, method = 'count', limit = 1000){
  mx <- getCollocationMX(texts, method, limit = limit)
  return(mx)
}

plotCollocation <- function(mx){
  
  require(graphics)
  index <- attr(mx, 'index')
  mx2 <- convertMx2dist(mx)
  attr(mx2, "Labels") <- unlist(flipCollocationIndex(index))
  hc <- hclust(mx2, "ave")
  #print(hc)
  #png("hc.png", width = 6000, height = 1000)
  pdf("hc.pdf", width = 200, height = 15)
  plot(hc)
  dev.off();
  cat('Plot was saved as ', getwd(), '/hd.pdf', '\n', sep='')
}

createCollocationIndex <- function(texts, limit = 1000) {
  text <- paste(texts, collapse = ' ')
  tokens <- tokenizeText(text, clean = FALSE)
  tokens <- tokens[nchar(tokens) > 0]
  token.aggre <- table(tokens)
  token.sort <- sort(token.aggre, decreasing=TRUE)
  if(limit > length(token.sort)){
    limit <- length(token.sort)
  }
  token.aggre2 <- token.aggre[token.aggre >= token.sort[limit]]
  tokens.unique <- names(token.aggre2)
  hash <- new.env(hash = TRUE, parent = emptyenv(), size = length(tokens.unique))
  for(token.index in 1:length(tokens.unique)) {
    hash[[tokens.unique[token.index]]] <- token.index
  }
  return(hash)
}

convertMx2dist <- function(mx){
  len <- dim(mx)[1]
  mx[upper.tri(mx, diag = TRUE)] <- NA
  dist <- as.vector(mx)
  dist <- dist[!is.na(dist)]
  attr(dist, 'Size') <- len
  #attr(dist, 'class') <- 'dist'
  #attr(dist, 'Diag') <- 'FALSE'
  #attr(dist, 'Upper') <- 'FALSE'
  return(1 / dist)
}

showCollocation <- function(mx, thresh = 0){
  
  index <- attr(mx, 'index')
  tokens <- flipCollocationIndex(index)
  mx[upper.tri(mx, diag = TRUE)] <- NA
  elms <- which(mx >= thresh & !is.na(mx), arr.ind=T)
  len <- length(elms)
  sv <- rep(NA, len)
  nv <- rep(NA, len)

  if(length(elms) == 0){
    cat('No item above the thresh\n')
    stop()
  }
  print(elms)
  for(k in 1:(elms)[1]){
    i <- elms[k,][[1]]
    j <- elms[k,][[2]]
    sv[k] <- mx[i, j]
    nv[k] <- paste(tokens[[i]], tokens[[j]])
    print(sprintf("%s %s", tokens[[i]], tokens[[j]]))    
  }
  
  
  df <- data.frame(score = sv, row.names = nv)
  df2 <- df[order(-df$score), , drop=FALSE]
  return(df2)
}

showCollocationToken <- function(mx, token, thresh = 0){
  sv <- c()
  nv <- c()
  index <- attrib(mx, 'index')
  toks <- flipCollocationIndex(index)
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

flipCollocationIndex <- function(index){
  list <- list()
  keys <- names(as.list(index))
  for(key in keys){
    val <- as.integer(index[[key]])
    list[[val]] <- key
  }
  return(list)
}


getCollocationMX <- function(texts, method = 'count', window = 5, smooth = 1, limit = 1000, segment = TRUE){
  index <- createCollocationIndex(texts, limit = limit)
  index.len <- length(index)
  mx <- matrix(0, nrow = index.len, ncol = index.len)
  cmx <- matrix(0, nrow = index.len, ncol = 1)
  #print(mx)
  if(segment){
    texts <- unlist(sentenceSeg(paste(texts, sep="\n")))
  }
  for(text in texts){
    tokens <- tokenizeText(text, clean = FALSE)
    len <- length(tokens)
    for(i in 1:length(tokens)){
      if(nchar(tokens[i]) == 0) next
      start <- i - window
      end <- i + window
      if(start < 1) start <- 1
      if(end > len) end <- len
      ti1 <- index[[tokens[i]]]
      if(!is.null(ti1)){
        cmx[ti1,1] <- cmx[ti1,1] + 1
      }
      for(j in start:end){
        if(nchar(tokens[j]) == 0) next
        ti2 <- index[[tokens[j]]]
        if(!is.null(ti1) & !is.null(ti2)){
          mx[ti1, ti2] <- mx[ti1, ti2] + 1
        }
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
  attr(smx, 'index') <- index
  return(smx)
}
