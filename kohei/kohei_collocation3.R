source('~/quanteda/kohei/kohei_tokenize2.R')

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

findCollocation <- function(texts, method = 'count', window = 5, smooth = 1, limit = 1000, filter = '', segment = FALSE){
  mx <- getCollocationMX(texts, method, window = window, smooth = smooth, limit = limit, filter = filter, segment = segment)
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

createCollocationIndex <- function(texts, limit, filter) {
  text <- paste(texts, collapse = ' ')
  tokens <- tokenizeText(text, clean = FALSE)
  tokens <- tokens[nchar(tokens) > 0]
  token.aggre <- table(tokens)
  token.sort <- sort(token.aggre, decreasing=TRUE)
  if(limit > length(token.sort)){
    limit <- length(token.sort)
  }
  if(limit == 0){
    token.aggre2 <- token.aggre
  }else{
    token.aggre2 <- token.aggre[token.aggre >= token.sort[limit]]
  }
  tokens.unique <- names(token.aggre2)
  if(filter == ''){
    tokens.x <- tokens.unique
    tokens.y <- tokens.unique
  }else{
    print(tokens.unique[grepl(filter, tokens.unique)])
    tokens.x <- tokens.unique[grep(filter, tokens.unique)]
    tokens.y <- tokens.unique
  }
  hash = list()
  hash[['x']] <- new.env(hash = TRUE, parent = emptyenv(), size = length(tokens.x))
  for(xi in 1:length(tokens.x)) {
    hash[['x']][[tokens.x[xi]]] <- xi
  }
  hash[['y']] <- new.env(hash = TRUE, parent = emptyenv(), size = length(tokens.y))
  for(yi in 1:length(tokens.y)) {
    hash[['y']][[tokens.y[yi]]] <- yi
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
  tokens.x <- flipCollocationIndex(index[['x']])
  tokens.y <- flipCollocationIndex(index[['y']])
  #mx[upper.tri(mx, diag = TRUE)] <- NA
  elms <- which(mx >= thresh & !is.na(mx), arr.ind=T)
  len <- nrow(elms)
  values <- rep(NA, len)
  names <- rep(NA, len)

  if(length(elms) == 0){
    cat('No item above the thresh\n')
    stop()
  }
  for(k in 1:len){
    yi <- elms[k,][1]
    xi <- elms[k,][2]
    values[k] <- mx[yi, xi]
    names[k] <- paste(tokens.x[[xi]], tokens.y[[yi]])
    #print(sprintf("%s %s", tokens.x[[xi]], tokens.y[[yi]]))    
  }
 
  df <- data.frame(score = values, row.names = names)
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


getCollocationMX <- function(texts, method = 'count', window = 5, smooth = 1, limit = 1000, filter = filter, segment = FALSE){
  index <- createCollocationIndex(texts, limit = limit, filter = filter)
  xi.len <- length(index[['x']])
  yi.len <- length(index[['y']])
  mx <- matrix(0, nrow = yi.len, ncol = xi.len)
  rv <- matrix(0, nrow = 1, ncol = xi.len)
  cv <- matrix(0, nrow = yi.len, ncol = 1)
  wv <- matrix(0, nrow = yi.len, ncol = 1)
  sum <- 0

  if(segment){
    texts <- unlist(sentenceSeg(paste(texts, sep="\n")))
  }
  for(text in texts){
    tokens <- tokenizeText(text, clean = FALSE)
    tokens <- tokens[nchar(tokens) > 0]
    len <- length(tokens)
    for(i in 1:length(tokens)){
      #if(nchar(tokens[i]) == 0) next

      x <- index[['x']][[tokens[i]]]
      if(!is.null(x)){
        rv[1, x] <- rv[1, x] + 1
      }
      
      #----
      y <- index[['y']][[tokens[i]]]
      if(!is.null(y)){
        cv[y, 1] <- cv[y, 1] + 1
        #print(paste(x, y ,tokens[i]))
      }
      #---
      range <- getCollocationRange(len, i, window)
      sum <- sum + length(range)
      if(!is.null(x)){
        for(j in range){
          y <- index[['y']][[tokens[j]]]
          if(!is.null(x) & !is.null(y)){
            #print(paste(tokens[i] ,tokens[j]))
            #dist <- abs(j - i)
            #mx[y, x] <- mx[y, x] + (1 - (dist / window))
            mx[y, x] <- mx[y, x] + 1
          }
        }
      }
    }
  }
  #print(mx)
  print(cv)
  sum <- sum + (smooth * length(mx))
  mx <- mx + smooth
  rv <- rv + smooth
  cv <- cv + smooth
  print(paste('sum', sum))
  #cv <- cv * wv
 
  
  if(method == 'pmi'){
    #Pointwise (specific) mutual information (Wordsmith replication)
    #sum <- sum(mx) / 2
    #print(mx / sum)
    #print(cv %*% rv / (sum(cv) ^ 2))

    #mx2 <- log((mx / sum) / ((cv %*% rv) / (sum(cv) ^ 2)), 2)
    mx2 <- log((mx / sum(cv)) / ((cv %*% rv) / (sum(cv) ^ 2)), 2)
    
    
  }else if(method == 'count'){
    mx2 <- mx
  }
  attr(mx2, 'index') <- index
  return(mx2)
}

getCollocationRange <- function(len, pos, window){
  start <- pos - window
  end <- pos + window
  #print(paste(i, end, tokens[end]))
  if(start < 1) start <- 1
  if(end > len) end <- len
  range <- (start:end)
  range2 <- range
  #range2 <- range[range != pos]
  return(range2)
}