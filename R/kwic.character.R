kwic.character <-
function(text, word, window=5){
    toks <- tokenize(text)
    matches = which(toks == word )
    contexts = vector()
    if(length(matches) == 0){return(NA)}
    contexts <- rep(NA, length(matches))
    for(m in 1:length(matches)){
      start <- matches[m] - window
      end <- matches[m] + window
      if(start < 0) start <- 0
      if(end > length(toks)) end <- length(toks)
      
      contexts[m] <- paste(toks[start:end], collapse=' ' )
      print(paste(toks[start:end], collapse=' ' ))
    }
  return(contexts)
}
