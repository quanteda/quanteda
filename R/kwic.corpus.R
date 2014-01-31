kwic.corpus <-
function(corpus, word, window=5){
  contexts <- lapply(corpus$attribs$texts, kwic.character, word=word, window=window)
  names(contexts) <- row.names(corpus$attribs)
  return(contexts)
}
