corpus.reshape <-
function(corpus){
  sentence <- sentenceSeg(corpus$attribs$texts[[1]])
  sentenceno <- 1:length(sentence)
  sourcetext <- rep(row.names(corpus$attribs)[[1]], length(sentence))
  atts <- data.frame(sourcetext, sentenceno)
  # print(names(atts))
  sentCorp <- corpus.create(unlist(sentence), attribs=atts)
  # print(names(sentCorp$attribs))
  for(i in 2:nrow(corpus$attribs)){
    sentence <- sentenceSeg(corpus$attribs$texts[[i]])
    sentenceno <- 1:length(sentence)
    sourcetext <- rep(row.names(corpus$attribs)[[i]], length(sentence))
    atts <- data.frame(sourcetext, sentenceno)
    
    sentCorp<-corpus.append(sentCorp, unlist(sentence), atts)
  }
  return(sentCorp)
}
