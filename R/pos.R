require(openNLP)
require(NLP)

pos.env <- new.env()
pos.env$model <- Maxent_POS_Tag_Annotator()

anno_word <- Simple_Word_Token_Annotator(function(s){
  return(NLP::Span(pos.env$from, pos.env$to))
})
anno_sent <- Simple_Sent_Token_Annotator(function(s){
  return(NLP::Span(head(pos.env$from, 1), tail(pos.env$to, 1)))
})

# @examples
# tokens <- tokenize(inaugTexts[1], simplify=TRUE)
# pos <- tagPOS(tokens)
# 
# @export
tagPOS <- function(x, ...){
  length <- stringi::stri_length(x)
  to <- as.integer(cumsum(length))
  from <- as.integer(to - (length - 1))
  str <- as.String(paste(token, collapse=''))
  
  pos.env$to <- to
  pos.env$from <- from
  pos <- annotate(str, pos.env$model, annotate(str, list(anno_sent, anno_word)))
  pos <- subset(pos, type == "word")
  return(sapply(pos$features, '[[', "POS"))
}


