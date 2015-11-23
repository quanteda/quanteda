# pos.env <- new.env()
# pos.env$model <- Maxent_POS_Tag_Annotator()

anno_word <- Simple_Word_Token_Annotator(function(s){
  return(NLP::Span(pos.env$from, pos.env$to))
})
anno_sent <- Simple_Sent_Token_Annotator(function(s){
  return(NLP::Span(head(pos.env$from, 1), tail(pos.env$to, 1)))
})



#' tag parts of speech in a tokenized list
#'
#' Add tags to a tokenized list of parts of speech
#' @param x list of tokens to tag
#' @param ... not currently used
#' @export
tagPOS <- function(x, ...)
  UseMethod("tagPOS")


#' @rdname tagPOS
#' @examples
#' tokens <- tokenize(inaugTexts[1], simplify=TRUE)
#' pos <- tagPOS(tokens) # tokens should be sentence
#' @export
tagPOS.tokenizedTexts <- function(x, ...){
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


