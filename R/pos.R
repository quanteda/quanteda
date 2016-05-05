
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
#' sents <- tokenize(inaugTexts[1], what="sentence", simplify=TRUE)
#' tokens <- lapply(sents, function(x) tokenize(x, simplify=TRUE))
#' pos <- tagPOS(tokens) # tokens should be sentence
#' @export
tagPOS.tokenizedTexts <- function(x, ...){
    
    model <- openNLP::Maxent_POS_Tag_Annotator()
    anno_word <- NLP::Simple_Word_Token_Annotator(function(s){
        token <- stringi::stri_split_fixed(s, '|', simplify=TRUE)
        length <- stringi::stri_length(token)
        to <- as.integer(cumsum(length))
        from <- as.integer(to - (length - 1))
        return(NLP::Span(from, to))
    })
    
    anno_sent <- NLP::Simple_Sent_Token_Annotator(function(s){
        return(NLP::Span(1, stringi::stri_length(s)))
    })
    
    pos <- lapply(x, function(y) {
        str <- NLP::as.String(paste(y, collapse='|'))
        anno <- NLP::annotate(str, model, NLP::annotate(str, list(anno_sent, anno_word)))
        anno <- subset(anno, type == "word")
        return(sapply(anno$features, '[[', "POS"))
    })
    pos
}


