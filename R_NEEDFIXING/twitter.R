#' @rdname corpus
#' @export
corpus.twitter <- function(x, enc=NULL, notes=NULL, citation=NULL, ...) {
    # extract the content (texts)
    texts <- x$text
    atts <-as.data.frame(results[,2:ncol(results)])    
    
    # using docvars inappropriately here but they show up as docmeta given 
    # the _ in the variable names
    corpus(texts, docvars=atts,
           source=paste("Converted from twitter search results"),
           enc=enc, ...)
}

#' work-in-progress interface to Twitter streaming API
twitterStreamer <- function(){
  
}

#' work-in-progress from-scratch interface to Twitter search API
twitterSearch <- function(){
  
  
}
