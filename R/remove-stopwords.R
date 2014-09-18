#' remove stopwords from a text or dfm
#' 
#' This function takes a character vector or \link{dfm} and removes words in the
#' remove common or 'semantically empty' words from a text. See \link{stopwordsGet}
#' for the information about the default lists.
#' 
#' This function takes a character vector 'text' and removes words in the list 
#' provided in stopwords. If no list of stopwords is provided a default
#' list for English is used. The function \link{stopwordsGet} can load a default
#' set of stopwords for many languages.
#' 
#' @param text Text from which stopwords will be removed
#' @param stopwords Character vector of stopwords to remove - if none is 
#'   supplied, a default set of English stopwords is used
#' @return a character vector or dfm with stopwords removed
#' @rdname stopwordsRemove
#' @export
#' @examples
#' ## examples for character objects
#' someText <- "Here is an example of text containing some stopwords we want to remove."
#' itText <- "Ecco un esempio di testo contenente alcune parole non significative che vogliamo rimuovere."
#' stopwordsRemove(someText)
#' stopwordsRemove(someText, stopwordsGet("SMART"))
#' stopwordsRemove(itText, stopwordsGet("italian"))
#' stopwordsRemove(someText, c("containing", "example"))
#' 
#' ## example for dfm objects
#' docmat <- dfm(uk2010immig)
#' docmatNostopwords <- stopwordsRemove(docmat)
#' dim(docmat)
#' dim(docmatNostopwords)
#' dim(stopwordsRemove(docmat, stopwordsGet("SMART")))
stopwordsRemove <- function(text, stopwords=NULL) {
    UseMethod("stopwordsRemove")
}


#' @rdname stopwordsRemove
#' @export
stopwordsRemove.character <- function(text, stopwords=NULL) {
    if (is.null(stopwords)) {
        stopwords <- stopwordsGet("english")
    }
    return(gsub(paste("(\\b|\\s)(", paste(stopwords, collapse="|"), ")(\\b)", sep=""), "", text, ignore.case=TRUE))
    ## Note: Will not remove capitalized words, such as those starting the sentence.  e.g. "The man."
}


#' @export
stopwordsRemove.dfm <- function(text, stopwords=NULL) {
    if (!("dfm" %in% class(text)) ) {
        stop(paste(text, "does not appear to be a valid dfm."))
    }
    if (is.null(stopwords)) {
        stopwords <- stopwordsGet("english")
    }
    removeIndex <- which(colnames(text) %in% stopwords)
    return(text[, -removeIndex])
}


#' access stopwords
#' 
#' This function retrieves stopwords from the type specified in the \code{kind}
#' argument and returns the stopword list as a character vector The default is
#' English. See \link{stopwords} for information about the list.
#' 
#' @param kind The pre-set kind of stopwords (as a character string)
#' @return a character vector or dfm with stopwords removed
#' @export
#' @examples
#' stopwordsGet()
#' stopwordsGet("italian")
stopwordsGet <- function(kind="english") {
    if (!(kind %in% c("english", "SMART", "danish", "french", "hungarian", "norwegian", "russian", "swedish", "catalan", "dutch", "finnish",   
                     "german", "italian", "portuguese", "spanish"))) {
        stop(paste(kind, "is not a recognized stopword list type."))
    }
    data(stopwords, envir = environment())
    stopwords[[kind]]
}


