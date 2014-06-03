<<<<<<< HEAD
#' remove stopwords from a text or dfm
#'
#' This function takes a character vector or \link{dfm} and removes words in the
=======
#' remove common or 'semantically empty' words from a text.
#'
#' This function takes a character vector 'text' and removes words in the
>>>>>>> 304330e5366a2c59f63041efbb1b458948968ddf
#' list provided in 'stopwords'. If no list of stopwords is provided a 
#' default list for English is used.
#' 
#' @param text Text from which stopwords will be removed
#' @param stopwords Character vector of stopwords to remove
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
#' data(iebudgets)
#' wfm <- dfm(subset(iebudgets, year==2010))
#' wfm.nostopwords <- stopwordsRemove(wfm)
#' dim(wfm)
#' dim(wfm.nostopwords)
#' dim(stopwordsRemove(wfm, stopwordsGet("SMART")))
stopwordsRemove <- function(text, stopwords=NULL) {
    UseMethod("stopwordsRemove")
}


#' @rdname stopwordsRemove
#' @method stopwordsRemove character
#' @S3method stopwordsRemove character
stopwordsRemove.character <- function(text, stopwords=NULL) {
    if (is.null(stopwords)) {
        stopwords <- stopwordsGet("english")
    }
    return(gsub(paste(stopwords, collapse=" | "), " ", text))
}


#' @rdname stopwordsRemove
#' @method stopwordsRemove matrix
#' @S3method stopwordsRemove matrix
stopwordsRemove.matrix <- function(text, stopwords=NULL) {
    if (names(dimnames(text))[2] != "words") {
        stop(paste(text, "does not appear to be a valid dfm matrix."))
    }
    if (is.null(stopwords)) {
        stopwords <- stopwordsGet("english")
    }
    remove.index <- which(colnames(text) %in% stopwords)
    return(text[, -remove.index])
}


#' access stopwords
#'
#' This function retrieves stopwords from the type specified in the \code{kind} argument and 
#' returns the stopword list as a character vector
#' The default is English.
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


