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
#' @param x Object from which stopwords will be removed
#' @param stopwords Character vector of stopwords to remove.  Now requires an explicit
#' list to be supplied, for instance \code{stopwordsGet("english")}.
#'   @param verbose if \code{TRUE} print message about how many items were removed
#' @return an object with stopwords removed
#' @rdname stopwordsRemove
#' @export
#' @examples
#' ## examples for character objects
#' someText <- "Here's some text containing words we want to remove."
#' stopwordsRemove(someText, stopwordsGet("english"))
#' stopwordsRemove(someText, stopwordsGet("SMART"))
#' stopwordsRemove(someText, c("some", "want"))
#' itText <- "Ecco alcuni di testo contenente le parole che vogliamo rimuovere."
#' stopwordsRemove(itText, stopwordsGet("italian"))
#' 
#' ## example for dfm objects
#' mydfm <- dfm(uk2010immig, verbose=FALSE)
#' stopwordsRemove(mydfm, stopwordsGet("english"))
#' mydfms <- dfm(uk2010immig, verbose=FALSE, matrixType="sparse")
#' dim(stopwordsRemove(mydfms, stopwordsGet("SMART")))
#' 
#' ## example for collocations
#' (myCollocs <- collocations(inaugTexts, top=20))
#' stopwordsRemove(myCollocs, stopwordsGet("english"))
stopwordsRemove <- function(x, stopwords=NULL, verbose=TRUE) {
    UseMethod("stopwordsRemove")
}


#' @rdname stopwordsRemove
#' @export
stopwordsRemove.character <- function(x, stopwords=NULL, verbose=TRUE) {
    if (is.null(stopwords))
        stop("Must supply a character vector of stopwords, e.g. stopwordsGet(\"english\")")
    ret <- gsub(paste("(\\b|\\s)(", paste(stopwords, collapse="|"), ")(\\b)", sep=""), "", x, ignore.case=TRUE)
    # if (verbose) cat("Removed", sum(ret==""), "tokens, from a list of", length(stopwords), "stopwords.")
    ret[ret != ""]
    ## Note: Will not remove capitalized words, such as those starting the sentence.  e.g. "The man."
}

#' @rdname stopwordsRemove
#' @export
stopwordsRemove.dfm <- function(x, stopwords=NULL, verbose=TRUE) {
    if (is.null(stopwords))
        stop("Must supply a character vector of stopwords, e.g. stopwordsGet(\"english\")")
    removeIndex <- which(colnames(x) %in% stopwords)
    if (verbose) cat("Removed", format(length(removeIndex), big.mark=","),  
                     "features, from a list of", length(stopwords), "stopwords.\n")
    x[, -removeIndex]
}

#' @rdname stopwordsRemove
#' @export
stopwordsRemove.dfmSnum <- function(x, stopwords=NULL, verbose=TRUE) {
    if (is.null(stopwords))
        stop("Must supply a character vector of stopwords, e.g. stopwordsGet(\"english\")")
    removeIndex <- which(colnames(x) %in% stopwords)
    if (verbose) cat("Removed", format(length(removeIndex), big.mark=","),  
                     "features, from a list of", length(stopwords), "stopwords.\n")
    x[, -removeIndex]
}
    
#' @rdname stopwordsRemove
#' @export
stopwordsRemove.collocations <- function(x, stopwords=NULL, verbose=TRUE) {
    if (is.null(stopwords))
        stop("Must supply a character vector of stopwords, e.g. stopwordsGet(\"english\")")
    removeIndex <- grep(paste0("\\b", paste(stopwords, collapse="\\b|\\b"), "\\b"), 
                        x$collocation)
    if (length(removeIndex) > 0) 
        x <- x[-removeIndex, ]
    if (verbose) cat("Removed", format(length(removeIndex), big.mark=","), 
                     "collocations, from a list of", length(stopwords), "stopwords.\n")
    x
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
                     "german", "italian", "portuguese", "spanish", "arabic"))) {
        stop(paste(kind, "is not a recognized stopword list type."))
    }
    data(stopwords, envir = environment())
    stopwords[[kind]]
}


