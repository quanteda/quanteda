

#' remove features from an object
#' 
#' This function removes features from a variety of objects, such as text, a dfm, or
#' a list of collocations.  The most common usage for \code{removeFeatures} will be
#' to eliminate stop words from a text or text-based object.  Some commonly used
#' built-in stop words can be accessed through \code{\link{stopwords}}.
#' 
#' Because we believe the user should take full responsibility for any features
#' that are removed, we do not provide a default list.  Use \code{\link{stopwords}} instead.
#' 
#' @param x object from which stopwords will be removed
#' @param stopwords character vector of features to remove.  Now requires an explicit
#' list to be supplied, for instance \code{stopwords("english")}.
#' @param verbose if \code{TRUE} print message about how many features were removed
#' @param pos indexes of word position if called on collocations: remove if word \code{pos}
#' is a stopword
#' @param ... additional arguments for some methods (such as \code{pos} for \link{collocations})
#' @return an object with stopwords removed
#' @name removeFeatures
#' @export
#' @seealso \link{stopwords}
#' @examples
#' ## examples for character objects
#' someText <- "Here's some text containing words we want to remove."
#' removeFeatures(someText, stopwords("english", verbose=FALSE))
#' removeFeatures(someText, stopwords("SMART", verbose=FALSE))
#' removeFeatures(someText, c("some", "want"))
#' itText <- "Ecco alcuni di testo contenente le parole che vogliamo rimuovere."
#' removeFeatures(itText, stopwords("italian", verbose=FALSE))
#' 
#' ## example for dfm objects
#' mydfm <- dfm(ukimmigTexts, verbose=FALSE)
#' removeFeatures(mydfm, stopwords("english", verbose=FALSE))
#' 
#' ## example for collocations
#' (myCollocs <- collocations(inaugTexts, top=20))
#' removeFeatures(myCollocs, stopwords("english", verbose=FALSE))
removeFeatures <- function(x, stopwords=NULL, verbose=TRUE, ...) {
    UseMethod("removeFeatures")
}


#' @rdname removeFeatures
#' @export
removeFeatures.character <- function(x, stopwords=NULL, verbose=TRUE, ...) {
    if (is.null(stopwords))
        stop("Must supply a character vector of stopwords, e.g. stopwordsGet(\"english\")")
    ret <- gsub(paste("(\\b|\\s)(", paste(stopwords, collapse="|"), ")(\\b)", sep=""), "", x, ignore.case=TRUE)
    # if (verbose) cat("Removed", sum(ret==""), "tokens, from a list of", length(stopwords), "stopwords.")
    ret[ret != ""]
    ## Note: Will not remove capitalized words, such as those starting the sentence.  e.g. "The man."
}

#' @rdname removeFeatures
#' @export
removeFeatures.dfm <- function(x, stopwords=NULL, verbose=TRUE, ...) {
    if (is.null(stopwords))
        stop("Must supply a character vector of stopwords, e.g. stopwordsGet(\"english\")")
    removeIndex <- which(colnames(x) %in% stopwords)
    if (verbose) cat("Removed", format(length(removeIndex), big.mark=","),  
                     "features, from a list of", length(stopwords), "stopwords.\n")
    x[, -removeIndex]
}


### now optimized for speed using data.table
#' @rdname removeFeatures
#' @export
removeFeatures.collocations <- function(x, stopwords=NULL, verbose=TRUE, pos=c(1,2,3), ...) {
    word <- word1 <- word2 <- word3 <- NULL
    if (is.null(stopwords))
        stop("Must supply a character vector of stopwords, e.g. stopwordsGet(\"english\")")
    if (!all(pos %in% 1:3))
        stop("pos for collocation position can only be 1, 2, and/or 3")
    stopwordTable <- data.table(word=stopwords, remove=1)
    setkey(stopwordTable, word)
    x$order <- 1:nrow(x)
    
    if (3 %in% pos) {
        setnames(stopwordTable, 1, "word3")
        setkey(x, word3)
        x <- stopwordTable[x]
        x <- x[is.na(remove)]
        x[, remove:=NULL]
    }
    if (2 %in% pos) {
        setnames(stopwordTable, 1, "word2")
        setkey(x, word2)
        x <- stopwordTable[x]
        x <- x[is.na(remove)]
        x[, remove:=NULL]
    }
    if (1 %in% pos) {
        setnames(stopwordTable, 1, "word1")
        setkey(x, word1)
        x <- stopwordTable[x]
        x <- x[is.na(remove)]
        x[, remove:=NULL]
    }
    setorder(x, order)
    setcolorder(x, c("word1", "word2", "word3", names(x)[4:ncol(x)]))
    x[, order:=NULL]
    x
}



#' @rdname removeFeatures
#' @export
stopwordsRemove <- function(x, stopwords=NULL, verbose=TRUE) {
    cat("stopwordsRemove is deprecated, use removeFeatures instead.")
    UseMethod("removeFeatures")
}



#' access built-in stopwords
#' 
#' This function retrieves stopwords from the type specified in the \code{kind} 
#' argument and returns the stopword list as a character vector The default is 
#' English.
#' 
#' The stopword list are SMART English stopwords from the SMART information
#' retrieval system (obtained from 
#' http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop)
#' and a set of stopword lists from the Snowball stemmer project in different
#' languages (obtained from 
#' http://svn.tartarus.org/snowball/trunk/website/algorithms/*/stop.txt).
#' Supported languages are arabic, danish, dutch, english, finnish, french,
#' german, hungarian, italian, norwegian, portuguese, russian, spanish, and 
#' swedish. Language names are case sensitive.
#' @rdname stopwords
#' @section A note of caution:
#'  Stop words are an arbitrary choice imposed by the
#'   user, and accessing a pre-defined list of words to ignore does not mean
#'   that it will perfectly fit your needs. You are strongly encourged to
#'   inspect the list and to make sure it fits your particular requirements.
#' @param kind The pre-set kind of stopwords (as a character string).  Allowed
#'   values are \code{english}, \code{SMART}, \code{danish}, \code{french},
#'   \code{hungarian}, \code{norwegian}, \code{russian}, \code{swedish},
#'   \code{catalan}, \code{dutch}, \code{finnish}, \code{german},
#'   \code{italian}, \code{portuguese}, \code{spanish}, \code{arabic}
#' @param verbose if \code{FALSE}, suppress the annoying warning note
#' @return a character vector of stopwords
#' @name stopwords
#' @export
#' @examples
#' stopwords("english")[1:5]
#' stopwords("italian")[1:5]
#' stopwords("arabic")[1:5]
stopwords <- function(kind="english", verbose=FALSE) {
    if (!(kind %in% c("english", "SMART", "danish", "french", "hungarian", "norwegian", "russian", "swedish", "catalan", "dutch", "finnish",   
                      "german", "italian", "portuguese", "spanish", "arabic"))) {
        stop(paste(kind, "is not a recognized stopword list type."))
    }
    if (verbose) cat("note: using", kind, "builtin stopwords, but beware that one size may not fit all.\n")
    data(stopwords, envir = environment())
    .stopwords[[kind]]
}

#' @name .stopwords
#' @rdname stopwords
#' @docType data
NULL

#' @rdname stopwords
#' @export
stopwordsGet <- function(kind="english") {
    cat("stopwordsGet() is deprecated, use stopwords() instead.\n")
    stopwords(kind)
}

