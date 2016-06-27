.stopwords <- NULL


#' access built-in stopwords
#' 
#' This function retrieves stopwords from the type specified in the \code{kind} 
#' argument and returns the stopword list as a character vector The default is 
#' English.
#' 
#' The stopword list are SMART English stopwords from the SMART information
#' retrieval system (obtained from 
#' \url{http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop})
#' and a set of stopword lists from the Snowball stemmer project in different
#' languages (obtained from 
#' \url{http://svn.tartarus.org/snowball/trunk/website/algorithms/} -- 
#' see the stop.txt files in each subdirectory).
#' Supported languages are arabic, danish, dutch, english, finnish, french,
#' german, hungarian, italian, norwegian, portuguese, russian, spanish, and 
#' swedish. Language names are case sensitive.
#' @rdname stopwords
#' @section A note of caution:
#'  Stop words are an arbitrary choice imposed by the
#'   user, and accessing a pre-defined list of words to ignore does not mean
#'   that it will perfectly fit your needs. You are strongly encourged to
#'   inspect the list and to make sure it fits your particular requirements.  The 
#'   built-in English stopword list does not contain "will", for instance, because
#'   of its multiple meanings, but you might want to include this word for your
#'   own application.
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
#'
#' # adding to the built-in stopword list
#' toks <- tokenize("The judge will sentence Mr. Adams to nine years in prison", removePunct = TRUE)
    #' removeFeatures(toks, c(stopwords("english"), "will", "mr", "nine"))
stopwords <- function(kind="english", verbose=FALSE) {
    if (!(kind %in% c("english", "SMART", "danish", "french", "hungarian", "norwegian", "russian", "swedish", "catalan", "dutch", "finnish",   
                      "german", "italian", "portuguese", "spanish", "arabic"))) {
        stop(paste(kind, "is not a recognized stopword list type."))
    }
    if (verbose) catm("note: using", kind, "builtin stopwords, but beware that one size may not fit all.\n")
    # data(stopwords, envir = environment())
    quanteda::.stopwords[[kind]]
}

#' @name .stopwords
#' @rdname stopwords
#' @docType data
NULL



# @rdname removeFeatures
# @export
stopwordsRemove <- function(x, stopwords=NULL, verbose=TRUE) {
    catm("stopwordsRemove is deprecated, use removeFeatures instead.")
    UseMethod("removeFeatures")
}

# @rdname stopwords
# @export
stopwordsGet <- function(kind="english") {
    catm("stopwordsGet() is deprecated, use stopwords() instead.\n")
    stopwords(kind)
}

