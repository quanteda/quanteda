
#' @export
cleanSingleNew <- function(s, removeDigits=TRUE, removePunct=TRUE, lower=TRUE) {
    if (removeDigits) {
        s <- gsub("[[:digit:]]", "", s)
    } 
    if (removePunct) {
        s <- gsub("[[:punct:]]", "", s)
    }
    if (lower) {
        s <- tolower(s)
    }
    return(s)
}

#' clean a set of texts
#'
#' clean the texts from a character vector or from a corpus.
#' @rdname clean
#' @param x The text to be cleaned
#' @export
clean <- function(x, ...) {
    UseMethod("clean")
}

#' clean a character vector
#' 
#' clean the texts from a character vector by removing (optionally) digits and
#' punctuation, and converting to lower case
#' @rdname clean
#' @param x The character vector to be cleaned
#' @param removeDigits Should digits be removed? TRUE or FALSE
#' @param removePunct Should punctuation be removed? TRUE or FALSE
#' @param lower Convert to lower case? TRUE or FALSE
#' @export
#' @examples
#' 
#' clean(inaugTexts, removeDigits=FALSE)
clean.character <- function(x, removeDigits=TRUE, removePunct=TRUE, lower=TRUE, ...) {
    return(sapply(x, cleanSingleNew, removeDigits=removeDigits, removePunct=removePunct, lower=lower,
           USE.NAMES=FALSE))
}


#' clean a corpus
#' 
#' clean the texts in a corpus by removing (optionally) digits and punctuation,
#' and converting to lower case. Returns a corpus with the texts cleaned.
#' @rdname clean
#' @param x The character vector to be cleaned
#' @param removeDigits Should digits be removed? TRUE or FALSE
#' @param removePunct Should punctuation be removed? TRUE or FALSE
#' @param lower Convert to lower case? TRUE or FALSE
#' @return  a corpus with the texts cleaned.
#' @export
#' @examples
#' 
#' clean(inaugCorpus, removeDigits=FALSE)
clean.corpus <- function(x, removeDigits=TRUE, removePunct=TRUE, lower=TRUE, ...) {
    texts(x) <- clean(texts(x), removeDigits=removeDigits, removePunct=removePunct, lower=lower)
    return(x)
}
