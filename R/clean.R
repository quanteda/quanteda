
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

#' clean: simple pre-processing cleanup
#' 
#' \code{clean} removes punctuation and digits from text, using the regex 
#' character classes for punctuation and digits. \code{clean} uses the standard R
#' function \code{tolower} to convert the text to lower case. Each of these 
#' steps is optional, but switched on by default, so for example, to remove 
#' punctuation and convert to lower, but keep digits, the command would be: 
#' \code{clean(mytexts, removeDigits=FALSE)}
#' @rdname clean
#' @param x The object to be cleaned. Can be either a character vector or a 
#'   corpus object. If x is a corpus, \code{clean} returns a copy of the x with 
#'   the texts cleaned.
#' @param removeDigits Should digits be removed? TRUE or FALSE
#' @param removePunct Should punctuation be removed? TRUE or FALSE
#' @param lower Convert to lower case? TRUE or FALSE
#' @examples
#' 
#' #convert a set of texts to lower case and remove 
#' punctuation, keeping digits
#' 
#' clean(inaugTexts, removeDigits=FALSE)
#' 
#' # remove digits from a corpus
#' clean(inaugTexts, removePunct=FALSE, lower=FALSE)
#' 
#' @export
clean <- function(x, ...) {
    UseMethod("clean")
}



#' @rdname clean
#' @export
clean.character <- function(x, removeDigits=TRUE, removePunct=TRUE, lower=TRUE, ...) {
    if(!(removeDigits | removePunct | lower)){
        warning("all options FALSE, text unchanged")
    }
    return(sapply(x, cleanSingleNew, removeDigits=removeDigits, removePunct=removePunct, lower=lower,
           USE.NAMES=FALSE))
}



#' @rdname clean
#' @export
clean.corpus <- function(x, removeDigits=TRUE, removePunct=TRUE, lower=TRUE, ...) {
    if(!(removeDigits | removePunct | lower)){
        warning("all options FALSE, text unchanged")
    }
    texts(x) <- clean(texts(x), removeDigits=removeDigits, removePunct=removePunct, lower=lower)
    return(x)
}
