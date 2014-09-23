
#' simple cleaning of text before processing
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
#' @param removeDigits remove numbers if \code{TRUE}
#' @param removePunct remove punctuation if \code{TRUE}
#' @param lower convert text to lower case \code{TRUE}
#' @param ... additional parameters
#' @return A character vector equal in length to the original texts, after cleaning.
#' @examples
#' clean("This is 1 sentence with 1.9 numbers in it, and one comma.", removeDigits=FALSE)
#' clean("This is 1 sentence with 1.9 numbers in it, and one comma.", lower=FALSE)
#' 
#' # for a vector of texts
#' clean(c("This is 1 sentence with 1.9 numbers in it, and one comma.", 
#'         "€1.2 billion was spent on text analysis in 2014."))
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
    cleanSingleNew <- function(s, removeDigits=TRUE, removePunct=TRUE, lower=TRUE) {
        if (removePunct) {
            s <- gsub("[[:punct:]]", "", s)
        }
        # must remove "punctuation" first
        if (removeDigits) {
            s <- gsub("[[:digit:]]", "", s)
        } 
        if (lower) {
            s <- tolower(s)
        }
        # convert multiple whitespace (up to 100 in a row) into one
        ## s <- gsub(" {2,}", " ", s)
        # remove leading and trailing whitespace and return
        gsub("^ +| +$", "", s)
    }

    return(sapply(x, cleanSingleNew, removeDigits=removeDigits, removePunct=removePunct, lower=lower,
                  USE.NAMES=FALSE))
}



#' @rdname clean
#' @export
clean.corpus <- function(x, removeDigits=TRUE, removePunct=TRUE, lower=TRUE, ...) {
    clean(texts(x), removeDigits=removeDigits, removePunct=removePunct, lower=lower, ...)
}
