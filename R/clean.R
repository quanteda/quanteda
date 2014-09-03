
#' @export
cleanSingleNew <- function(s, removeDigits=TRUE, removePunct=TRUE, lower=TRUE) {
    if (removeDigits){
        s <- gsub("[[:digit:]]", "", s)
    } 
    if (removePunct){
        s <- gsub("[[:punct:]]", "", s)
    }
    if (lower){
        s <- tolower(s)
    }
    return(s)
}

#' @export
clean <- function(x, ...) {
    UseMethod("clean")
}

#' @export
clean.character <- function(s, removeDigits=TRUE, removePunct=FALSE, lower=TRUE, ...) {
    return(lapply(s, cleanSingleNew, removeDigits=removeDigits, removePunct=removePunct, lower=lower))
}

#' @export
clean.corpus <- function(corpus, removeDigits=TRUE, removePunct=FALSE, lower=TRUE, ...) {
    texts(corpus) <- clean(texts(corpus), removeDigits=removeDigits, removePunct=removePunct, lower=lower)
    return(corpus)
}
