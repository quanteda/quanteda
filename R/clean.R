
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

#' @export
clean <- function(x, ...) {
    UseMethod("clean")
}

#' @export
clean.character <- function(x, removeDigits=TRUE, removePunct=TRUE, lower=TRUE, ...) {
    return(sapply(x, cleanSingleNew, removeDigits=removeDigits, removePunct=removePunct, lower=lower,
           USE.NAMES=FALSE))
}

#' @export
clean.corpus <- function(x, removeDigits=TRUE, removePunct=TRUE, lower=TRUE, ...) {
    texts(x) <- clean(texts(x), removeDigits=removeDigits, removePunct=removePunct, lower=lower)
    return(x)
}
