
#' @export
cleanSingleNew <- function(s, removeDigits=TRUE, removePunct=TRUE, lower=TRUE) {
    if (removeDigits){
        s <- gsub("[[:digit:]]", "", s, perl = FALSE)
    } 
    if (removePunct){
        s <- gsub("[[:punct:]]", "", s, perl = FALSE)
    }
    if (lower){
        s <- tolower(s)
    }
    return(s)
}


#' @export
cleanSingle <- function(s, removeDigits=TRUE, removePunct=TRUE, lower=TRUE) {
    if (strict) {
        # lowercases and discards everything except spaces and alphabet
        # possibly the most frequent use-case for QTA
        s <- tolower(s)
        s <- gsub("[[:space:]+]", ' ', s)
        s <- gsub("[^[:lower:] *]", '', s)
        
        # had difficulty combing these into a single command, can revisit if
        # performance is poor
        s <- gsub(" +", ' ', s)
    }
    else {
        if (removeDigits){
            s <- gsub("[[:digit:]]", "", s, perl = FALSE)
        } 
        if (removePunct){
            s <- gsub("[[:punct:]]", "", s, perl=TRUE)
        }
        if (lower){
            s <- tolower(s)
        }
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
