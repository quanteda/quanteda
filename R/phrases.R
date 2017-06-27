#' declare a compound character to be a sequence of separate pattern matches
#' 
#' Declares that a whitespace-separated expression consists of multiple patterns, separated by whitespace.
#' @param ... the sequence, as a \code{character} object containing whitespace separating the patterns.
#' @export
#' @examples 
#' # make phrases from characters
#' phrase(c("a b", "c d e", "f"))
#' 
#' # from a dictionary
#' phrase(dictionary(catone = c("a b"), cattwo = "c d e", catthree = "f"))
#' 
#' # from a collocations object
#' (coll <- textstat_collocations(tokens("a b c a b d e b d a b")))
#' phrase(coll)
phrase <- function(...) {
    UseMethod("phrase")
}

#' @noRd
#' @export
phrase.character <- function(...) {
    x <- lapply(as.list(...), 
                function(y) as.character(tokens(y, what = "fasterword", remove_punct = FALSE)))
    phrase(x)    
}

#' @noRd
#' @export
phrase.dictionary2 <- function(...) {
    phrase(unlist(..., use.names = FALSE))
}

#' @noRd
#' @export
phrase.collocations <- function(...) {
    x <- as.data.frame(do.call(rbind, list(...)))
    phrase(x[["collocation"]])
}

#' @noRd
#' @export
phrase.list <- function(...) {
    x <- as.list(...)
    class(x) <- c("phrases", class(x))
    x
}

#' @noRd
#' @export
phrase.tokens <- function(...) {
    x <- as.list(...)
    class(x) <- c("phrases", class(x))
    x
}


#' @rdname phrase
#' @return \code{is.phrase} returns \code{TRUE} if the object is a
#'   \link{phrase}s class; \code{FALSE} otherwise.
#' @export
is.phrase <- function(x) {
    "phrases" %in% class(x)
}

#' print a phrase object
#' 
#' prints a phrase object in a way that looks like a standard list.
#' @keywords internal
#' @method print phrases
#' @param x a phrases (constructed by \code{\link{phrase}} object to be printed
#' @param ... further arguments passed to or from other methods
#' @export
print.phrases <- function(x, ...) {
    print(as.list(x))
}

#' @noRd
#' @method as.list phrases
#' @keywords internal
#' @export
as.list.phrases <- function(x, ...) {
    attributes(x) <- NULL
    x
}
