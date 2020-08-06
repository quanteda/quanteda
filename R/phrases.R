#' Declare a compound character to be a sequence of separate pattern matches
#' 
#' Declares that a whitespace-separated expression consists of multiple 
#' patterns, separated by whitespace.  This is typically used as a wrapper 
#' around [pattern()] to make it explicit that the pattern elements 
#' are to be used for matches to multi-word sequences, rather than individual,
#' unordered matches to single words.
#' @param x the sequence, as a `character` object containing whitespace 
#'   separating the patterns
#' @return `phrase` returns a specially classed list whose white-spaced 
#'   elements have been parsed into separate `character` elements.
#' @export
#' @examples 
#' # make phrases from characters
#' phrase(c("a b", "c d e", "f"))
#' 
#' # from a dictionary
#' phrase(dictionary(list(catone = c("a b"), cattwo = "c d e", catthree = "f")))
#' 
#' # from a collocations object
#' (coll <- textstat_collocations(tokens("a b c a b d e b d a b")))
#' phrase(coll)
phrase <- function(x) {
    UseMethod("phrase")
}

#' @export
phrase.default <- function(x) {
    stop(friendly_class_undefined_message(class(x), "phrase"))
}

#' @noRd
#' @importFrom stringi stri_split_charclass
#' @export
phrase.character <- function(x) {
    phrase(stri_split_charclass(x, "\\p{Z}"))
}

#' @noRd
#' @export
phrase.dictionary2 <- function(x) {
    phrase(unlist(x, use.names = FALSE))
}

#' @noRd
#' @export
phrase.collocations <- function(x) {
    phrase(x[["collocation"]])
}

#' @noRd
#' @export
phrase.list <- function(x) {
    if (!all(is.character(unlist(x, use.names = FALSE))))
        stop("all list elements must be character")
    class(x) <- c("phrases", class(x))
    x
}

#' @noRd
#' @export
phrase.tokens <- function(x) {
    phrase(as.list(x))
}


#' @rdname phrase
#' @return `is.phrase` returns `TRUE` if the object was created by 
#'   [phrase()]; `FALSE` otherwise.
#' @export
is.phrase <- function(x) {
    "phrases" %in% class(x)
}

#' Print a phrase object
#' 
#' prints a phrase object in a way that looks like a standard list.
#' @keywords internal
#' @method print phrases
#' @param x a phrases (constructed by [phrase()] object to be printed
#' @param ... further arguments passed to or from other methods
#' @export
print.phrases <- function(x, ...) {
    print(as.list(x))
}

# @rdname phrase
# @method as.list phrases
# @return \code{as.list} for phrase objects returns an unclassed list.
# @export
as.list.phrases <- function(x, ...) {
    attributes(x) <- NULL
    x
}
