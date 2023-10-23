#' Declare a pattern to be a sequence of separate patterns
#'
#' Declares that a character expression consists of multiple patterns, separated
#' by an element such as whitespace.  This is typically used as a wrapper around
#' [pattern()] to make it explicit that the pattern elements are to be used for
#' matches to multi-word sequences, rather than individual, unordered matches to
#' single words.
#' @param x character, [dictionary], list, collocations, or tokens object; the
#'   compound patterns to be treated as a sequence separated by `separator`.
#'   For list, collocations, or tokens objects, use `as.phrase()`.
#' @param separator	character; the character in between the patterns. This
#'   defaults to " ".  For `phrase()` only.
#' @return `phrase()` and `as.phrase()` return a specially classed list whose
#'   elements have been split into separate `character` (pattern) elements.
#' @seealso [as.phrase()]
#' @export
#' @examples
#' # make phrases from characters
#' phrase(c("natural language processing"))
#' phrase(c("natural_language_processing", "text_analysis"), separator = "_")
#'
#' # from a dictionary
#' phrase(dictionary(list(catone = c("a b"), cattwo = "c d e", catthree = "f")))
#' 
#' # from a list
#' as.phrase(list(c("natural", "language", "processing")))
#' 
#' # from tokens
#' as.phrase(tokens("natural language processing"))
#
phrase <- function(x, separator = " ") {
    UseMethod("phrase")
}

#' @export
phrase.default <- function(x, separator = " ") {
    check_class(class(x), "phrase")
}

#' @noRd
#' @importFrom stringi stri_split_charclass
#' @export
phrase.character <- function(x, separator = " ") {
    separator <- check_character(separator, 1, 1, 1, 1)
    x <- stri_split_fixed(x, separator)
    class(x) <- c("phrases", class(x))
    return(x)
}

#' @noRd
#' @export
phrase.dictionary2 <- function(x, separator = " ") {
    phrase(unlist(x, use.names = FALSE), separator = " ")
}

#' @rdname phrase
#' @export
as.phrase <- function(x) {
    UseMethod("as.phrase")
}

#' @export
as.phrase.default <- function(x) {
    check_class(class(x), "as.phrase")
}

#' @noRd
#' @export
as.phrase.collocations <- function(x) {
    phrase(x[["collocation"]])
}

#' @noRd
#' @export
phrase.collocations <- function(x, separator = " ") {
    as.phrase(x)
}

#' @noRd
#' @export
as.phrase.dictionary2 <- function(x) {
  phrase(unlist(x, use.names = TRUE), separator = " ")
}

#' @noRd
#' @export
as.phrase.list <- function(x) {
    if (!all(unlist(lapply(x, is.character), use.names = FALSE)))
        stop("all list elements must be character")
    class(x) <- c("phrases", class(x))
    return(x)
}

#' @noRd
#' @export
phrase.list <- function(x, separator = " ") {
    lifecycle::deprecate_stop(
        when = "3.0", 
        what = "phrase.list()",
        with = "as.phrase()"
    )
}

#' @noRd
#' @export
as.phrase.tokens <- function(x) {
    as.phrase(as.list(x))
}

#' @noRd
#' @export
phrase.tokens <- function(x, separator = " ") {
    lifecycle::deprecate_stop(
        when = "3.0", 
        what = "phrase.tokens()",
        with = "as.phrase()"
    )
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
