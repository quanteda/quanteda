###
### Methods to extend the "spacyr" package, by defining methods for 
### spacy_parsed objects
###

#' Extensions for and from spacy_parse objects
#' 
#' These functions provide \pkg{quanteda} methods for \pkg{spacyr} objects, and
#' also extend [spacy_parse][spacyr::spacy_parse] and
#' [spacy_tokenize][spacyr::spacy_tokenize] to work directly with [corpus]
#' objects.
#' @name spacyr-methods
#' @details `spacy_parse(x, ...)` and `spacy_tokenize(x, ...)` work directly on 
#' \pkg{quanteda} [corpus] objects.
#' 
#' @param x an object returned by `spacy_parse`, or (for
#'   `spacy_parse`) a [corpus] object
#' @param ... not used for these functions
#' @details 
#' `docnames(x)` returns the document names
#' 
#' `ndoc(x)` returns the number of documents
#' 
#' `ntoken(x, ...)` returns the number of tokens by document
#' 
#' `ntype(x, ...)` returns the number of types (unique tokens) by document
#' 
#' `nsentence(x)` returns the number of sentences by document
#' @examples 
#' \dontrun{
#' library("spacyr")
#' spacy_initialize()
#' 
#' corp <- corpus(c(doc1 = "And now, now, now for something completely different.",
#'                  doc2 = "Jack and Jill are children."))
#' spacy_tokenize(corp)
#' (parsed <- spacy_parse(corp))
#' 
#' ntype(parsed)
#' ntoken(parsed)
#' ndoc(parsed)
#' docnames(parsed)
#' }
NULL

#' @export
docnames.spacyr_parsed <- function(x) {
    unique(x$doc_id)
}

#' @export
ndoc.spacyr_parsed <- function(x) {
    length(docnames(x))
}

#' @export
ntoken.spacyr_parsed <- function(x, ...) {
    lengths(split(x$token, x$doc_id))
}

#' @export
ntype.spacyr_parsed <- function(x, ...) {
    vapply(split(x$token, x$doc_id), function(y) length(unique(y)), integer(1))
}

#' @export
nsentence.spacyr_parsed <- function(x, ...) {
    sapply(split(x, x$doc_id), function(y) length(unique(y$sentence_id)))
}
