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
#' @importFrom spacyr spacy_parse
#' @details `spacy_parse(x, ...)` and `spacy_tokenize(x, ...)` work directly on 
#' \pkg{quanteda} [corpus] objects.
#' 
#' @param x an object returned by `spacy_parse`, or (for
#'   `spacy_parse`) a [corpus] object
#' @param ... unused except for `spacy_parse`, in which case it passes
#'   through extra arguments to that function
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

#' @rdname spacyr-methods
#' @importFrom spacyr spacy_parse
#' @export
spacy_parse.corpus <- function(x, ...) spacy_parse(texts(x), ...)

#' @rdname spacyr-methods
#' @importFrom spacyr spacy_tokenize
#' @export
spacy_tokenize.corpus <- function(x, ...) spacy_tokenize(texts(x), ...)

#' @rdname spacyr-methods
#' @details
#' `docnames()` returns the document names
#' 
#' @export
docnames.spacyr_parsed <- function(x) {
    unique(x$doc_id)
}

#' @rdname spacyr-methods
#' @details
#' `ndoc()` returns the number of documents
#' 
#' @export
ndoc.spacyr_parsed <- function(x) {
    length(docnames(x))
}

#' @rdname spacyr-methods
#' @details
#' `ntoken()` returns the number of tokens by document
#' 
#' @export
ntoken.spacyr_parsed <- function(x, ...) {
    lengths(split(x$token, x$doc_id))
}

#' @rdname spacyr-methods
#' @details
#' `ntype()` returns the number of types (unique tokens) by document
#' @export
ntype.spacyr_parsed <- function(x, ...) {
    vapply(split(x$token, x$doc_id), function(y) length(unique(y)), integer(1))
}

#' @rdname spacyr-methods
#' @details
#' `nsentence()` returns the number of sentences by document
#' @export
nsentence.spacyr_parsed <- function(x, ...) {
    sapply(split(x, x$doc_id), function(y) length(unique(y$sentence_id)))
}
