###
### Methods to extend the "spacyr" package, by defining methods for 
### spacy_parsed objects
###

#' extensions of methods defined in the quanteda package
#' 
#' Extensions to quanteda functions.  You must have attached \pkg{quanteda} for these
#' to work.
#' @name spacyr-methods
#' @section Usage:
#' \code{docnames(x)} returns the document names
#' 
#' \code{ndoc(x)} returns the number of documents
#' 
#' \code{ntoken(x, ...)} returns the number of tokens by document
#' 
#' \code{ntype(x, ...)} returns the number of types (unique tokens) by document
#' 
#' @param x an object returned by \code{spacy_parse}
#' @param ... unused
#' @examples 
#' \dontrun{
#' require(spacyr)
#' spacy_initialize()
#' txt <- c(doc1 = "And now, now, now for something completely different.",
#'          doc2 = "Jack and Jill are children.")
#' parsed <- spacy_parse(txt)
#' ntype(parsed)
#' ntoken(parsed)
#' ndoc(parsed)
#' docnames(parsed)
#' }
NULL

#' @rdname spacyr-methods
#' @details
#' \code{docnames} returns the document names
#' 
#' @noRd
#' @export
docnames.spacyr_parsed <- function(x) {
    unique(x$doc_id)
}


#' @rdname spacyr-methods
#' @details
#' \code{ndoc} returns the number of documents
#' 
#' @noRd
#' @export
ndoc.spacyr_parsed <- function(x) {
    length(docnames(x))
}

#' @rdname spacyr-methods
#' @details
#' \code{ntoken} returns the number of tokens by document
#' 
#' @noRd
#' @export
ntoken.spacyr_parsed <- function(x, ...) {
    lengths(split(x$token, x$doc_id))
}

#' @rdname spacyr-methods
#' @details
#' \code{ntype} returns the number of types (unique tokens) by document
#' 
#' @noRd
#' @export
ntype.spacyr_parsed <- function(x, ...) {
    sapply(split(x$token, x$doc_id), function(y) length(unique(y)))
}

