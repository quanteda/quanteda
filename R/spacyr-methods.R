###
### Methods to extend the "spacyr" package, by defining methods for 
### spacy_parsed objects
###

#' extensions for and from spacy_parse objects
#' 
#' These functions provide \pkg{quanteda} methods for \pkg{spacyr} objects, and
#' also extend \link[spacyr]{spacy_parse} to work with \link{corpus} objects.
#' @name spacyr-methods
#' @importFrom spacyr spacy_parse
#' @section Usage:
#' \code{docnames(x)} returns the document names
#' 
#' \code{ndoc(x)} returns the number of documents
#' 
#' \code{ntoken(x, ...)} returns the number of tokens by document
#' 
#' \code{ntype(x, ...)} returns the number of types (unique tokens) by document
#' 
#' \code{spacy_parse(x, ...)} is also defined for a \pkg{quanteda} \link{corpus}
#' 
#' @param x an object returned by \code{spacy_parse}, or (for
#'   \code{spacy_parse}) a \link{corpus} object
#' @param ... unused except for \code{spacy_parse}, in which case it passes
#'   through extra arguments to that function
#' @examples 
#' \dontrun{
#' library("spacyr")
#' spacy_initialize()
#' 
#' txt <- c(doc1 = "And now, now, now for something completely different.",
#'          doc2 = "Jack and Jill are children.")
#' parsed <- spacy_parse(txt)
#' ntype(parsed)
#' ntoken(parsed)
#' ndoc(parsed)
#' docnames(parsed)
#' 
#' corpus_subset(data_corpus_inaugural, Year <= 1793) %>% spacy_parse()
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

#' @export
spacyr::spacy_parse

#' @rdname spacyr-methods
#' @importFrom spacyr spacy_parse
#' @export
spacy_parse.corpus <- function(x, ...) spacy_parse(texts(x), ...)

#' @rdname as.tokens
#' @param use_lemma logical; if \code{TRUE}, use the lemma rather than the raw
#'   token
#' @param include_pos character; whether and which part-of-speech tag to use:
#'   \code{"none"} do not use any part of speech indicator, \code{"pos"} use the
#'   \code{pos} variable, \code{"tag"} use the \code{tag} variable.  The POS
#'   will be added to the token after \code{"concatenator"}.
#' @export
as.tokens.spacyr_parsed <- function(x, concatenator = "/", 
                                    include_pos = c("none", "pos", "tag"), 
                                    use_lemma = FALSE) {
    token_index <-  if (use_lemma) "lemma" else "token"
    
    include_pos <- match.arg(include_pos)
    if (include_pos != "none") {
        x[[token_index]] <- 
            paste(x[[token_index]], x[[include_pos]], sep = concatenator)
    }
            
    as.tokens(base::split(x[[token_index]], 
                          factor(x[["doc_id"]], levels = unique(x[["doc_id"]]))))
}

