quanteda_document_delimiter <- "###END_DOCUMENT###"

#' @noRd
#' @export
texts.corpuszip <- function(x, groups = NULL, ...) {
    result <- memDecompress(x$texts, 'gzip', asChar = TRUE)
    result <- strsplit(result, paste0(quanteda_document_delimiter, "\n"))
    result <- unlist(result, use.names = FALSE)
    names(result) <- docnames(x)
    result
}

#' @noRd
#' @method as.character corpus
#' @export
as.character.corpuszip <- function(x, ...) {
    texts(x)
}


#' @noRd
#' @export
"texts<-.corpuszip" <- function(x, value) { 
    temp_texts <- texts(x)
    temp_texts <- value
    temp_texts[1 : (length(temp_texts)-1)] <- paste0(temp_texts[1 : (length(temp_texts)-1)], 
                                                     quanteda_document_delimiter)
    x$texts <- memCompress(temp_texts, 'gzip')
    # NOTE: this will not replace named elements in docnames
    x
}

#' @export
#' @noRd
docnames.corpuszip <- function(x) {
    x$docnames
}

#' @export
#' @noRd
"docnames<-.corpuszip" <- function(x, value) {
    if (!is.corpus(x))
        stop("docnames<-  only valid for corpus objects.")
    rownames(x$documents) <- x$docnames <- value
    return(x)
}


#' coerce a compressed corpus to a standard corpus
#' 
#' Recast a compressed corpus object into a standard (uncompressed) corpus object.
#' @param x a compressed \link{corpus} object
#' @export
#' @keywords internal
as.corpus <- function(x) {
    UseMethod("as.corpus")
}

#' coerce a compressed corpus to a standard corpus
#' 
#' Recast a compressed corpus object into a standard (uncompressed) corpus object.
#' @param x a compressed \link{corpus} object
#' @method as.corpus corpuszip
#' @export
as.corpus.corpuszip <- function(x) {
    corpus(texts(x), docvars = docvars(x), metacorpus = metacorpus(x))
}

setOldClass("corpuszip")
