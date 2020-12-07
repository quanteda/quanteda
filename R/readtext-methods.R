###
### Methods to extend the "readtext" package, by defining methods for 
### readtext objects
###

#' Extensions for readtext objects
#' 
#' These functions provide \pkg{quanteda} methods for \pkg{readtext} objects.
#' @name readtext-methods
#' @param x an object returned by `spacy_parse`, or (for
#'   `spacy_parse`) a [corpus] object
#' @param ... not used for these functions
#' @details 
#' `texts(x)` returns the texts from a readtext object
#'
#' `docnames(x)` returns the document names from a readtext object
#'   
#' `docvars(x, field = NULL)` returns a data.frame of the document variables
#' from a readtext object
#' 
#' `ndoc(x)` returns the number of documents from a readtext object
NULL

#' @export
texts.readtext <- function(x, groups = NULL, ...) {
    if (!is.null(groups))
        stop("groups argument not supported for texts() on a readtext object")
    result <- x[["text"]]
    names(result) <- x[["doc_id"]]
    result
}

#' @export
docvars.readtext <- function(x, field = NULL) {
    if (!is.null(field))
        warning("field argument not used for docvars on a readtext object", noBreaks. = TRUE)
    as.data.frame(x[, -which(names(x) %in% c("doc_id", "text")), drop = FALSE])
}

#' @export
docnames.readtext <- function(x) {
    x[["doc_id"]]
}

#' @export
ndoc.readtext <- function(x) {
    nrow(x)
}
