###
### Methods to extend the "readtext" package, by defining methods for 
### readtext objects
###

#' Extensions for readtext objects
#' 
#' These functions provide \pkg{quanteda} methods for \pkg{readtext} objects.
#' @name readtext-methods
#' @param x an object read by `readtext()` from the \pkg{readtext} package
NULL

#' @rdname readtext-methods
#' @returns 
#' `docnames(x)` returns a character vector of the document names from a
#' readtext object
#' @export
docnames.readtext <- function(x) {
    x[["doc_id"]]
}

#' @rdname readtext-methods
#' @inheritParams docvars
#' @returns `docvars(x, field = NULL)` returns a data.frame of the document
#'   variables from a readtext object or a vector if `field` is a single value
#' @export
docvars.readtext <- function(x, field = NULL) {
    if (!is.null(field))
        warning("field argument not used for docvars on a readtext object", noBreaks. = TRUE)
    as.data.frame(x[, -which(names(x) %in% c("doc_id", "text")), drop = FALSE])
}

#' @rdname readtext-methods
#' @returns 
#' `ndoc(x)` returns the number of documents from a readtext object
#' @export
ndoc.readtext <- function(x) {
    nrow(x)
}
