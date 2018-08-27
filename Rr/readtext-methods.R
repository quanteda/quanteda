###
### Methods to extend the "readtext" package, by defining methods for 
### readtext objects
###


#' @noRd
#' @export
texts.readtext <- function(x, groups = NULL, ...) {
    if (!is.null(groups))
        stop("groups argument not supported for texts() on a readtext object")
    result <- x[["text"]]
    names(result) <- x[["doc_id"]]
    result
}

#' @noRd
#' @export
docvars.readtext <- function(x, field = NULL) {
    if (!is.null(field))
        warning("field argument not used for docvars on a readtext object", noBreaks. = TRUE)
    as.data.frame(x[, -which(names(x) %in% c("doc_id", "text")), drop = FALSE])
}

#' @noRd
#' @export
docnames.readtext <- function(x) {
    x[["doc_id"]]
}

#' @noRd
#' @export
ndoc.readtext <- function(x) {
    nrow(x)
}
