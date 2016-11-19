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
    names(result) <- row.names(x)
    result
}

#' @noRd
#' @export
docvars.readtext <- function(x, field = NULL) {
    if (!is.null(field))
        warning("field argument not used for docvars on a readtext object", noBreaks. = TRUE)
    as.data.frame(x[, -which(names(x)=="text"), drop = FALSE])
}

#' @noRd
#' @export
docnames.readtext <- function(x) {
    row.names(x)
}

#' @noRd
#' @export
ndoc.readtext <- function(x) {
    nrow(x)
}

    
