###
### Methods to extend the "readtext" package, by defining methods for 
### readtext objects
###

#' old function to read texts from files
#' 
#' This function was removed from \pkg{quanteda} in v.0.9.9 and moved to a 
#' separate package, \pkg{readtext}.  Please use \code{readtext} instead
#' of \code{textfile}.
#' @param ... any collection of objects
#' @keywords internal deprecated
#' @export
textfile <- function(...) {
    .Defunct("textfile", package = "readtext")
}

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

    
