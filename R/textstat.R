#' Statistics for textual data
#' 
#' The `textstat_*()` functions formerly in \pkg{quanteda} have now been moved 
#' to the \pkg{quanteda.textstats} package.
#' @name textstats
#' @seealso `quanteda.textstats::quanteda.textstats-package`
NULL

#' Check if an object is collocations
#' 
#' Function to check if an object is a collocations object, created by
#' `quanteda.textstats::textstat_collocations()`.
#' @param x object to be checked
#' @export
#' @return `TRUE` if the object is of class `collocations`, `FALSE` otherwise
is.collocations <- function(x) {
    "collocations" %in% class(x)
}
