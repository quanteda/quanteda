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
#' [quanteda.textstats::textstat_collocations()].
#' @param x object to be checked
#' @export
#' @return `TRUE` if the object is of class `collocations`, `FALSE` otherwise
is.collocations <- function(x) {
    "collocations" %in% class(x)
}

# class definitions and class functions for textstat_proxy --------

# transitional only, to get quanteda.textstats passing on CRAN

#' @title textstat_simil/dist classes
#' @description Sparse classes for similarity and distance matrices created by
#'   [quanteda.textstats::textstat_simil()] and
#'   [quanteda.textstats::textstat_dist()].
#' @rdname textstat_proxy-class
#' @export
#' @keywords internal textstat
#' @slot .Data a sparse \pkg{Matrix} object, symmetric if selection is
#'   `NULL`
#' @slot method the method used for computing similarity or distance
#' @slot min_simil numeric; a threshold for the similarity values below which similarity
#'   values are not computed
#' @slot margin identifies the margin of the dfm on which similarity or
#'   difference was computed:  `"documents"` for documents or
#'   `"features"` for word/term features.
#' @slot type either `"textstat_simil"` or `"textstat_dist"`
#' @seealso [quanteda.textstats::textstat_simil()]
setClass("textstat_proxy", contains = "Matrix",
         slots = c(method = "character",
                   margin = "character",
                   type = "character"))

#' @rdname textstat_proxy-class
#' @slot selection target units, if any
setClass("textstat_dist", contains = c("textstat_proxy", "dgeMatrix"))

#' @rdname textstat_proxy-class
setClass("textstat_dist_symm", contains = c("textstat_proxy", "dspMatrix"))

#' @rdname textstat_proxy-class
setClass("textstat_simil", contains = c("textstat_proxy", "dgeMatrix"))

#' @rdname textstat_proxy-class
#' @export
setClass("textstat_simil_symm", contains = c("textstat_proxy", "dspMatrix"))

#' @rdname textstat_proxy-class
#' @export
setClass("textstat_simil_sparse", contains = c("textstat_proxy", "dgTMatrix"),
         slots = c(min_simil = "numeric"))

#' @rdname textstat_proxy-class
setClass("textstat_simil_symm_sparse", contains = c("textstat_proxy", "dsTMatrix"),
         slots = c(min_simil = "numeric"))

#' @rdname textstat_proxy-class
validate_min_simil <- function(object) {
    if (object@min_simil < -1.0 || object@min_simil > 1.0) {
        paste("min_simil must range from -1.0 to 1.0")
    } else {
        return(TRUE)
    }
}

setValidity("textstat_simil_sparse", function(object) {
    validate_min_simil(object)
})

setValidity("textstat_simil_symm_sparse", function(object) {
    validate_min_simil(object)
})
