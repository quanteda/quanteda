#' Drop small values from an object
#'
#' Generic for dropping small values (hard thresholding).
#'
#' @param x object to threshold.
#' @param ... arguments passed to methods.
#'
#' @export
drop0 <- function(x, ...) {
  UseMethod("drop0")
}

#' Drop small values from a dfm (hard thresholding)
#'
#' Zeroes out values in a dfm below a threshold while preserving document
#' variables and names, unlike manual subsetting that drops dfm attributes.
#'
#' @param x a dfm object.
#' @param tol numeric; tolerance for values to drop (set to zero).
#'   Cells with absolute value < tol are set to zero.
#' @param ... additional arguments passed to \code{Matrix::drop0()}.
#'
#' @return A dfm with small cell values set to zero, with docvars preserved.
#'
#' @details
#' This performs "hard thresholding" (matrix sparsification) on the dfm:
#' it increases the number of zero cells but does not change the dfm
#' dimensions. For removing features entirely, use \code{\link{dfm_trim}}.
#'
#' @keywords internal
#' @export
drop0.dfm <- function(x, tol = 0, ...) {
    x <- quanteda::as.dfm(x)

    # save all attributes
    attrs <- attributes(x)

    # coerce to dgCMatrix and apply thresholding
    x <- methods::as(x, "dgCMatrix")
    x <- Matrix::drop0(x, tol = tol, ...)

    # rebuild dfm preserving all original attributes
    rebuild_dfm(x, attrs)
}
