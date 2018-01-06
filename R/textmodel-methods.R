setClass("textmodel_summary", contains = "list")
setClass("textmodel_coefficients", contains = "numeric")
setClass("textmodel_statistics", contains = "data.frame")

#' @rdname textmodel-internal
#' @keywords internal
#' @export
setMethod("show", signature(object = "textmodel_summary"), 
       function(object) print(object))

#' Impliments print methods for textmodel_summary 
#'
#' @param x a textmodel_summary object
#' @param ... additional arguments not used
#' @export
print.textmodel_summary <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    for (i in seq_along(x)) {
        cat(stri_trans_totitle(names(x[i])), ':\n', sep = '')
        print(x[[i]], digits = digits)
        cat('\n')
    }
}

#' Impliments print methods for textmodel_features 
#'
#' @param x a textmodel_features object
#' @param ... additional arguments not used
#' @export
print.textmodel_coefficients <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    x <- unclass(x)
    cat("(showing first", length(x), "features)\n\n")
    NextMethod(digits = digits)
}

#' Impliments print methods for textmodel_statistics 
#'
#' @param x a textmodel_wordscore_statistics object
#' @param ... additional arguments not used
#' @export
print.textmodel_statistics <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    NextMethod(digits = digits, row.names = FALSE)
}

