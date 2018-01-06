setClass("textmodel_summary", contains = "list")
setClass("textmodel_coefficients", contains = "numeric")

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
print.textmodel_summary <- function(x, digits = 2, ...) {
    for (n in names(x)) {
        cat(stri_trans_totitle(n), ':\n', sep = '')
        print(x[[n]], digits = digits)
        cat('\n')
    }
}

#' Impliments print methods for textmodel_features 
#'
#' @param x a textmodel_features object
#' @param ... additional arguments not used
#' @export
print.textmodel_coefficients <- function(x, digits = 2, ...) {
    cat("(showing first", length(x), "features)\n\n")
    print(unclass(x), digits = digits)
}



