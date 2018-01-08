
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

#' Assign the textmodel_summary class to a list
#' @param x a named list
#' @keywords internal
as.textmodel_summary <- function(x) {
    class(x) <- c('textmodel_summary', 'list')
    return(x)
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

#' Assign the textmodel_coefficients class to a numeric vector
#' @param x a numeric vector
#' @keywords internal
as.textmodel_coefficients <- function(x) {
    class(x) <- c('textmodel_coefficients', 'numeric')
    return(x)
} 

#' Impliments print methods for textmodel_statistics 
#'
#' @param x a textmodel_wordscore_statistics object
#' @param ... additional arguments not used
#' @export
print.textmodel_statistics <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    NextMethod(digits = digits, row.names = FALSE)
}

#' Assign the textmodel_coefficients class to a data.frame
#' @param x a data.frame
#' @keywords internal
as.textmodel_statistics <- function(x) {
    class(x) <- c('textmodel_statistics', 'data.frame')
    return(x)
} 
