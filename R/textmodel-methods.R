
#' Impliments print methods for textmodel_summary 
#'
#' @param x a textmodel_summary object
#' @param ... additional arguments not used
#' @export
print.textmodel_summary <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    label <- stri_trans_totitle(stri_replace_all_fixed(names(x), '.', ' '))
    for (i in seq_along(x)) {
        cat(label[i], ':\n', sep = '')
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
    cat("(showing first", length(x), "elements)\n")
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
    NextMethod(digits = digits, row.names = TRUE)
}

#' Assign the textmodel_coefficients class to a data.frame
#' @param x a data.frame
#' @keywords internal
as.textmodel_statistics <- function(x) {
    class(x) <- c('textmodel_statistics', 'data.frame')
    return(x)
} 
