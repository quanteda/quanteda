
#' print method for summary.textmodel
#'
#' @param x a \code{summary.textmodel} object
#' @param digits minimal number of \emph{significant digits}, see
#'   \code{\link{print.default}}
#' @param ... additional arguments not used
#' @method print summary.textmodel
#' @export
print.summary.textmodel <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    label <- stri_trans_totitle(stri_replace_all_fixed(names(x), '.', ' '))
    cat("\n")
    for (i in seq_along(x)) {
        cat(label[i], ':\n', sep = '')
        print(x[[i]], digits = digits)
        cat("\n")
    }
}

#' Assign the summary.textmodel class to a list
#' @param x a named list
#' @keywords internal
as.summary.textmodel <- function(x) {
    class(x) <- c('summary.textmodel', 'list')
    return(x)
} 

#' Print methods for textmodel features estimates
#'
#' @param x a textmodel_features object
#' @param digits minimal number of \emph{significant digits}, see
#'   \code{\link{print.default}}
#' @param ... additional arguments not used
#' @method print coef.textmodel
#' @export
print.coef.textmodel <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    x <- unclass(x)
    cat("(showing first", length(x), "elements)\n")
    NextMethod(digits = digits)
}

#' Assign the textmodel_coefficients class to a numeric vector
#' @param x a numeric vector
#' @keywords internal
as.coef.textmodel <- function(x) {
    class(x) <- c('coef.textmodel', 'numeric')
    return(x)
} 

#' Impliments print methods for textmodel_statistics 
#'
#' @param x a textmodel_wordscore_statistics object
#' @param digits minimal number of \emph{significant digits}, see
#'   \code{\link{print.default}}
#' @param ... additional arguments not used
#' @export
print.textmodel_statistics <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    NextMethod(digits = digits, row.names = TRUE)
}

#' Assign the textmodel_coefficients class to a data.frame
#' @param x a data.frame
#' @keywords internal
as.textmodel_statistics <- function(x) {
    class(x) <- c('statistics.textmodel', 'data.frame')
    return(x)
} 
