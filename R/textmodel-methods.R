# generic methods -----------

#' print method for summary.textmodel
#'
#' @param x a \code{summary.textmodel} object
#' @param digits minimal number of \emph{significant digits}, see
#'   \code{\link{print.default}}
#' @param ... additional arguments not used
#' @method print summary.textmodel
#' @keywords textmodel internal
#' @export
print.summary.textmodel <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    label <- stri_trans_totitle(stri_replace_all_fixed(names(x), ".", " "))
    # print the formatted remaining elements
    for (i in seq_along(x)) {
        cat("\n")
        cat(label[i], ':\n', sep = '')
        if (stri_detect_fixed(label[i], "Feature")) {
            print(t(x[[i]]), digits = digits)
        } else {
            print(x[[i]], digits = digits)    
        }
        
    }
}

#' Assign the summary.textmodel class to a list
#' @param x a named list
#' @keywords internal
#' @export
as.summary.textmodel <- function(x) {
    class(x) <- c("summary.textmodel", "list")
    x
} 

# 
# #' Print methods for textmodel features estimates
# #'
# #' @param x a textmodel_features object
# #' @param digits minimal number of \emph{significant digits}, see
# #'   \code{\link{print.default}}
# #' @param n how many coefficients to print before truncating
# #' @param ... additional arguments not used
# #' @method print coef.textmodel
# #' @export
# print.coef.textmodel <- function(x, digits = max(3L, getOption("digits") - 3L), n = 30L, ...) {
#     x <- unclass(x)
#     if (length(x) > n)
#         cat("(showing first", length(x), "elements)\n")
#     NextMethod(digits = digits)
# }
# 
# #' Assign the textmodel_coefficients class to a numeric vector
# #' @param x a numeric vector
# #' @keywords internal
# as.coef.textmodel <- function(x) {
#     class(x) <- c("coef.textmodel", "numeric")
#     return(x)
# } 

#' Print methods for textmodel features estimates

#' This is a helper function used in \code{print.summary.textmodel}.
#' @param x a coefficients_textmodel object
#' @param digits minimal number of \emph{significant digits}, see
#'   \code{\link{print.default}}
#' @param ... additional arguments not used
#' @method print coefficients_textmodel
#' @keywords internal textmodel
#' @export
print.coefficients_textmodel <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    if (is.data.frame(x)) {
        n <- nrow(x)
        x <- as.data.frame(x)
    } else {
        n <- length(x)
        x <- unclass(x)
    }
    cat("(showing first", n, "elements)\n")
    print(x, digits = digits)
}

#' Coerce various objects to coefficients_textmodel

#' This is a helper function used in \code{summary.textmodel_*}.
#' @param x an object to be coerced
#' @importFrom stats coefficients
#' @importFrom stats coef
#' @keywords internal
#' @export
as.coefficients_textmodel <- function(x) {
    UseMethod('as.coefficients_textmodel')
} 

#' @noRd
#' @method as.coefficients_textmodel data.frame
#' @keywords internal
#' @export
as.coefficients_textmodel.data.frame <- function(x) {
    class(x) <- c("coefficients_textmodel", "data.frame")
    return(x)
} 

#' @noRd
#' @method as.coefficients_textmodel numeric
#' @keywords internal
#' @export
as.coefficients_textmodel.numeric <- function(x) {
    class(x) <- c("coefficients_textmodel", "numeric")
    return(x)
} 

#' @noRd
#' @method as.coefficients_textmodel numeric
#' @keywords internal
#' @export
as.coefficients_textmodel.matrix <- function(x) {
    as.coefficients_textmodel(as.data.frame(x))
} 

#' Implements print methods for textmodel_statistics 
#'
#' @param x a textmodel_wordscore_statistics object
#' @param digits minimal number of \emph{significant digits}, see
#'   \code{\link{print.default}}
#' @param ... further arguments passed to or from other methods
#' @method print statistics_textmodel
#' @keywords internal textmodel
#' @export
print.statistics_textmodel <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    NextMethod(digits = digits, row.names = TRUE)
}

#' Coerce various objects to statistics_textmodel
#'
#' This is a helper function used in \code{summary.textmodel_*}.
#' @param x an object to be coerced
#' @keywords internal textmodel
#' @export
as.statistics_textmodel <- function(x) {
    UseMethod("as.statistics_textmodel")
} 

#' @noRd
#' @method as.statistics_textmodel data.frame
#' @keywords internal textmodel
#' @export
as.statistics_textmodel.data.frame <- function(x) {
    class(x) <- c("statistics_textmodel", "data.frame")
    return(x)
}

#' @noRd
#' @method as.statistics_textmodel matrix
#' @keywords internal textmodel
#' @export
as.statistics_textmodel.matrix <- function(x) {
    as.statistics_textmodel(as.data.frame(x))
} 

# extension of quanteda methods ---------------

#' @export
ndoc.textmodel <- function(x) 
    ndoc(x$x)

#' @export
nfeat.textmodel <- function(x) 
    nfeat(x$x)

#' @export
docnames.textmodel <- function(x) 
    docnames(x$x)

#' @export
featnames.textmodel <- function(x) 
    featnames(x$x)
