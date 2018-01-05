
#' Impliments print methods for textmodel_summary 
#'
#' @param x a textmodel_summary object
#' @param ... additional arguments not used
#' @export
print.textmodel_summary <- function(x, ...) {
    for (n in names(x)) {
        cat(stri_trans_totitle(n), ':\n', sep = '')
        print(x[[n]])
        cat('\n')
    }
}


