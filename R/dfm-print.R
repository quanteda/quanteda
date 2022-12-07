#' Print a dfm object
#'
#' @rdname print-methods
#' @param max_nfeat max number of features to print; default is from the
#'   `print_dfm_max_nfeat` setting of [quanteda_options()]
#' @export
#' @keywords dfm
setMethod("print", signature(x = "dfm"), 
          function(x, 
                   max_ndoc = quanteda_options("print_dfm_max_ndoc"), 
                   max_nfeat = quanteda_options("print_dfm_max_nfeat"), 
                   show_summary = quanteda_options("print_dfm_summary"), 
                   ...) {
              
              max_ndoc <- check_integer(max_ndoc, min = -1)
              max_nfeat <- check_integer(max_nfeat, min = -1)
              show_summary <- check_logical(show_summary)
              
              if (show_summary) {
                  docvars <- docvars(x)
                  cat(msg("Document-feature matrix of: %d %s, %d %s (%s sparse) and %d %s.\n",
                          list(ndoc(x), c("document", "documents"),
                               nfeat(x), c("feature", "features"),
                               format_sparsity(sparsity(x)),
                               ncol(docvars), c("docvar", "docvars")
                               ),
                          list(NULL, ndoc(x) != 1, NULL, nfeat(x) != 1, NULL, 
                               NULL, ncol(docvars) != 1)
                          ))
              }
              if (max_ndoc < 0) 
                  max_ndoc <- ndoc(x)
              if (max_ndoc > 0) 
                  print_dfm(x, max_ndoc, max_nfeat, show_summary, ...)
          })

#' @noRd
setMethod("show", signature(object = "dfm"), function(object) print(object))

#' @rdname print-methods
#' @name print.dfm
NULL

# internal function for print.dfm
print_dfm <- function(x, max_ndoc, max_nfeat, show_summary, ...) {
    
    x <- as.dfm(x)
    check_dots(...)
    
    ndoc <- ndoc(x)
    nfeat <- nfeat(x)
    if (max_ndoc < 0 || max_ndoc > ndoc) 
        max_ndoc <- ndoc
    if (max_nfeat < 0 || max_nfeat > nfeat)
        max_nfeat <- nfeat
    if (max_ndoc > 0 && max_nfeat > 0) {
        Matrix::printSpMatrix(x[seq_len(max_ndoc), seq_len(max_nfeat)], 
                              col.names = TRUE, zero.print = 0)
    }
    ndoc_rem <- ndoc - max_ndoc
    nfeat_rem <- nfeat - max_nfeat
    if (ndoc_rem > 0 || nfeat_rem > 0) {
        cat("[", sep = "") 
        if (ndoc_rem > 0) {
            cat(" reached max_ndoc ... ", format(ndoc_rem, big.mark = ","), " more document", sep = "") 
            if (ndoc_rem > 1) cat("s", sep = "")
        }
        if (ndoc_rem > 0 && nfeat_rem > 0) 
            cat(",", sep = "")
        if (nfeat_rem > 0) {
            cat(" reached max_nfeat ... ", format(nfeat_rem, big.mark = ","), " more feature", sep = "") 
            if (nfeat_rem > 1) cat("s", sep = "")
        }
        cat(" ]\n", sep = "") 
    }
}


#' format a sparsity value for printing
#'
#' Inputs a dfm sparsity value from [sparsity()] and formats it for
#' printing in `print.dfm()`.
#' @param x input sparsity value, ranging from 0 to 1.0
#' @examples 
#' ss <- c(1, .99999, .9999, .999, .99, .9,
#'        .1, .01, .001, .0001, .000001, .0000001, .00000001, .000000000001, 0)
#' for (s in ss) 
#'     cat(format(s, width = 10),  ":", quanteda:::format_sparsity(s), "\n")
#' @keywords internal
format_sparsity <- function(x) {
    if (is.na(x))
        x <- 0
    x <- check_double(x, min = 0, max = 1.0)
    level <- c(0, 0.0001, 0.9999, 1)
    if (any(x == level))
        return(stringi::stri_sprintf("%.2f%%", x * 100))
    v <- c("<0.01%", stringi::stri_sprintf("%.2f%%", x * 100), ">99.99%")
    return(v[as.integer(cut(x, level))])
}

