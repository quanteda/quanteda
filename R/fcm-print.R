#' @rdname print-quanteda
#' @export
setMethod("print", signature(x = "fcm"), 
          function(x, max_nfeat = quanteda_options("print_dfm_max_nfeat"), 
                   show_summary = TRUE, ...) {
              if (show_summary) {
                cat("Feature co-occurrence matrix of: ",
                    format(ndoc(x), big.mark = ","), " by ",
                    format(nfeat(x), big.mark = ","), " feature",
                    if (nfeat(x) != 1L) "s" else "",
                    ".\n", sep = "")
              }
              print_fcm(x, max_nfeat, show_summary, ...)
          })

#' @rdname print-quanteda
setMethod("show", signature(object = "fcm"), function(object) print(object))

# internal function for print.fcm
print_fcm <- function(x, max_nfeat, show_summary, ...) {
  
    unused_dots(...)
    x <- as.fcm(x)
    nrow <- nrow(x)
    ncol <- ncol(x)
    if (max_nfeat < 0 || max_nfeat > nrow) {
        max_nrow <- nrow
    } else {
        max_nrow <- max_nfeat
    }
    if (max_nfeat < 0 || max_nfeat > ncol) {
        max_ncol <- ncol
    } else {
        max_ncol <- max_nfeat
    }
    
    Matrix::printSpMatrix(x[seq_len(max_nrow), seq_len(max_ncol)], 
                          col.names = TRUE, zero.print = 0)
    nrow_rem <- nrow - max_nrow
    ncol_rem <- ncol - max_ncol
    if (nrow_rem > 0 || ncol_rem > 0) {
      cat("[", sep = "") 
      if (nrow_rem > 0) {
          cat(" reached max_feat ... ", format(nrow_rem, big.mark = ","), " more feature", sep = "") 
          if (nrow_rem > 1) cat("s", sep = "")
      }
      if (nrow_rem > 0 && ncol_rem > 0) 
          cat(",", sep = "")
      if (ncol_rem > 0) {
          cat(" reached max_nfeat ... ", format(ncol_rem, big.mark = ","), " more feature", sep = "") 
          if (ncol_rem > 1) cat("s", sep = "")
      }
      cat(" ]\n", sep = "") 
    }
}
