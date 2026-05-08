#' @rdname print-methods
#' @export
setMethod("print", signature(x = "fcm"), 
          function(x, max_nfeat = quanteda_options("print_dfm_max_nfeat"), 
                   show_summary = TRUE, ...) {
              if (show_summary) {
                  wrap(msg("Feature co-occurrence matrix of: %s by %s %s.\n",
                           nrow(x), ncol(x), inflect("feature", nrow(x) * ncol(x))))
              }
              print_fcm(x, max_nfeat, show_summary, ...)
          })

#' @noRd
setMethod("show", signature(object = "fcm"), function(object) print(object))

# internal function for print.fcm
print_fcm <- function(x, max_nfeat, show_summary, ...) {
  
    x <- as.fcm(x)
    check_dots(...)
    
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
      line <- "[ "
      if (nrow_rem > 0) {
          line <- msg("reached max_nfeat ... %s more %s",
                      nrow_rem, inflect("feature", nrow_rem),
                      prepend = line)
      }
      if (nrow_rem > 0 && ncol_rem > 0) 
          line <- paste0(line, ", ")
      if (ncol_rem > 0) {
          line <- msg("reached max_nfeat ... %s more %s",
                      ncol_rem, inflect("feature", ncol_rem),
                      prepend = line)
      }
      line <- paste0(line, " ]")
      wrap(line)
    }
}
