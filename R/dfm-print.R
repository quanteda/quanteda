#' print a dfm object
#'
#' print methods for document-feature matrices
#' @name print.dfm
#' @keywords internal dfm
NULL

#' @param x the dfm to be printed
#' @param show.values print the dfm values; if called explicitly this will print
#'   all values, overriding \code{ndoc} and \code{nfeature}.
#' @param show.settings print the settings used to create the dfm. See 
#'   \link{settings}.
#' @param show.summary print a brief summary indicating the number of documents 
#'   and features
#' @param ndoc max number of documents to print; default is from the
#'   \code{print_dfm_max_ndoc} setting of \code{\link{quanteda_options}}
#' @param nfeature max number of features to print; default is from the
#'   \code{print_dfm_max_nfeature} setting of \code{\link{quanteda_options}}
#' @param ... further arguments passed to \code{Matrix::printSpMatrix2}
#' @seealso \code{\link{quanteda_options}}
#' @export
#' @rdname print.dfm
#' @keywords dfm
setMethod("print", signature(x = "dfm"), 
          function(x, show.values = NULL, show.settings = FALSE, show.summary = TRUE, 
                   ndoc = quanteda_options("print_dfm_max_ndoc"), 
                   nfeature = quanteda_options("print_dfm_max_nfeature"), ...) {
              if (show.summary) {
                  cat("Document-feature matrix of: ",
                      format(ndoc(x), , big.mark = ","), " document",
                      if (ndoc(x) > 1L || ndoc(x) == 0L) "s, " else ", ",
                      format(nfeature(x), big.mark=","), " feature",
                      if (nfeature(x) > 1L || nfeature(x) == 0L) "s" else "",
                      if (is.resampled(x)) paste0(", ", nresample(x), " resamples") else "",
                      if (prod(dim(x))) paste0(" (", format(sparsity(x)*100, digits = 3), "% sparse)"),
                      ".\n", sep = "")
              }
              print_dfm(x, ndoc, nfeature, show.values, show.settings, ...)
          })

#' @rdname print.dfm
#' @param object the item to be printed
setMethod("show", signature(object = "dfm"), function(object) print(object))

# internal function for print.dfm and print.fcm
print_dfm <- function(x, ndoc, nfeature, show_values, show_settings, ...) {
    
    if (show_settings)
        cat("Settings: TO BE IMPLEMENTED.")
    if (!is.null(show_values)) {
        # if show.values is set to TRUE, show full matrix
        ndoc <- nrow(x)
        nfeature <- ncol(x)
    } else {  
        if (nrow(x) <= ndoc && ncol(x) <= nfeature) {
            # use TRUE default but limit dimensions
            ndoc <- nrow(x)
            nfeature <- ncol(x)
            show_values <- TRUE
        } else {
            # turn off display if > dimensions
            show_values <- FALSE        
        }                      
    }
    if (show_values) {
        Matrix::printSpMatrix2(x[seq_len(ndoc), seq_len(nfeature)], 
                               col.names = TRUE, zero.print = 0, ...)
    }
}

#' return the first or last part of a dfm
#' 
#' For a \link{dfm} object, returns the first or last \code{n} documents 
#' and first \code{nfeature} features.
#' @param x a dfm object
#' @param n a single, positive integer.  If positive, size for the resulting object: 
#'   number of first/last documents for the dfm. If negative, all but the n 
#'   last/first number of documents of x.
#' @param nfeature the number of features to return, where the resulting object 
#'   will contain the first \code{ncol} features
#' @param ... additional arguments passed to other functions
#' @return A \link{dfm} class object corresponding to the subset defined 
#'   by \code{n} and \code{nfeature}.
#' @export
#' @name head.dfm
#' @method head dfm
#' @keywords dfm
#' @examples
#' head(data_dfm_lbgexample, 3, nfeature = 5)
#' head(data_dfm_lbgexample, -4)
#' 
head.dfm <- function(x, n = 6L, nfeature = 6L, ...) {
    x <- as.dfm(x)
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    stopifnot(length(n) == 1L || length(nfeature) == 1L)
    n <- if (n < 0L)  max(ndoc(x) + n, 0L) else min(n, ndoc(x))
    nfeature <- if (nfeature < 0L)  max(nfeature(x) + nfeature, 0L) else min(nfeature, nfeature(x))
    x[seq_len(n), seq_len(nfeature)]
}


#' @rdname head.dfm
#' @method tail dfm
#' @export
#' @examples 
#' tail(data_dfm_lbgexample)
#' tail(data_dfm_lbgexample, n = 3, nfeature = 4)
tail.dfm <- function(x, n = 6L, nfeature = 6L, ...) {
    x <- as.dfm(x)
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    stopifnot(length(n) == 1L || length(nfeature) == 1L)
    nrx <- ndoc(x)
    ncl <- nfeature(x)
    n <- if (n < 0L) max(nrx + n, 0L) else min(n, nrx)
    nfeature <- if (nfeature < 0L) max(ncl + nfeature, 0L) else min(nfeature, ncl)
    sel_doc <- as.integer(seq.int(to = nrx, length.out = n))
    sel_feat <- as.integer(seq.int(to = ncl, length.out = nfeature))
    x[sel_doc, sel_feat]
}

setMethod("head", signature(x = "dfm"), function(x, n = 6L, nfeature = 6L, ...) {
    UseMethod("head")
})
setMethod("tail", signature(x = "dfm"), function(x, n = 6L, nfeature = 6L, ...) {
    UseMethod("tail")
})


