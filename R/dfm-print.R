#' Print a dfm object
#'
#' Print methods for document-feature matrices
#' @name print.dfm
#' @keywords internal dfm
NULL

#' @param x the dfm to be printed
#' @param show.values print the dfm values; if called explicitly this will print
#'   all values.
#' @param show.settings print the settings used to create the dfm. See 
#'   \link{settings}.
#' @param show.summary print a brief summary indicating the number of documents 
#'   and features
#' @param ndoc max number of documents to print; default is from the
#'   \code{print_dfm_max_ndoc} setting of \code{\link{quanteda_options}}
#' @param nfeat max number of features to print; default is from the
#'   \code{print_dfm_max_nfeat} setting of \code{\link{quanteda_options}}
#' @param ... further arguments passed to \code{Matrix::printSpMatrix2}
#' @seealso \code{\link{quanteda_options}}
#' @export
#' @rdname print.dfm
#' @keywords dfm
setMethod("print", signature(x = "dfm"), 
          function(x, show.values = NULL, show.settings = FALSE, 
                   show.summary = TRUE, 
                   ndoc = quanteda_options("print_dfm_max_ndoc"), 
                   nfeat = quanteda_options("print_dfm_max_nfeat"), ...) {
              if (show.summary) {
                  cat("Document-feature matrix of: ",
                      format(ndoc(x), big.mark = ","), " document",
                      if (ndoc(x) > 1L || ndoc(x) == 0L) "s, " else ", ",
                      format(nfeat(x), big.mark=","), " feature",
                      if (nfeat(x) > 1L || nfeat(x) == 0L) "s" else "",
                      if (is.resampled(x)) paste0(", ", nresample(x), " resamples") else "",
                      if (prod(dim(x))) format_sparsity(sparsity(x)),
                          # paste0(" (", format(sparsity(x)*100, digits = 3), "% sparse)"),
                          # paste0(" (", format(sparsity(x)*100, digits = 6, nsmall = 2, scientific = 4), "% sparse)"),
                      ".\n", sep = "")
              }
              print_dfm(x, ndoc, nfeat, show.values, show.settings, ...)
          })

#' @rdname print.dfm
#' @param object the item to be printed
setMethod("show", signature(object = "dfm"), function(object) print(object))


#' format a sparsity value for printing
#'
#' Inputs a dfm sparsity value from \code{\link{sparsity}} and formats it for
#' printing in \code{\link{print.dfm}}.
#' @param x input sparsity value, ranging from 0 to 1.0
#' @param threshold value below which the decimal places will be rounded and
#'   printed with an inequality
#' @param digits \code{digits} input to \code{\link{format}}
#' @param nsmall \code{nsmall} input to \code{\link{format}}
#' @return \code{character} value for inserting into the dfm print output
#' @examples 
#' s <- c(.9, .99, .999, .9999, .99999, 
#'        .1, .01, .001, .0001, .000001, .0000001, .00000001, .000000000001, 
#'        sparsity(dfm(data_corpus_inaugural)),
#'        sparsity(dfm(data_corpus_irishbudget2010)),
#'        .12312431242134)
#' for (i in s) 
#'     print(paste0(format(i, width = 10),  ":  ", quanteda:::format_sparsity(i)))
#' print(as.dfm(Matrix::rsparsematrix(1000, 1000, density = 0.9999)))
#' print(as.dfm(Matrix::rsparsematrix(10000, 10000, density = 0.00001)))
#' @keywords internal
format_sparsity <- function(x, threshold = .01, digits = 3, nsmall = 1) {
    if (x < 0 || x > 1.0)
        stop("illegal sparsity value; must be 0 <= x <= 1.0")
    sep = ""
    threshold_char <- as.character(threshold)
    sparsity100 <- x * 100
    
    # for edge cases
    if ((sparsity100 == threshold || sparsity100 == (100 - threshold)) && digits <= 3) 
        digits <- 4
        
    sparsity_output <- if (sparsity100 < threshold && sparsity100 > 0.00) {
        paste("<", stringi::stri_trim_left(as.character(format(threshold, digits = 4, nsmall = 2))), sep = sep)
    } else if (sparsity100 > (100 - threshold) && sparsity100 < 100.00) {
        paste(">", stringi::stri_trim_left(as.character(format(100 - threshold, digits = 4, nsmall = 2))), sep = sep)
    } else {
        stringi::stri_trim_left(format(sparsity100, digits = digits, nsmall = nsmall))
    }
    
    paste(" (", sparsity_output, "% sparse)", sep = "")
}


# internal function for print.dfm and print.fcm
print_dfm <- function(x, ndoc, nfeat, show_values, show_settings, ...) {
    
    if (show_settings)
        cat("Settings: TO BE IMPLEMENTED.")
    if (!is.null(show_values)) {
        # if show.values is set to TRUE, show full matrix
        ndoc <- nrow(x)
        nfeat <- ncol(x)
    } else {  
        if (nrow(x) <= ndoc && ncol(x) <= nfeat) {
            # use TRUE default but limit dimensions
            ndoc <- nrow(x)
            nfeat <- ncol(x)
            show_values <- TRUE
        } else {
            # turn off display if > dimensions
            show_values <- FALSE        
        }                      
    }
    if (show_values) {
        Matrix::printSpMatrix2(x[seq_len(ndoc), seq_len(nfeat)], 
                               col.names = TRUE, zero.print = 0, ...)
    }
}

#' Return the first or last part of a dfm
#' 
#' For a \link{dfm} object, returns the first or last \code{n} documents 
#' and first \code{nfeat} features.
#' @param x a dfm object
#' @param n a single, positive integer.  If positive, size for the resulting
#'   object: number of first/last documents for the dfm. If negative, all but
#'   the n last/first number of documents of x.
#' @param nf the number of features to return, where the resulting object 
#'   will contain the first \code{ncol} features; default is all features
#' @param ... additional arguments passed to other functions
#' @return A \link{dfm} class object corresponding to the subset defined 
#'   by \code{n} and \code{nfeat}.
#' @export
#' @name head.dfm
#' @method head dfm
#' @keywords dfm
#' @examples
#' head(data_dfm_lbgexample, 3, nf = 5)
#' head(data_dfm_lbgexample, -4)
#' 
head.dfm <- function(x, n = 6L, nf = nfeat(x), ...) { 
    x <- as.dfm(x)
    unused_dots(...)
    stopifnot(length(n) == 1L && length(nf) == 1L)
    n <- if (n < 0L)  max(ndoc(x) + n, 0L) else min(n, ndoc(x))
    nf <- if (nf < 0L)  max(nfeat(x) + nf, 0L) else min(nf, nfeat(x))
    x[seq_len(n), seq_len(nf)]
}


#' @rdname head.dfm
#' @method tail dfm
#' @export
#' @examples 
#' tail(data_dfm_lbgexample)
#' tail(data_dfm_lbgexample, n = 3, nf = 4)
tail.dfm <- function(x, n = 6L, nf = nfeat(x), ...) { 
    x <- as.dfm(x)
    unused_dots(...)
    stopifnot(length(n) == 1L && length(nf) == 1L)
    nrx <- ndoc(x)
    ncl <- nfeat(x)
    n <- if (n < 0L) max(nrx + n, 0L) else min(n, nrx)
    nf <- if (nf < 0L) max(ncl + nf, 0L) else min(nf, ncl)
    sel_doc <- as.integer(seq.int(to = nrx, length.out = n))
    sel_feat <- as.integer(seq.int(to = ncl, length.out = nf))
    x[sel_doc, sel_feat]
}

setMethod("head", signature(x = "dfm"), function(x, n = 6L, nf = nfeat(x), ...) { 
    UseMethod("head")
})
setMethod("tail", signature(x = "dfm"), function(x, n = 6L, nf = nfeat(x), ...) { 
    UseMethod("tail")
})
