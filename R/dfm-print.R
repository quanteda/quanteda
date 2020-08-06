#' Print a dfm object
#'
#' @rdname print-quanteda
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
              if (show_summary) {
                  docvars <- docvars(x)
                  cat("Document-feature matrix of: ",
                      format(ndoc(x), big.mark = ","), " document",
                      if (ndoc(x) > 1L || ndoc(x) == 0L) "s, " else ", ",
                      format(nfeat(x), big.mark=","), " feature",
                      if (nfeat(x) > 1L || nfeat(x) == 0L) "s" else "",
                      if (prod(dim(x))) format_sparsity(sparsity(x)), sep = "")
                  if (ncol(docvars))
                      cat(" and ", format(ncol(docvars), big.mark = ","), " docvar",
                          if (ncol(docvars) == 1L) "" else "s", sep = "")
                  cat(".\n")
              }
              if (max_ndoc < 0) 
                  max_ndoc <- ndoc(x)
              if (max_ndoc > 0) 
                  print_dfm(x, max_ndoc, max_nfeat, ...)
          })

#' @rdname print-quanteda
setMethod("show", signature(object = "dfm"), function(object) print(object))

# internal function for print.dfm
print_dfm <- function(x, max_ndoc, max_nfeat, show_summary, ...) {
    
    unused_dots(...)
    x <- as.dfm(x)
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
#' @param threshold value below which the decimal places will be rounded and
#'   printed with an inequality
#' @param digits `digits` input to [format()]
#' @param nsmall `nsmall` input to [format()]
#' @return `character` value for inserting into the dfm print output
#' @examples 
#' s <- c(.9, .99, .999, .9999, .99999, 
#'        .1, .01, .001, .0001, .000001, .0000001, .00000001, .000000000001, 
#'        sparsity(dfm(data_corpus_inaugural)),
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


#' Return the first or last part of a dfm
#' 
#' For a [dfm] object, returns the first or last `n` documents 
#' and first `nfeat` features.
#' @param x a dfm object
#' @param n a single, positive integer.  If positive, size for the resulting
#'   object: number of first/last documents for the dfm. If negative, all but
#'   the n last/first number of documents of x.
#' @param nf the number of features to return, where the resulting object 
#'   will contain the first `ncol` features; default is all features
#' @param ... additional arguments passed to other functions
#' @return A [dfm] class object corresponding to the subset defined 
#'   by `n` and `nfeat`.
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
