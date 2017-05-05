#' print a dfm object
#'
#' print methods for document-feature matrices
#' @name print.dfm
#' @keywords internal dfm
NULL

#' @param x the dfm to be printed
#' @param show.values print the dfm as a matrix or array (if resampled).
#' @param show.settings print the settings used to create the dfm.  See 
#'   \link{settings}.
#' @param show.summary print a brief summary indicating the number of documents and features
#' @param ndoc max number of documents to print
#' @param nfeature max number of features to print
#' @param ... further arguments passed to or from other methods
#' @export
#' @rdname print.dfm
#' @keywords dfm
setMethod("print", signature(x = "dfm"), 
          function(x, show.values = FALSE, show.settings = FALSE, show.summary = TRUE, ndoc = 20L, nfeature = 20L, ...) {
              if (length(x) > 0) {
                  if (show.summary) {
                      cat("Document-feature matrix of: ",
                          format(ndoc(x), , big.mark=","), " document",
                          ifelse(ndoc(x)>1 | ndoc(x)==0, "s, ", ", "),
                          format(nfeature(x), big.mark=","), " feature",
                          ifelse(nfeature(x)>1 | nfeature(x)==0, "s", ""),
                          ifelse(is.resampled(x), paste(", ", nresample(x), " resamples", sep=""), ""),
                          " (", format(sparsity(x)*100, digits = 3),
                          "% sparse).\n", sep="")
                  }
                  if (show.settings) {
                      cat("Settings: TO BE IMPLEMENTED.")
                  }
                  if (show.values | (nrow(x) <= ndoc & ncol(x) <= nfeature)) {
                      if (is(x, "sparseMatrix"))
                          Matrix::printSpMatrix2(x[1:min(ndoc, ndoc(x)), 1:min(nfeature, nfeature(x))], 
                                             col.names=TRUE, zero.print=0, ...)
                      else if (is(x, "denseMatrix")) {
                          getMethod("show", "denseMatrix")(x, ...)
                      } else {
                          print(as.matrix(x))
                      }
                  }
              } else{
                  print(NULL)
              }
          })

#' @rdname print.dfm
#' @param object the item to be printed
setMethod("show", signature(object = "dfm"), function(object) print(object))


#' return the first or last part of a dfm
#' 
#' For a \link{dfm} object, returns the first or last \code{n} documents 
#' and first \code{ncol} features for inspection.
#' @param x a dfm object
#' @param n a single integer.  If positive, size for the resulting object: 
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
#' myDfm <- dfm(data_corpus_inaugural, ngrams = 2, verbose = FALSE)
#' head(myDfm)
#' tail(myDfm)
#' tail(myDfm, nfeature = 4)
head.dfm <- function(x, n = 6L, nfeature = 6L, ...) {
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    print(x, show.values = FALSE)
    cat("(showing first ", 
        ifelse(min(ndoc(x), n)==1, "", paste0(min(ndoc(x), n), " ")), 
        "document", ifelse(min(ndoc(x), n)==1, "", "s"), 
        " and first ", ifelse(min(nfeature, nfeature(x))==1, "", paste0(min(nfeature, nfeature(x)), " ")), 
        "feature", ifelse(min(nfeature, nfeature(x))==1, "", "s"), ")\n", sep = "")
    print(head(as.matrix(x[, 1:min(nfeature(x), nfeature)]), n))
    return(invisible(x[1:min(ndoc(x), n), 1:min(nfeature(x), nfeature)]))
}


#' @rdname head.dfm
#' @method tail dfm
#' @export
tail.dfm <- function(x, n = 6L, nfeature = 6L, ...) {
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    print(x, show.values = FALSE)
    cat("(showing last ", 
        ifelse(min(ndoc(x), n)==1, "", paste0(min(ndoc(x), n), " ")), 
        "document", ifelse(min(ndoc(x), n)==1, "", "s"), 
        " and last ", ifelse(min(nfeature, nfeature(x))==1, "", paste0(min(nfeature, nfeature(x)), " ")), 
        "feature", ifelse(min(nfeature, nfeature(x))==1, "", "s"), ")\n", sep = "")
    print(tail(as.matrix(x[, 1:min(nfeature(x), nfeature)]), n))
    return(invisible(x[max(ndoc(x) - n + 1, 1) : ndoc(x), 1:min(nfeature(x), nfeature)]))
}

setMethod("head", signature(x = "dfm"), function(x, n = 6L, nfeature = 6L, ...) {
    UseMethod("head")
})
setMethod("tail", signature(x = "dfm"), function(x, n = 6L, nfeature = 6L, ...) {
    UseMethod("tail")
})


