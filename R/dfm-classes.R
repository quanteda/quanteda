####################################################################
## dfm class definition and methods for primitives, ops, etc.
##
## Ken Benoit
####################################################################


#' Virtual class "dfm" for a document-feature matrix
#' 
#' @description The dfm class of object is a type of \link[Matrix]{Matrix-class}
#'   object with additional slots, described below.  \pkg{quanteda} uses two 
#'   subclasses of the \code{dfm} class, depending on whether the object can be 
#'   represented by a sparse matrix, in which case it is a \code{dfmSparse} 
#'   class object, or if dense, then a \code{dfmDense} object.  See Details.
#'   
#' @slot settings settings that govern corpus handling and subsequent downstream
#'   operations, including the settings used to clean and tokenize the texts, 
#'   and to create the dfm.  See \code{\link{settings}}.
#' @slot weighting the feature weighting applied to the dfm.  Default is
#'   \code{"frequency"}, indicating that the values in the cells of the dfm are
#'   simple feature counts.  To change this, use the \code{\link{weight}}
#'   method.
#' @slot smooth a smoothing parameter, defaults to zero.  Can be changed using 
#'   either the \code{\link{smooth}} or the \code{\link{weight}} methods.
#' @slot Dimnames  These are inherited from \link[Matrix]{Matrix-class} but are 
#'   named \code{docs} and \code{features} respectively.
#' @details The \code{dfm} class is a virtual class that will contain one of two
#'   subclasses for containing the cell counts of document-feature matrixes: 
#'   \code{dfmSparse} or \code{dfmDense}.
#' @seealso \link{dfm}
#' @export
#' @import methods
#' @docType class
#' @name dfm-class
setClass("dfm",
         slots = c(settings = "list", weightTf = "list", weightDf = "list", smooth = "numeric",
                   ngrams = "integer", concatenator = "character"),
         prototype = list(settings = list(NULL),
                          Dim = integer(2), 
                          Dimnames = list(docs=NULL, features=NULL),
                          weightTf = list(scheme = "count", base = NULL, K = NULL),
                          weightDf = list(scheme = "unary", base = NULL, c = NULL, smoothing = NULL, threshold = NULL),
                          smooth = 0,
                          ngrams = 1L,
                          concatenator = ""),
         contains = "Matrix")

#' @rdname dfm-class
#' @details The \code{dfmSparse} class is a sparse matrix version of
#'   \code{dfm-class}, inheriting \link[Matrix]{dgCMatrix-class} from the
#'   \pkg{Matrix} package.  It is the default object type created when feature
#'   counts are the object of interest, as typical text-based feature counts
#'   tend contain many zeroes.  As long as subsequent transformations of the dfm
#'   preserve cells with zero counts, the dfm should remain sparse.
#'   
#'   When the \pkg{Matrix} package implements sparse integer matrixes, we will
#'   switch the default object class to this object type, as integers are 4
#'   bytes each (compared to the current numeric double type requiring 8 bytes
#'   per cell.)
#' @export
setClass("dfmSparse",
         contains = c("dfm", "dgCMatrix"))

#' @rdname dfm-class
#' @details The \code{dfmDense} class is a sparse matrix version of \code{dfm-class}, 
#' inheriting \link[Matrix]{dgeMatrix-class} from the \pkg{Matrix} package.  dfm objects that
#' are converted through weighting or other transformations into cells without zeroes will 
#' be automatically converted to the dfmDense class.  This will necessarily be a much larger sized
#' object than one of \code{dfmSparse} class, because each cell is recorded as a numeric (double) type
#' requiring 8 bytes of storage.
#' @export
setClass("dfmDense",
         contains = c("dfm", "dgeMatrix"))



# # @rdname print.dfm
# # @export
# setMethod("print", signature(x = "dfm"), callNextMethod())
#               

#' print a dfm object
#'
#' print methods for document-feature matrices
#' @name print.dfm
NULL

#' Return the first or last part of a dfm
#' 
#' For a \link{dfm-class} object, returns the first or last \code{n} documents 
#' and first \code{ncol} features for inspection.
#' @export
#' @method head dfm
#' @param x a dfm object
#' @param n a single integer.  If positive, size for the resulting object: 
#'   number of first/last documents for the dfm. If negative, all but the n 
#'   last/first number of documents of x.
#' @param nfeature the number of features to return, where the resulting object 
#'   will contain the first \code{ncol} features
#' @param ... additional arguments passed to other functions
#' @return A \link{dfm-class} class object corresponding to the subset defined 
#'   by \code{n} and \code{ncol}.
#' @examples
#' myDfm <- dfm(inaugCorpus, ngrams = 2, verbose = FALSE)
#' head(myDfm)
#' tail(myDfm)
#' tail(myDfm, nfeature = 4)
head.dfm <- function(x, n = 6L, nfeature = 6L, ...) {
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    print(x, show.values = FALSE)
    cat("(showing first", min(ndoc(x), n), "documents and first", min(nfeature, nfeature(x)), "features)\n")
    #print(as.matrix(x[1:min(ndoc(x), n), 1:min(nfeature(x), nfeature)])) #, show.summary = FALSE, ndoc = n, nfeature = nfeature)
    print(head(as.matrix(x[, 1:min(nfeature(x), nfeature)]), n))
    return(invisible(x[1:min(ndoc(x), n), 1:min(nfeature(x), nfeature)]))
}

#' @export
#' @rdname head.dfm
setMethod("head", signature(x = "dfm"), function(x, n = 6L, nfeature = 6L, ...) 
    head.dfm(x, n = n, nfeature = nfeature, ...))

#' @export
#' @rdname head.dfm
setMethod("tail", signature(x = "dfm"), function(x, n = 6L, nfeature = 6L, ...) 
    tail.dfm(x, n = n, nfeature = nfeature, ...))


#' @export
#' @method tail dfm
#' @rdname head.dfm
tail.dfm <- function(x, n = 6L, nfeature = 6L, ...) {
    if (length(addedArgs <- list(...)))
        warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    print(x, show.values = FALSE)
    cat("(showing last", min(ndoc(x), n), "documents and first", min(nfeature, nfeature(x)), "features)\n")
    print(tail(as.matrix(x[, 1:min(nfeature(x), nfeature)]), n))
    return(invisible(x[max(ndoc(x) - n + 1, 1) : ndoc(x), 1:min(nfeature(x), nfeature)]))
}

#' @export
#' @rdname print.dfm
setMethod("print", signature(x = "dfm"), 
          function(x, show.values=FALSE, show.settings=FALSE, show.summary = TRUE, ndoc = 20L, nfeature = 20L, ...) {
              if (show.summary) {
                  cat("Document-feature matrix of: ",
                      format(ndoc(x), , big.mark=","), " document",
                      ifelse(ndoc(x)>1 | ndoc(x)==0, "s, ", ", "),
                      format(nfeature(x), big.mark=","), " feature",
                      ifelse(nfeature(x)>1 | nfeature(x)==0, "s", ""),
                      ifelse(is.resampled(x), paste(", ", nresample(x), " resamples", sep=""), ""),
                      ".\n", sep="")
              }
              if (show.settings) {
                  cat("Settings: TO BE IMPLEMENTED.")
              }
              if (show.values | (nrow(x)<=ndoc & ncol(x)<=nfeature)) {
                  Matrix::printSpMatrix2(x[1:min(ndoc, ndoc(x)), 1:min(nfeature, nfeature(x))], 
                                         col.names=TRUE, zero.print=0, ...)
              }
          })

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
setMethod("print", signature(x = "dfmSparse"), 
          function(x, show.values=FALSE, show.settings=FALSE, show.summary = TRUE, ndoc = 20L, nfeature = 20L, ...) {
              if (show.summary) {
                  cat("Document-feature matrix of: ",
                      format(ndoc(x), , big.mark=","), " document",
                      ifelse(ndoc(x)>1 | ndoc(x)==0, "s, ", ", "),
                      format(nfeature(x), big.mark=","), " feature",
                      ifelse(nfeature(x)>1 | nfeature(x)==0, "s", ""),
                      ifelse(is.resampled(x), paste(", ", nresample(x), " resamples", sep=""), ""),
                      ".\n", sep="")
              }
              if (show.settings) {
                  cat("Settings: TO BE IMPLEMENTED.")
              }
              if (show.values | (nrow(x) <= ndoc & ncol(x) <= nfeature)) {
                  Matrix::printSpMatrix2(x[1:min(ndoc, ndoc(x)), 1:min(nfeature, nfeature(x))], 
                                         col.names=TRUE, zero.print=0, ...)
              }
          })

#' @rdname print.dfm
setMethod("print", signature(x = "dfmDense"), 
          function(x, show.values=FALSE, show.settings=FALSE, show.summary = TRUE, ndoc = 20L, nfeature = 20L, ...) {
              if (show.summary) {
                  cat("Document-feature matrix of: ",
                      format(ndoc(x), , big.mark=","), " document",
                      ifelse(ndoc(x)>1 | ndoc(x)==0, "s, ", ", "),
                      format(nfeature(x), big.mark=","), " feature",
                      ifelse(nfeature(x)>1 | nfeature(x)==0, "s", ""),
                      ifelse(is.resampled(x), paste(", ", nresample(x), " resamples", sep=""), ""),
                      ".\n", sep="")
              }
              if (show.settings) {
                  cat("Settings: TO BE IMPLEMENTED.")
              }
              if (show.values | (nrow(x)<=ndoc & ncol(x)<=nfeature)) {
                  getMethod("show", "denseMatrix")(x, ...)
              }
          })

#' @rdname print.dfm
#' @param object the item to be printed
setMethod("show", signature(object = "dfmSparse"), function(object) print(object))

#' @rdname print.dfm
setMethod("show", signature(object = "dfmDense"), function(object) print(object))

#' @method print dfm
#' @rdname print.dfm
print.dfm <- function(x, show.values=FALSE, show.settings=FALSE, show.summary = TRUE, ndoc = 20L, nfeature = 20L, ...) {
    if (show.summary) {
        cat("Document-feature matrix of: ",
            ndoc(x), " document",
            ifelse(ndoc(x)>1, "s, ", ", "),
            dim(x)[2], " feature",
            ifelse(dim(x)[2]>1, "s", ""), ".\n", sep="")
    }
    cat(ndoc(x), "x", nfeature(x), "dense matrix of (S3) class \"dfm\"\n")
    #    ifelse(is.resampled(x), paste(", ", nresample(x), " resamples", sep=""), ""),
    
    if (show.settings) {
        cat("Settings: TO BE IMPLEMENTED.")
    }
    if (show.values | (nrow(x)<=ndoc & ncol(x)<=nfeature)) {
        class(x) <- class(x)[2]
        attr(x, "settings") <- NULL
        attr(x, "weighting") <- NULL
        print(x[1:ndoc, 1:nfeature])
    }
}


## S4 Method for the S4 class sparse dfm
# @param x the sparse dfm
# @rdname dfm-class
# @method t dfmSparse
#setMethod("t", signature(x = "dfmSparse"), getMethod("t", "dgCMatrix"))

# S4 Method for the S4 class dense/weighted dfm
#' @export
#' @rdname dfm-class
setMethod("t", signature(x = "dfmDense"), definition = 
              function(x) {
                  getMethod("t", "dgeMatrix")(x)
              }) #getMethod("t", "dgeMatrix"))


## S4 Method for the S3 class dense dfm
#' @export
#' @param x the dfm object
#' @rdname dfm-class 
setMethod("t",
          signature = (x = "dfmSparse"),
          definition = function(x) {
              new("dfmSparse", getMethod("t", "dgCMatrix")(x))
          })



#' @method colSums dfmSparse
#' @rdname dfm-class
#' @param na.rm if \code{TRUE}, omit missing values (including \code{NaN}) from
#'   the calculations
#' @param dims ignored
#' @export
setMethod("colSums", 
          signature = (x = "dfmSparse"),
          function(x, na.rm = FALSE, dims = 1L, ...) callNextMethod())

#' @method rowSums dfmSparse
#' @rdname dfm-class
#' @export
setMethod("rowSums", 
          signature = (x = "dfmSparse"),
          function(x, na.rm = FALSE, dims = 1L, ...) callNextMethod())


#' @method colMeans dfmSparse
#' @rdname dfm-class
#' @export
setMethod("colMeans", 
          signature = (x = "dfmSparse"),
          function(x, na.rm = FALSE, dims = 1L, ...) callNextMethod())

#' @method rowSums dfmSparse
#' @rdname dfm-class
#' @export
setMethod("rowMeans", 
          signature = (x = "dfmSparse"),
          function(x, na.rm = FALSE, dims = 1L, ...) callNextMethod())



# #' @param i index for documents
# #' @param j index for features
# #' @param drop always set to \code{FALSE}
# #' @param ... additional arguments not used here
# #' @export
# #' @rdname dfm-class
# setMethod("[", 
#           signature = c(x = "dfmSparse", i = "ANY", j = "ANY", drop = "logical"),
#           function(x, i, j, ..., drop = FALSE) {
#               cat("XXXXXX\n")
#               new("dfmSparse", getMethod("[", "dgCMatrix")(x, i, j, ..., drop = FALSE))
#               })
# 
# #' @rdname dfm-class
# #' @export
# setMethod("[", 
#           signature = (x = "dfmDense"),
#           function(x, i, j, ..., drop = FALSE) {
#               cat("OH YEAH\n")
#               new("dfmDense", callNextMethod())
#           })
# 

## S4 METHODS FOR INDEXING SPARSE dfm (dfmSparse) objects


# FROM THE MATRIX PACKAGE - no need to duplicate here
# setClassUnion("index", members =  c("numeric", "integer", "logical", "character"))

wrapIndexOperation <- function(x, i=NULL, j=NULL, ..., drop=FALSE) {
    if (is(x, "dfmSparse")) {
        asType <- "sparseMatrix"
        newType <- "dfmSparse"
    } else {
        asType <- "denseMatrix"
        newType <- "dfmDense"
    }
    if (drop) warning("drop=TRUE not currently supported")
    new(newType, "["(as(x, asType), i, j, ..., drop=FALSE))
}

#' @param i index for documents
#' @param j index for features
#' @param drop always set to \code{FALSE}
#' @param ... additional arguments not used here
#' @rdname dfm-class
setMethod("[", signature(x = "dfmDense", i = "index", j = "index", drop = "missing"),
          wrapIndexOperation)

#' @rdname dfm-class
setMethod("[", signature(x = "dfmDense", i = "index", j = "index", drop = "logical"),
          wrapIndexOperation)

#' @rdname dfm-class
setMethod("[", signature(x = "dfmDense", i = "index", j = "missing", drop = "missing"),
          function(x, i, j, ..., drop=FALSE)
              new("dfmDense", "["(as(x, "denseMatrix"), i, , ..., drop=FALSE)))

#' @rdname dfm-class
setMethod("[", signature(x = "dfmDense", i = "logical", j = "missing", drop = "missing"),
          function(x, i, j, ..., drop=FALSE)
              new("dfmDense", "["(as(x, "denseMatrix"), which(i), , ..., drop=FALSE)))

#' @rdname dfm-class
setMethod("[", signature(x = "dfmDense", i = "index", j = "missing", drop = "logical"),
          function(x, i, j, ..., drop=FALSE) {
              if (drop) warning("drop=TRUE not currently supported")
              new("dfmDense", "["(as(x, "denseMatrix"), i, , ..., drop=FALSE))
          })

#' @rdname dfm-class
setMethod("[", signature(x = "dfmDense", i = "missing", j = "index", drop = "missing"),
          function(x, i, j, ..., drop=FALSE) 
              new("dfmDense", "["(as(x, "denseMatrix"), , j, ..., drop=FALSE)))

#' @rdname dfm-class
setMethod("[", signature(x = "dfmDense", i = "missing", j = "index", drop = "logical"),
          function(x, i, j, ..., drop=FALSE) {
              if (drop) warning("drop=TRUE not currently supported")
              new("dfmDense", "["(as(x, "denseMatrix"), , j, ..., drop=FALSE))
          })

#' @rdname dfm-class
setMethod("[", signature(x = "dfmDense", i = "missing", j = "missing", drop = "missing"),
          function(x, i, j, ..., drop=FALSE) 
              new("dfmDense", "["(as(x, "denseMatrix"), , , ..., drop=FALSE)))

#' @rdname dfm-class
setMethod("[", signature(x = "dfmDense", i = "missing", j = "missing", drop = "logical"),
          function(x, i, j, ..., drop=FALSE) {
              if (drop) warning("drop=TRUE not currently supported")
              new("dfmDense", "["(as(x, "denseMatrix"), , , ..., drop=FALSE))
          })

#' @rdname dfm-class
setMethod("[", signature(x = "dfmSparse", i = "index", j = "index", drop = "missing"),
          wrapIndexOperation)

#' @rdname dfm-class
setMethod("[", signature(x = "dfmSparse", i = "index", j = "index", drop = "logical"),
          wrapIndexOperation)


#' @rdname dfm-class
setMethod("[", signature(x = "dfmSparse", i = "logical", j = "missing", drop = "missing"),
          function(x, i, j, ..., drop = FALSE) {
              new("dfmSparse", "["(as(x, "sparseMatrix"), which(i), , ..., drop=FALSE))
          })

#' @rdname dfm-class
setMethod("[", signature(x = "dfmSparse", i = "index", j = "missing", drop = "missing"),
          function(x, i, j, ..., drop=FALSE) {
              new("dfmSparse", "["(as(x, "sparseMatrix"), i, , ..., drop=FALSE))
          })

#' @rdname dfm-class
setMethod("[", signature(x = "dfmSparse", i = "index", j = "missing", drop = "logical"),
          function(x, i, j, ..., drop=FALSE) {
              if (drop) warning("drop=TRUE not currently supported")
              new("dfmSparse", "["(as(x, "sparseMatrix"), i, , ..., drop=FALSE))
          })

#' @rdname dfm-class
setMethod("[", signature(x = "dfmSparse", i = "missing", j = "index", drop = "missing"),
          function(x, i, j, ..., drop=FALSE) 
              new("dfmSparse", "["(as(x, "sparseMatrix"), , j, ..., drop=FALSE)))

#' @rdname dfm-class
setMethod("[", signature(x = "dfmSparse", i = "missing", j = "index", drop = "logical"),
          function(x, i, j, ..., drop=FALSE) {
              if (drop) warning("drop=TRUE not currently supported")
              new("dfmSparse", "["(as(x, "sparseMatrix"), , j, ..., drop=FALSE))
          })

#' @rdname dfm-class
setMethod("[", signature(x = "dfmSparse", i = "missing", j = "missing", drop = "missing"),
          function(x, i, j, ..., drop=FALSE) 
              new("dfmSparse", "["(as(x, "sparseMatrix"), , , ..., drop=FALSE)))

#' @rdname dfm-class
setMethod("[", signature(x = "dfmSparse", i = "missing", j = "missing", drop = "logical"),
          function(x, i, j, ..., drop=FALSE) {
              if (drop) warning("drop=TRUE not currently supported")
              new("dfmSparse", "["(as(x, "sparseMatrix"), , , ..., drop=FALSE))
          })




#' @param e1 first quantity in "+" operation for dfm
#' @param e2 second quantity in "+" operation for dfm
#' @rdname dfm-class
setMethod("+", signature(e1 = "dfmSparse", e2 = "numeric"),
          function(e1, e2) {
              as(as(e1, "Matrix") + e2, ifelse(e2==0, "dfmSparse", "dfmDense"))
          })       
#' @rdname dfm-class
setMethod("+", signature(e1 = "numeric", e2 = "dfmSparse"),
          function(e1, e2) {
              as(e1 + as(e2, "Matrix"), ifelse(e1==0, "dfmSparse", "dfmDense"))
          })  
#' @rdname dfm-class
setMethod("+", signature(e1 = "dfmDense", e2 = "numeric"),
          function(e1, e2) {
              as(as(e1, "Matrix") + e2, "dfmDense")
          })
#' @rdname dfm-class
setMethod("+", signature(e1 = "numeric", e2 = "dfmDense"),
          function(e1, e2) {
              as(e1 + as(e2, "Matrix"), "dfmDense")
          })


#' @rdname dfm-class
#' @export
#' @examples
#' # coercion to matrix
#' dfmSparse <- dfm(inaugTexts, verbose = FALSE)
#' str(as.matrix(dfmSparse))
setMethod("as.matrix", signature(x="dfm"),
          function(x) {
              if (isS4(x)) {
                  f <- getMethod("as.matrix", "Matrix")
                  x <- f(x)
                  names(dimnames(x)) <- c("docs", "features")
              } else {
                  x <- matrix(x, nrow=ndoc(x), dimnames = list(docs = docnames(x), features = features(x)))
              }
              x
          })

#' coerce a dfm to a data.frame
#' 
#' Method for coercing a \link{dfm-class} object to a data.frame
#' @aliases as.data.frame.dfm
#' @param x dfm to be coerced to a data.frame
#' @param row.names if \code{FALSE}, do not set the row names of the data.frame
#'   to the docnames of the dfm (default); or a vector of values to which the
#'   row names will be set.
#' @param optional not applicable to this method
#' @param ... not used for this method
#' @export
#' @examples
#' inaugDfm <- dfm(inaugTexts[1:5], verbose = FALSE)
#' as.data.frame(inaugDfm[, 1:10])
#' str(as.data.frame(inaugDfm))
#' as.data.frame(inaugDfm[, 1:10], row.names = FALSE)
setMethod("as.data.frame", signature = "dfm", 
          function(x, row.names = NULL, optional = FALSE , ...) {
              as.data.frame(as.matrix(x), row.names = row.names, optional = optional, ...)
})


#' Combine dfm objects by Rows or Columns
#' 
#' Take a sequence of \link{dfm-class} objects and combine by columns or
#' rows, returning a dfm with the combined documents or features, respectively.
#' 
#' @param ... \link{dfm} objects to be joined column-wise (\code{cbind}) or
#'   row-wise (\code{rbind}) to the first
#' @details \code{cbind(x, y, ...)} combines dfm objects by columns, returning a
#'   dfm object with combined features from input dfm objects.  Note that this should be used
#'   with extreme caution, as joining dfms with different documents will result in a new row
#'   with the docname(s) of the first dfm, merging in those from the second.  Furthermore, 
#'   if features are shared between the dfms being cbinded, then duplicate feature labels will 
#'   result.  In both instances, warning messages will result.
#' @export
#' @method cbind dfm
#' @examples 
#' # cbind() for dfm objects
#' (dfm1 <- dfm("This is one sample text sample.", verbose = FALSE))
#' (dfm2 <- dfm("More words here.", verbose = FALSE))
#' cbind(dfm1, dfm2)
cbind.dfm <- function(...) {
    args <- list(...)
    if (!all(sapply(args, is.dfm)))
        stop("all arguments must be dfm objects")
    dnames <- sapply(args, docnames)
    # make into a matrix-like object for apply to work below, even if just one document per input
    if (is.null(dim(dnames)))
        dnames <- matrix(dnames, ncol = length(dnames))
    if (!all(apply(dnames, 1, function(x) length(table(x)) == 1)))
        warning("cbinding dfms with different docnames", noBreaks. = TRUE)
    if (length(Reduce(intersect, lapply(args, features))))
        warning("cbinding dfms with overlapping features will result in duplicated features", noBreaks. = TRUE)
    
    result <- Matrix::cbind2(args[[1]], args[[2]])
    if (length(args) > 2) {
        for (y in args[3:length(args)]) 
            result <- Matrix::cbind2(result, y)
    }
    new("dfmSparse", result)
}

# setMethod("cbind", signature(x = "dfmSparse", y = "dfmSparse"), function(x, y, ...) {
#     getMethod("cbind", "dgCMatrix")(x, y, ...)
# })
#     

#' @rdname cbind.dfm
#' @details  \code{rbind(x, y, ...)} combines dfm objects by rows, returning a dfm
#'   object with combined features from input dfm objects.  Features are matched
#'   between the two dfm objects, so that the order and names of the features
#'   do not need to match.  The order of the features in the resulting dfm
#'   is not guaranteed.  The attributes and settings of this new dfm are not
#'   currently preserved.
#' @export
#' @method rbind dfm
#' @examples 
#' 
#' # rbind() for dfm objects
#' (dfm1 <- dfm(c(doc1 = "This is one sample text sample."), verbose = FALSE))
#' (dfm2 <- dfm(c(doc2 = "One two three text text."), verbose = FALSE))
#' (dfm3 <- dfm(c(doc3 = "This is the fourth sample text."), verbose = FALSE))
#' rbind(dfm1, dfm2)
#' rbind(dfm1, dfm2, dfm3)
rbind.dfm <- function(...) {
    args <- list(...)
    if (!all(sapply(args, is.dfm)))
        stop("all arguments must be dfm objects")
    catm(names(args))

    if (length(args) == 1) {
        warning('rbind.dfm called on single dfm')
        return(args[[1]])
    }
    else if (length(args) == 2) {
        return(rbind2.dfm(args[[1]], args[[2]]))
    } else {
        # Recursive call
        result <- rbind2.dfm(args[[1]], args[[2]])
        for (y in args[3:length(args)]) 
            result <- rbind2.dfm(result, y)
        return(result)
    }
}

rbind2.dfm <- function(x, y) {
    x_names <- features(x)
    y_names <- features(y)

      if (identical(x_names, y_names)) {
          return(new("dfmSparse", Matrix::rbind2(x, y)))
      }

    common_names <- intersect(x_names, y_names)

    only_x_names <- setdiff(x_names, y_names)
    only_y_names <- setdiff(y_names, x_names)

    #  Evetually, we want to rbind together two rows with the same columns
    #  we acheive this by re-ordering the columns in the original matrices, and adding
    #  in some blank ones

    #  Here's what we're constructing:
    #  row x ... <columns in both x and y> ... <columns only in x> ... <blank columns>
    #  row y ... <columns in both x and y> ... <blank columns>     ... <columns only in y>
    xcols <- Matrix::cbind2(
                x[, common_names],
                x[, only_x_names]
    )
    empty_ycols <- Matrix::sparseMatrix(i = NULL, j = NULL, dims = c(ndoc(x), 
                length(only_y_names)), dimnames = list(NULL, only_y_names))

    empty_xcols <- Matrix::sparseMatrix(i = NULL, j = NULL, dims = c(ndoc(y), 
                    length(only_x_names)), dimnames = list(NULL, only_x_names))

    y_with_xcols <- Matrix::cbind2(
                y[, common_names],
                empty_xcols
            )

    new_dfm <- new("dfmSparse", Matrix::rbind2(
        Matrix::cbind2( xcols, empty_ycols),
        Matrix::cbind2( y_with_xcols, y[, only_y_names])
    ))
}

rbind2.dfm.old <- function(x, y) {
    x.names <- features(x)
    y.names <- features(y)

      if (identical(x.names, y.names)) {
          return(new("dfmSparse", Matrix::rbind2(x, y)))
      }

    all.names <- union(x.names, y.names)
    
    toAddx <- setdiff(all.names, x.names)
    toAddy <- setdiff(all.names, y.names)
    
    addedx <- new("dfmSparse", Matrix::cbind2(x, 
                           sparseMatrix(i = NULL, j = NULL, dims = c(ndoc(x), length(toAddx)), 
                                    dimnames = list(docnames(x), toAddx))))
    addedy <- new("dfmSparse", Matrix::cbind2(y, 
                           sparseMatrix(i = NULL, j = NULL, dims = c(ndoc(y), length(toAddy)), 
                                    dimnames = list(docnames(y), toAddy))))

    # sort in same feature order
    addedx <- addedx[, order(features(addedx))]
    addedy <- addedy[, order(features(addedy))]
    
    new("dfmSparse", Matrix::rbind2(addedx, addedy))
}
