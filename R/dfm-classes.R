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
#' @slot weighting the feature weighting applied to the dfm.  Default is \code{"frequency"}, 
#'   indicating that the values in the cells of the dfm are simple feature 
#'   counts.  To change this, use the \code{\link{weight}} method.
#' @slot smooth a smoothing parameter, defaults to zero.  Can be changed using 
#'   either the \code{\link{smooth}} or the \code{\link{weight}} methods.
#' @slot Dimnames  These are inherited from \link[Matrix]{Matrix-class} but are 
#'   named \code{docs} and \code{features} respectively.
#' @details The \code{dfm} class is a virtual class that will contain one of two
#'   subclasses for containing the cell counts of document-feature matrixes:  
#'   \code{dfmSparse} or \code{dfmDense}.
#' @export
#' @import methods
setClass("dfm",
         slots = c(settings = "list", weighting = "character", smooth = "numeric"),
         prototype = list(settings = list(NULL),
                          Dim = integer(2), 
                          Dimnames = list(docs=NULL, features=NULL),
                          weighting = "frequency", 
                          smooth = 0),
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

#' @rdname print.dfm
setMethod("print", signature(x = "dfmSparse"), 
          function(x, show.values=FALSE, show.settings=FALSE, ...) {
              cat("Document-feature matrix of: ",
                  ndoc(x), " document",
                  ifelse(ndoc(x)>1, "s, ", ", "),
                  dim(x)[2], " feature",
                  ifelse(dim(x)[2]>1, "s", ""),
                  ifelse(is.resampled(x), paste(", ", nresample(x), " resamples", sep=""), ""),
                  ".\n", sep="")
              if (show.settings) {
                  cat("Settings: TO BE IMPLEMENTED.")
              }
              if (show.values | (nrow(x)<=20 & ncol(x)<=20)) {
                  Matrix::printSpMatrix2(x, col.names=TRUE, zero.print=0, ...)
              }
          })

#' @rdname print.dfm
setMethod("print", signature(x = "dfmDense"), 
          function(x, show.values=FALSE, show.settings=FALSE, ...) {
              cat("Document-feature matrix of: ",
                  ndoc(x), " document",
                  ifelse(ndoc(x)>1, "s, ", ", "),
                  dim(x)[2], " feature",
                  ifelse(dim(x)[2]>1, "s", ""),
                  ifelse(is.resampled(x), paste(", ", nresample(x), " resamples", sep=""), ""),
                  ".\n", sep="")
              if (show.settings) {
                  cat("Settings: TO BE IMPLEMENTED.")
              }
              if (show.values | (nrow(x)<=20 & ncol(x)<=20)) {
                  getMethod("show", "denseMatrix")(x, ...)
              }
          })


#' @rdname print.dfm
#' @param object the item to be printed
setMethod("show", signature(object = "dfmSparse"), function(object) print(object))

#' @rdname print.dfm
setMethod("show", signature(object = "dfmDense"), function(object) print(object))

## S4 Method for the S4 class sparse dfm
# @param x the sparse dfm
# @rdname dfm-class
# @method t dfmSparse
#setMethod("t", signature(x = "dfmSparse"), getMethod("t", "dgCMatrix"))

## S4 Method for the S4 class dense/weighted dfm
# @rdname dfm-class
# @method t dfmSparse
# setMethod("t", signature(x = "dfmDense"), definition = 
#               function(x) {
#                   selectMethod("t", "dgeMatrix")
#               }) #getMethod("t", "dgeMatrix"))


## S4 Method for the S3 class dense dfm
#' @export
#' @param x the dfm object
#' @rdname dfm-class 
setMethod("t",
          signature = (x = "dfm"),
          definition = function(x) {
              newx <- t(matrix(x, nrow=nrow(x)))
              dimnames(newx) <- rev(dimnames(x))
#               if (isS4(x)) {
#                   newx <- t(as.Matrix(x))
#                   attributes(newx)$dimnames <- rev(x@Dimnames)
#               } else {
#                   attsorig <- attributes(x)
#                   attributes(newx)$dimnames <- rev(attsorig$dimnames)
#               }
              newx
          })


# @details \code{rowSums} and \code{colSums} form row and column sums and means for \link{dfm-class} objects.
# @param x a dfm, inheriting from \link[Matrix]{Matrix}
# @param na.rm if \code{TRUE}, omit missing values (including \code{NaN}) from
#'   the calculations
# @param dims ignored
# @param ... additional arguments, for methods/generic compatibility
# @return returns a named (non-sparse) numeric vector
# @rdname dfm-class
# @aliases colSums rowSums
# @export
# @examples
#' myDfm <- dfm(inaugTexts, verbose=FALSE)
# colSums(myDfm[, 1:10])
# rowSums(myDfm)
# @export
# setGeneric("colSums",
#           def = function(x, na.rm = FALSE, dims = 1L, ...) standardGeneric("colSums"))
# 
# # @export
# # @rdname dfm-class
# setGeneric("rowSums",
#            def = function(x, na.rm = FALSE, dims = 1L, ...) standardGeneric("rowSums"))


# @method colSums dfmSparse
#' @rdname dfm-class
#' @param na.rm if \code{TRUE}, omit missing values (including \code{NaN}) from
#'   the calculations
#' @param dims ignored
# @export
setMethod("colSums", 
          signature = (x = "dfmSparse"),
          definition = function(x, na.rm = FALSE, dims = 1L, ...) {
              csums <- callNextMethod()
              names(csums) <- features(x)
              csums
          })

# @method colSums dfmDense
#' @rdname dfm-class
# @export
setMethod("colSums", 
          signature = (x = "dfmDense"),
          definition = function(x, na.rm = FALSE, dims = 1L, ...) {
              csums <- callNextMethod()
              names(csums) <- features(x)
              csums
          })

# @method rowSums dfmSparse
#' @rdname dfm-class
# @export
setMethod("rowSums", 
          signature = (x = "dfmSparse"),
          definition = function(x, na.rm = FALSE, dims = 1L, ...) {
              rsums <- callNextMethod()
              names(rsums) <- docnames(x)
              rsums
          })

# @method rowSums dfmDense
#' @rdname dfm-class
# @export
setMethod("rowSums", 
          signature = (x = "dfmDense"),
          definition = function(x, na.rm = FALSE, dims = 1L, ...) {
              rsums <- callNextMethod()
              names(rsums) <- docnames(x)
              rsums
          })

#' @export
#' @rdname topfeatures
topfeatures.dgCMatrix <- function(x, n=10, decreasing=TRUE, ...) {
    if (is.null(n)) n <- ncol(x)
    #     if (is.resampled(x)) {
    #         subdfm <- x[, order(colSums(x[,,1]), decreasing=decreasing), ]
    #         subdfm <- subdfm[, 1:n, ]   # only top n need to be computed
    #         return(data.frame(#features=colnames(subdfm),
    #             freq=colSums(subdfm[,,1]),
    #             cilo=apply(colSums(subdfm), 1, quantile, (1-ci)/2),
    #             cihi=apply(colSums(subdfm), 1, quantile, 1-(1-ci)/2)))
    #     } else {
    
    csums <- colSums(x)
    names(csums) <- x@Dimnames$features
    subdfm <- sort(csums, decreasing)
    return(subdfm[1:n])
    #    }
}



#' print a dfm object
#'
#' print method for dfm objects
#' @param x the dfm to be printed
#' @param show.values print the dfm as a matrix or array (if resampled).
#' @param show.settings Print the settings used to create the dfm.  See
#'   \link{settings}.
#' @param ... further arguments passed to or from other methods
#' @export 
#' @method print dfm
print.dfm <- function(x, show.values=FALSE, show.settings=FALSE, ...) {
    cat("Document-feature matrix of: ",
        ndoc(x), " document",
        ifelse(ndoc(x)>1, "s, ", ", "),
        dim(x)[2], " feature",
        ifelse(dim(x)[2]>1, "s", ""), ".\n", sep="")
    cat(ndoc(x), "x", nfeature(x), "dense matrix of (S3) class \"dfm\"\n")
    #    ifelse(is.resampled(x), paste(", ", nresample(x), " resamples", sep=""), ""),
    
    if (show.settings) {
        cat("Settings: TO BE IMPLEMENTED.")
    }
    if (show.values | (nrow(x)<=20 & ncol(x)<=20)) {
        class(x) <- class(x)[2]
        attr(x, "settings") <- NULL
        attr(x, "weighting") <- NULL
        print(x)
    }
}

## S3 METHODS FOR INDEXING DENSE dfm object
#' @export
#' @method [ dfm
#' @rdname dfm-class
`[.dfm` <- function(x, i, j, ..., drop=FALSE) {
    if (drop) warning("drop=TRUE not currently supported")
    m <- NextMethod("[", drop=FALSE)
    attr(m, "settings") <- attr(x, "settings")
    attr(m, "weighting") <- attr(x, "weighting")
    class(m) <- class(x)
    m
}

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
setMethod("[", signature(x = "dfmSparse", i = "index", j = "missing", drop = "missing"),
          function(x, i, j, ..., drop=FALSE)
              new("dfmSparse", "["(as(x, "sparseMatrix"), i, , ..., drop=FALSE)))

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


#' @rdname dfm
#' @export
#' @examples
#' \dontshow{
#' dfmSparse <- dfm(inaugTexts, verbose=FALSE)
#' str(as.matrix(dfmSparse))
#' class(as.matrix(dfmSparse))
#' dfmDense <- dfm(inaugTexts, verbose=FALSE, matrixType="dense")
#' str(as.matrix(dfmDense))
#' class(as.matrix(dfmDense))
#' identical(as.matrix(dfmSparse), as.matrix(dfmDense))
#' }
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

#' @rdname dfm
#' @export
#' @examples
#' \dontshow{
#' dfmSparse <- dfm(inaugTexts, verbose=FALSE)
#' str(as.data.frame(dfmSparse))
#' class(as.data.frame(dfmSparse))
#' dfmDense <- dfm(inaugTexts, verbose=FALSE, matrixType="dense")
#' str(as.data.frame(dfmDense))
#' class(as.data.frame(dfmDense))
#' identical(as.data.frame(dfmSparse), as.data.frame(dfmDense))
#' }
setMethod("as.data.frame", signature(x="dfm"), function(x) as.data.frame(as.matrix(x)))

