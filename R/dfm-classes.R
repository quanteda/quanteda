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
#' @keywords internal dfm
setClass("dfm",
         slots = c(settings = "list", weightTf = "list", weightDf = "list", smooth = "numeric",
                   ngrams = "integer", skip = "integer", concatenator = "character"),
         prototype = list(settings = list(NULL),
                          Dim = integer(2), 
                          Dimnames = list(docs=NULL, features=NULL),
                          weightTf = list(scheme = "count", base = NULL, K = NULL),
                          weightDf = list(scheme = "unary", base = NULL, c = NULL, smoothing = NULL, threshold = NULL),
                          smooth = 0,
                          ngrams = 1L,
                          skip = 0L,
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


#' coerce a dfm to a matrix or data.frame
#' 
#' Methods for coercing a \link{dfm} object to a matrix or data.frame object.
#' @rdname as.matrix.dfm
#' @param x dfm to be coerced
#' @export
#' @keywords dfm
#' @method as.matrix dfm
#' @examples
#' # coercion to matrix
#' mydfm <- dfm(data_char_inaugural)
#' str(as.matrix(mydfm))
#' 
as.matrix.dfm <- function(x, ...) {
    as(x, "matrix")
}
# setMethod("as.matrix", signature(x = "dfm"),
#           function(x) as(x, "matrix"))

#' @rdname as.matrix.dfm
#' @param row.names if \code{FALSE}, do not set the row names of the data.frame
#'   to the docnames of the dfm (default); or a vector of values to which the
#'   row names will be set.
#' @param optional not applicable to this method
#' @param ... not used 
#' @method as.data.frame dfm
#' @export
#' @examples
#' # coercion to a data.frame
#' inaugDfm <- dfm(data_char_inaugural[1:5])
#' as.data.frame(inaugDfm[, 1:10])
#' as.data.frame(inaugDfm[, 1:10], row.names = FALSE)
as.data.frame.dfm <- function(x, row.names = NULL, optional = FALSE , ...) {
    as.data.frame(as.matrix(x), row.names = row.names, optional = optional, ...)
}
    


#' Combine dfm objects by Rows or Columns
#' 
#' Take a sequence of \link{dfm} objects and combine by columns or
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
#' @keywords internal dfm
#' @examples 
#' # cbind() for dfm objects
#' (dfm1 <- dfm("This is one sample text sample"))
#' (dfm2 <- dfm("More words here"))
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
    if (length(Reduce(intersect, lapply(args, featnames))))
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
        # Recursive call
        result <- rbind2.dfm(args[[1]], args[[2]])
        for (y in args[3:length(args)]) 
            result <- rbind2.dfm(result, y)
        return(result)
    }
}

rbind2.dfm <- function(x, y) {
    x_names <- featnames(x)
    y_names <- featnames(y)

      if (identical(x_names, y_names)) {
          return(new("dfmSparse", Matrix::rbind2(x, y)))
      }

    common_names <- intersect(x_names, y_names)

    only_x_names <- setdiff(x_names, y_names)
    only_y_names <- setdiff(y_names, x_names)

    #  Evetually, we want to rbind together two rows with the same columns
    #  we acheive this by re-ordering the columns in the original matrices, and adding
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

