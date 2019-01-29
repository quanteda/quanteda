#' Virtual class "dfm" for a document-feature matrix
#' 
#' The dfm class of object is a type of \link[Matrix]{Matrix-class} object with
#' additional slots, described below.  \pkg{quanteda} uses two subclasses of the
#' \code{dfm} class, depending on whether the object can be represented by a
#' sparse matrix, in which case it is a \code{dfm} class object, or if dense,
#' then a \code{dfmDense} object.  See Details.
#'
#' @slot settings settings that govern corpus handling and subsequent downstream
#'   operations, including the settings used to clean and tokenize the texts,
#'   and to create the dfm.  See \code{\link{settings}}.
#' @slot weighting the feature weighting applied to the dfm.  Default is
#'   \code{"frequency"}, indicating that the values in the cells of the dfm are
#'   simple feature counts. To change this, use the \code{\link{dfm_weight}}
#'   method.
#' @slot smooth a smoothing parameter, defaults to zero.  Can be changed using
#'   the \code{\link{dfm_smooth}} method.
#' @slot Dimnames  These are inherited from \link[Matrix]{Matrix-class} but are
#'   named \code{docs} and \code{features} respectively.
#' @details The \code{dfm} class is a virtual class that will contain
#'   \link[Matrix]{dgCMatrix-class}.
#' @seealso \link{dfm}
#' @name dfm-class
#' @rdname dfm-class
#' @import methods
#' @keywords internal dfm
setClass("dfm",
         slots = c(settings = "list", weightTf = "list", weightDf = "list", 
                   smooth = "numeric", 
                   ngrams = "integer", skip = "integer", 
                   concatenator = "character", version = "integer",
                   docvars = "data.frame"),
         prototype = list(settings = list(),
                          Dim = integer(2), 
                          Dimnames = list(docs = character(), features = character()),
                          weightTf = list(scheme = "count", base = NULL, K = NULL),
                          weightDf = list(scheme = "unary", base = NULL, c = NULL,
                                          smoothing = NULL, threshold = NULL),
                          smooth = 0,
                          ngrams = 1L,
                          skip = 0L,
                          concatenator = "_",
                          version = unlist(packageVersion("quanteda")),
                          docvars = data.frame(row.names = character())),
         contains = "dgCMatrix")

# deprecated dfmSparse class for backward compatibility
#' @rdname dfm-class
#' @keywords internal dfm
setClass("dfmSparse", contains = "dfm")


## S4 method dfm objects
#' @export
#' @param x the dfm object
#' @rdname dfm-class
setMethod("t",
          signature = (x = "dfm"),
          function(x) as.dfm(t(as(x, "dgCMatrix"))))

#' @method colSums dfm
#' @rdname dfm-class
#' @param na.rm if \code{TRUE}, omit missing values (including \code{NaN}) from
#'   the calculations
#' @param dims ignored
#' @export
setMethod("colSums", 
          signature = (x = "dfm"),
          function(x, ...) Matrix::colSums(as(x, "dgCMatrix"), ...))

#' @method rowSums dfm
#' @rdname dfm-class
#' @export
setMethod("rowSums", 
          signature = (x = "dfm"),
          function(x, ...) Matrix::rowSums(as(x, "dgCMatrix"), ...))

#' @method colMeans dfm
#' @rdname dfm-class
#' @export
setMethod("colMeans", 
          signature = (x = "dfm"),
          function(x, ...) Matrix::colMeans(as(x, "dgCMatrix"), ...))

#' @method rowSums dfm
#' @rdname dfm-class
#' @export
setMethod("rowMeans", 
          signature = (x = "dfm"),
          function(x, ...) Matrix::rowMeans(as(x, "dgCMatrix"), ...))

#' @param e1 first quantity in "+" operation for dfm
#' @param e2 second quantity in "+" operation for dfm
#' @rdname dfm-class
setMethod("Arith", signature(e1 = "dfm", e2 = "numeric"),
          function(e1, e2) {
              switch(.Generic[[1]],
                     `+` = matrix2dfm(as(e1, "dgCMatrix") + e2, attributes(e1)),
                     `-` = matrix2dfm(as(e1, "dgCMatrix") - e2, attributes(e1)),
                     `*` = matrix2dfm(as(e1, "dgCMatrix") * e2, attributes(e1)),
                     `/` = matrix2dfm(as(e1, "dgCMatrix") / e2, attributes(e1)),
                     `^` = matrix2dfm(as(e1, "dgCMatrix") ^ e2, attributes(e1))
              )
          })
#' @rdname dfm-class
setMethod("Arith", signature(e1 = "numeric", e2 = "dfm"),
          function(e1, e2) {
              switch(.Generic[[1]],
                     `+` = matrix2dfm(e1 + as(e2, "dgCMatrix"), attributes(e2)),
                     `-` = matrix2dfm(e1 - as(e2, "dgCMatrix"), attributes(e2)),
                     `*` = matrix2dfm(e1 * as(e2, "dgCMatrix"), attributes(e2)),
                     `/` = matrix2dfm(e1 / as(e2, "dgCMatrix"), attributes(e2)),
                     `^` = matrix2dfm(e1 ^ as(e2, "dgCMatrix"), attributes(e2))
              )
          })


#' Coerce a dfm to a matrix or data.frame
#' 
#' Methods for coercing a \link{dfm} object to a matrix or data.frame object.
#' @rdname as.matrix.dfm
#' @param x dfm to be coerced
#' @param ... unused
#' @export
#' @keywords dfm
#' @method as.matrix dfm
#' @examples
#' # coercion to matrix
#' as.matrix(data_dfm_lbgexample[, 1:10])
#' 
as.matrix.dfm <- function(x, ...) {
    as(x, "matrix")
}


#' Convert a dfm to a data.frame
#' 
#' Deprecated function to convert a dfm into a data.frame.
#' Recommended that you use \code{convert(x, to = "data.frame")} instead.
#' @param document optional first column of mode \code{character} in the
#'   data.frame, defaults \code{docnames(x)}.  Set to \code{NULL} to exclude.
#' @inheritParams base::as.data.frame
#' @inheritParams base::data.frame
#' @param ... unused
#' @method as.data.frame dfm
#' @keywords internal dfm
#' @seealso \code{\link{convert}}
#' @export
as.data.frame.dfm <- function(x, row.names = NULL, ..., document = docnames(x),
                              check.names = FALSE) {
    .Deprecated("convert(x, to = \"data.frame\")")
    if (!(is.character(document) || is.null(document)))
        stop("document must be character or NULL")
    df <- data.frame(as.matrix(x), row.names = row.names, 
                     check.names = check.names)
    if (!is.null(document)) df <- cbind(document, df, stringsAsFactors = FALSE)
    df
}



#' Combine dfm objects by Rows or Columns
#' 
#' Combine a \link{dfm} with another dfm, or numeric, or matrix object, 
#' returning a dfm with the combined documents or features, respectively.
#' 
#' @param ... \link{dfm}, numeric, or matrix  objects to be joined column-wise
#'   (\code{cbind}) or row-wise (\code{rbind}) to the first.  Numeric objects
#'   not confirming to the row or column dimension will be recycled as normal.
#' @details \code{cbind(x, y, ...)} combines dfm objects by columns, returning a
#'   dfm object with combined features from input dfm objects.  Note that this
#'   should be used with extreme caution, as joining dfms with different
#'   documents will result in a new row with the docname(s) of the first dfm,
#'   merging in those from the second.  Furthermore, if features are shared
#'   between the dfms being cbinded, then duplicate feature labels will result.
#'   In both instances, warning messages will result.
#' @export
#' @method cbind dfm
#' @keywords internal dfm
#' @examples 
#' # cbind() for dfm objects
#' (dfmat1 <- dfm(c("a b c d", "c d e f")))
#' (dfmat2 <- dfm(c("a b", "x y z")))
#' cbind(dfmat1, dfmat2)
#' cbind(dfmat1, 100)
#' cbind(100, dfmat1)
#' cbind(dfmat1, matrix(c(101, 102), ncol = 1))
#' cbind(matrix(c(101, 102), ncol = 1), dfmat1)
#' 
cbind.dfm <- function(...) {
    
    args <- list(...)
    names <- names(args) # for non-dfm objects
    
    if (!any(vapply(args, is.dfm, logical(1)))) 
        stop("at least one input object must be a dfm")

    if (length(args) == 1) return(args[[1]])
    
    x <- args[[1]]
    y <- args[[2]]
    attrs <- attributes(x)
    
    if (is.matrix(x)) {
        x <- as.dfm(x)
    } else if (is.numeric(x)) {
        x <- as.dfm(matrix(x, ncol = 1, nrow = nrow(y), 
                           dimnames = list(docs = docnames(y), features = names[1])))
    }
    
    if (is.matrix(y)) {
        y <- as.dfm(y)
    } else if (is.numeric(y)) {
        y <- as.dfm(matrix(y, ncol = 1, nrow = nrow(x), 
                           dimnames = list(docs = docnames(x), features = names[2])))
    }
    
    if (!is.dfm(x) || !is.dfm(y)) stop("all arguments must be dfm objects")
    if (!nfeat(y)) return(x)
    if (any(docnames(x) != docnames(y)))
        warning("cbinding dfms with different docnames", noBreaks. = TRUE, call. = FALSE)
    
    result <-  new("dfm", Matrix::cbind2(x, y))
    if (length(args) > 2) {
        for (i in seq(3, length(args))) {
            result <- cbind(result, args[[i]])
        }
    }
    
    # make any added feature names unique
    i_added <- stri_startswith_fixed(colnames(result), 
                                         quanteda_options("base_featname"))
    colnames(result)[i_added] <- 
        make.unique(colnames(result)[i_added], sep = "")
    
    # only issue warning if these did not come from added feature names
    if (any(duplicated(colnames(result))))
        warning("cbinding dfms with overlapping features will result in duplicated features", 
                noBreaks. = TRUE, call. = FALSE)
    
    # TODO could be removed after upgrading as.dfm()
    set_dfm_dimnames(result) <- dimnames(result)
    slots(result) <- attrs
    result@docvars <- data.frame(matrix(ncol = 0, nrow = nrow(result)))
    return(result)

}

#' @rdname cbind.dfm
#' @details  \code{rbind(x, y, ...)} combines dfm objects by rows, returning a
#'   dfm object with combined features from input dfm objects.  Features are
#'   matched between the two dfm objects, so that the order and names of the
#'   features do not need to match.  The order of the features in the resulting
#'   dfm is not guaranteed.  The attributes and settings of this new dfm are not
#'   currently preserved.
#' @export
#' @method rbind dfm
#' @examples 
#' 
#' # rbind() for dfm objects
#' (dfmat1 <- dfm(c(doc1 = "This is one sample text sample.")))
#' (dfmat2 <- dfm(c(doc2 = "One two three text text.")))
#' (dfmat3 <- dfm(c(doc3 = "This is the fourth sample text.")))
#' rbind(dfmat1, dfmat2)
#' rbind(dfmat1, dfmat2, dfmat3)
rbind.dfm <- function(...) {
    
    args <- list(...)
    if (length(args) == 1) return(args[[1]])

    x <- args[[1]]
    y <- args[[2]]
    attrs <- attributes(x)

    if (!is.dfm(x) || !is.dfm(y)) stop("all arguments must be dfm objects")
    if (!ndoc(y)) return(x)
    
    if (identical(featnames(x), featnames(y))) {
        result <- new("dfm", Matrix::rbind2(x, y))
    } else {
        featname <- union(featnames(x), featnames(y))
        result <- new("dfm", Matrix::rbind2(pad_dfm(x, featname), pad_dfm(y, featname)))
    }
    if (length(args) > 2) {
        for (i in seq(3, length(args))) {
            result <- rbind(result, args[[i]])
        }
    }
    
    # TODO could be removed after upgrading as.dfm()
    set_dfm_dimnames(result) <- dimnames(result)
    slots(result) <- attrs
    result@docvars <- data.frame(matrix(ncol = 0, nrow = nrow(result)))
    return(result)
}
