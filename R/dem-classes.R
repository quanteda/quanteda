#' Virtual class "dem" for a document-feature matrix
#'
#' The dem class of object is a type of [Matrix-class][Matrix::Matrix-class] object with
#' additional slots, described below.  \pkg{quanteda} uses two subclasses of the
#' `dem` class, depending on whether the object can be represented by a
#' sparse matrix, in which case it is a `dem` class object, or if dense,
#' then a `demDense` object.  See Details.
#' @slot weightTf the type of term frequency weighting applied to the dem.  Default is
#'   `"frequency"`, indicating that the values in the cells of the dem are
#'   simple feature counts. To change this, use the [dem_weight()]
#'   method.
#' @slot weightFf the type of document frequency weighting applied to the dem. See
#'   [docfreq()].
#' @slot smooth a smoothing parameter, defaults to zero.  Can be changed using
#'   the [dem_smooth()] method.
#' @slot Dimnames  These are inherited from [Matrix-class][Matrix::Matrix-class] but are
#'   named `docs` and `features` respectively.
#' @details The `dem` class is a virtual class that will contain
#'   [dgeMatrix-class][Matrix::dgeMatrix-class].
#' @seealso [dem]
#' @name dem-class
#' @rdname dem-class
#' @keywords internal dem
setClass("dem",
         slots = c(
                   # weightTf = "list", weightDf = "list",
                   # smooth = "numeric", unit = "character",
                   # ngrams = "integer", skip = "integer",
                   # concatenator = "character", version = "integer",
                   docvars = "data.frame",
                   meta = "list"),
         prototype = list(Dim = integer(2),
                          Dimnames = list(docs = character(), features = character()),
                          docvars = data.frame(row.names = integer()),
                          meta = list(system = list(), object = list(), user = list())),
         contains = "dgeMatrix")


## S4 method dem objects
# @export
# @param x the dem object
# @rdname dem-class
# setMethod("t",
#           signature = (x = "dem"),
#           function(x) { 
#               dnames <- names(x@Dimnames)
#               x <- as.dem(t(as(x, "dgeMatrix")))
#               names(x@Dimnames) <- rev(dnames)
#               return(x)
#           })

#' @method colSums dem
#' @rdname dem-class
#' @param na.rm if `TRUE`, omit missing values (including `NaN`) from
#'   the calculations
#' @param dims ignored
#' @export
setMethod("colSums",
          signature = (x = "dem"),
          function(x, ...) Matrix::colSums(as(x, "dgeMatrix"), ...))

#' @method rowSums dem
#' @rdname dem-class
#' @export
setMethod("rowSums",
          signature = (x = "dem"),
          function(x, ...) Matrix::rowSums(as(x, "dgeMatrix"), ...))

#' @method colMeans dem
#' @rdname dem-class
#' @export
setMethod("colMeans",
          signature = (x = "dem"),
          function(x, ...) Matrix::colMeans(as(x, "dgeMatrix"), ...))

#' @method rowSums dem
#' @rdname dem-class
#' @export
setMethod("rowMeans",
          signature = (x = "dem"),
          function(x, ...) Matrix::rowMeans(as(x, "dgeMatrix"), ...))

#' @param e1 first quantity in an \link[methods:S4groupGeneric]{Arith} operation for dem
#' @param e2 second quantity in an \link[methods:S4groupGeneric]{Arith} operation for dem
#' @rdname dem-class
setMethod("Arith", signature(e1 = "dem", e2 = "numeric"),
          function(e1, e2) {
              switch(.Generic[[1]],
                     `+` = matrix2dem(as(e1, "dgeMatrix") + e2, e1@docvars, e1@meta),
                     `-` = matrix2dem(as(e1, "dgeMatrix") - e2, e1@docvars, e1@meta),
                     `*` = matrix2dem(as(e1, "dgeMatrix") * e2, e1@docvars, e1@meta),
                     `/` = matrix2dem(as(e1, "dgeMatrix") / e2, e1@docvars, e1@meta),
                     `^` = matrix2dem(as(e1, "dgeMatrix") ^ e2, e1@docvars, e1@meta)
              )
          })
#' @rdname dem-class
setMethod("Arith", signature(e1 = "numeric", e2 = "dem"),
          function(e1, e2) {
              switch(.Generic[[1]],
                     `+` = matrix2dem(e1 + as(e2, "dgeMatrix"), e2@docvars, e2@meta),
                     `-` = matrix2dem(e1 - as(e2, "dgeMatrix"), e2@docvars, e2@meta),
                     `*` = matrix2dem(e1 * as(e2, "dgeMatrix"), e2@docvars, e2@meta),
                     `/` = matrix2dem(e1 / as(e2, "dgeMatrix"), e2@docvars, e2@meta),
                     `^` = matrix2dem(e1 ^ as(e2, "dgeMatrix"), e2@docvars, e2@meta)
              )
          })

#' #' @rdname dem-class
#' #' @exportMethod drop0
#' setMethod("drop0", signature = (x = "dem"),
#'           function(x, tol = 0) {
#'               x <- as.dem(x)
#'               attrs <- attributes(x)
#'               x <- Matrix::drop0(as(x, "dgeMatrix"), tol = tol)
#'               build_dem(x, colnames(x),
#'                         docvars = group_docvars(attrs[["docvars"]]),
#'                         meta = attrs[["meta"]]
#'               )
#'           })

#' Coerce a dem to a matrix or data.frame
#'
#' Methods for coercing a [dem] object to a matrix or data.frame object.
#' @rdname as.matrix.dem
#' @param x dem to be coerced
#' @param ... unused
#' @export
#' @keywords dem
#' @method as.matrix dem
#' @examples
#' # coercion to matrix
#' as.matrix(data_dem_lbgexample[, 1:10])
#'
as.matrix.dem <- function(x, ...) {
    as(x, "matrix")
}


#' #' Convert a dem to a data.frame
#' #'
#' #' @description 
#' #' `r lifecycle::badge('superseded')`
#' #' 
#' #' Defunct function to convert a dem into a data.frame.
#' #' Use `convert(x, to = "data.frame")` instead.
#' #' @param document optional first column of mode `character` in the
#' #'   data.frame, defaults `docnames(x)`.  Set to `NULL` to exclude.
#' #' @inheritParams base::as.data.frame
#' #' @inheritParams base::data.frame
#' #' @inheritParams convert
#' #' @param ... unused
#' #' @method as.data.frame dem
#' #' @keywords internal dem
#' #' @seealso [convert()]
#' #' @export
#' as.data.frame.dem <- function(x, row.names = NULL, ..., document = docnames(x),
#'                               docid_field = "doc_id", check.names = FALSE) {
#'     lifecycle::deprecate_stop(
#'             when = "3.0", 
#'             what = "as.data.frame.dem()",
#'             with = I('`convert(x, to = "data.frame"`')
#'         )
#' }
#' 
#' #' Combine dem objects by Rows or Columns
#' #'
#' #' Combine a [dem] with another dem, or numeric, or matrix object,
#' #' returning a dem with the combined documents or features, respectively.
#' #'
#' #' @param ... [dem], numeric, or matrix  objects to be joined column-wise
#' #'   (`cbind`) or row-wise (`rbind`) to the first.  Numeric objects
#' #'   not confirming to the row or column dimension will be recycled as normal.
#' #' @details `cbind(x, y, ...)` combines dem objects by columns, returning a
#' #'   dem object with combined features from input dem objects.  Note that this
#' #'   should be used with extreme caution, as joining dems with different
#' #'   documents will result in a new row with the docname(s) of the first dem,
#' #'   merging in those from the second.  Furthermore, if features are shared
#' #'   between the dems being cbinded, then duplicate feature labels will result.
#' #'   In both instances, warning messages will result.
#' #' @export
#' #' @method cbind dem
#' #' @keywords internal dem
#' #' @examples
#' #' # cbind() for dem objects
#' #' (demat1 <- dem(tokens(c("a b c d", "c d e f"))))
#' #' (demat2 <- dem(tokens(c("a b", "x y z"))))
#' #' cbind(demat1, demat2)
#' #' cbind(demat1, 100)
#' #' cbind(100, demat1)
#' #' cbind(demat1, matrix(c(101, 102), ncol = 1))
#' #' cbind(matrix(c(101, 102), ncol = 1), demat1)
#' #'
#' cbind.dem <- function(...) {
#' 
#'     args <- list(...)
#'     names <- names(args) # for non-dem objects
#' 
#'     if (!any(vapply(args, is.dem, logical(1))))
#'         stop("at least one input object must be a dem")
#' 
#'     if (length(args) == 1) return(args[[1]])
#' 
#'     x <- args[[1]]
#'     y <- args[[2]]
#' 
#'     if (is.matrix(x)) {
#'         x <- as.dem(x)
#'     } else if (is.numeric(x)) {
#'         x <- as.dem(matrix(x, ncol = 1, nrow = nrow(y),
#'                            dimnames = list(docs = docnames(y),
#'                                            features = names[1])))
#'     }
#' 
#'     if (is.matrix(y)) {
#'         y <- as.dem(y)
#'     } else if (is.numeric(y)) {
#'         y <- as.dem(matrix(y, ncol = 1, nrow = nrow(x),
#'                            dimnames = list(docs = docnames(x),
#'                                            features = names[2])))
#'     }
#' 
#'     if (!is.dem(x) || !is.dem(y)) stop("all arguments must be dem objects")
#'     if (!nfeat(y)) return(x)
#'     if (any(docnames(x) != docnames(y)))
#'         warning("cbinding dems with different docnames", noBreaks. = TRUE, call. = FALSE)
#' 
#'     # only issue warning if these did not come from added feature names
#'     if (length(intersect(colnames(x), colnames(y))))
#'         warning("cbinding dems with overlapping features",
#'                 call. = FALSE)
#' 
#'     attrs <- attributes(x)
#'     result <- build_dem(
#'         Matrix::cbind2(x, y),
#'         features = c(colnames(x), colnames(y)),
#'         docvars = attrs[["docvars"]],
#'         meta = attrs[["meta"]]
#'     )
#' 
#'     if (length(args) > 2) {
#'         for (i in seq(3, length(args))) {
#'             result <- cbind(result, args[[i]])
#'         }
#'     }
#'     return(result)
#' }

#' @rdname cbind.dem
#' @details  `rbind(x, y, ...)` combines dem objects by rows, returning a
#'   dem object with combined features from input dem objects.  Features are
#'   matched between the two dem objects, so that the order and names of the
#'   features do not need to match.  The order of the features in the resulting
#'   dem is not guaranteed.  The attributes and settings of this new dem are not
#'   currently preserved.
#' @export
#' @method rbind dem
#' @examples
#'
#' # rbind() for dem objects
#' (demat1 <- dem(tokens(c(doc1 = "This is one sample text sample."))))
#' (demat2 <- dem(tokens(c(doc2 = "One two three text text."))))
#' (demat3 <- dem(tokens(c(doc3 = "This is the fourth sample text."))))
#' rbind(demat1, demat2)
#' rbind(demat1, demat2, demat3)
rbind.dem <- function(...) {

    args <- list(...)
    if (length(args) == 1) return(args[[1]])

    x <- args[[1]]
    y <- args[[2]]

    if (!is.dem(x) || !is.dem(y)) stop("all arguments must be dem objects")

    attrs <- attributes(x)
    #featname <- union(featnames(x), featnames(y))
    result <- build_dem(
        #Matrix::rbind2(pad_dem(x, featname), pad_dem(y, featname)),
        Matrix::rbind2(x, y),
        #features = featname,
        docvars = rbind_fill(get_docvars(x, user = TRUE, system = TRUE),
                             get_docvars(y, user = TRUE, system = TRUE)),
        meta = attrs[["meta"]]
    )

    if (length(args) > 2) {
        for (i in seq(3, length(args))) {
            result <- rbind(result, args[[i]])
        }
    }
    return(result)
}
