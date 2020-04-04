#' Virtual class "dfm" for a document-feature matrix
#'
#' The dfm class of object is a type of [Matrix-class][Matrix::Matrix-class] object with
#' additional slots, described below.  \pkg{quanteda} uses two subclasses of the
#' `dfm` class, depending on whether the object can be represented by a
#' sparse matrix, in which case it is a `dfm` class object, or if dense,
#' then a `dfmDense` object.  See Details.
#' @slot weightTf the type of term frequency weighting applied to the dfm.  Default is
#'   `"frequency"`, indicating that the values in the cells of the dfm are
#'   simple feature counts. To change this, use the [dfm_weight()]
#'   method.
#' @slot weightFf the type of document frequency weighting applied to the dfm. See
#'   [docfreq()].
#' @slot smooth a smoothing parameter, defaults to zero.  Can be changed using
#'   the [dfm_smooth()] method.
#' @slot Dimnames  These are inherited from [Matrix-class][Matrix::Matrix-class] but are
#'   named `docs` and `features` respectively.
#' @details The `dfm` class is a virtual class that will contain
#'   [dgCMatrix-class][Matrix::dgCMatrix-class].
#' @seealso [dfm]
#' @name dfm-class
#' @rdname dfm-class
#' @keywords internal dfm
setClass("dfm",
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
         contains = "dgCMatrix")


## S4 method dfm objects
#' @export
#' @param x the dfm object
#' @rdname dfm-class
setMethod("t",
          signature = (x = "dfm"),
          function(x) { 
              dnames <- names(x@Dimnames)
              x <- as.dfm(t(as(x, "dgCMatrix")))
              names(x@Dimnames) <- rev(dnames)
              return(x)
          })

#' @method colSums dfm
#' @rdname dfm-class
#' @param na.rm if `TRUE`, omit missing values (including `NaN`) from
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
                     `+` = matrix2dfm(as(e1, "dgCMatrix") + e2, e1@docvars, e1@meta),
                     `-` = matrix2dfm(as(e1, "dgCMatrix") - e2, e1@docvars, e1@meta),
                     `*` = matrix2dfm(as(e1, "dgCMatrix") * e2, e1@docvars, e1@meta),
                     `/` = matrix2dfm(as(e1, "dgCMatrix") / e2, e1@docvars, e1@meta),
                     `^` = matrix2dfm(as(e1, "dgCMatrix") ^ e2, e1@docvars, e1@meta)
              )
          })
#' @rdname dfm-class
setMethod("Arith", signature(e1 = "numeric", e2 = "dfm"),
          function(e1, e2) {
              switch(.Generic[[1]],
                     `+` = matrix2dfm(e1 + as(e2, "dgCMatrix"), e2@docvars, e2@meta),
                     `-` = matrix2dfm(e1 - as(e2, "dgCMatrix"), e2@docvars, e2@meta),
                     `*` = matrix2dfm(e1 * as(e2, "dgCMatrix"), e2@docvars, e2@meta),
                     `/` = matrix2dfm(e1 / as(e2, "dgCMatrix"), e2@docvars, e2@meta),
                     `^` = matrix2dfm(e1 ^ as(e2, "dgCMatrix"), e2@docvars, e2@meta)
              )
          })


#' Coerce a dfm to a matrix or data.frame
#'
#' Methods for coercing a [dfm] object to a matrix or data.frame object.
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
#' Recommended that you use `convert(x, to = "data.frame")` instead.
#' @param document optional first column of mode `character` in the
#'   data.frame, defaults `docnames(x)`.  Set to `NULL` to exclude.
#' @inheritParams base::as.data.frame
#' @inheritParams base::data.frame
#' @inheritParams convert
#' @param ... unused
#' @method as.data.frame dfm
#' @keywords internal dfm
#' @seealso [convert()]
#' @export
as.data.frame.dfm <- function(x, row.names = NULL, ..., document = docnames(x),
                              docid_field = "doc_id", check.names = FALSE) {
    .Deprecated("convert(x, to = \"data.frame\")")
    result <- dfm2dataframe(x, row.names = NULL, ..., document = document,
                            docid_field = docid_field, check.names = check.names)
    if (!is.null(row.names))
        row.names(result) <- row.names
    result
}


#' Combine dfm objects by Rows or Columns
#'
#' Combine a [dfm] with another dfm, or numeric, or matrix object,
#' returning a dfm with the combined documents or features, respectively.
#'
#' @param ... [dfm], numeric, or matrix  objects to be joined column-wise
#'   (`cbind`) or row-wise (`rbind`) to the first.  Numeric objects
#'   not confirming to the row or column dimension will be recycled as normal.
#' @details `cbind(x, y, ...)` combines dfm objects by columns, returning a
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

    if (is.matrix(x)) {
        x <- as.dfm(x)
    } else if (is.numeric(x)) {
        x <- as.dfm(matrix(x, ncol = 1, nrow = nrow(y),
                           dimnames = list(docs = docnames(y),
                                           features = names[1])))
    }

    if (is.matrix(y)) {
        y <- as.dfm(y)
    } else if (is.numeric(y)) {
        y <- as.dfm(matrix(y, ncol = 1, nrow = nrow(x),
                           dimnames = list(docs = docnames(x),
                                           features = names[2])))
    }

    if (!is.dfm(x) || !is.dfm(y)) stop("all arguments must be dfm objects")
    if (!nfeat(y)) return(x)
    if (any(docnames(x) != docnames(y)))
        warning("cbinding dfms with different docnames", noBreaks. = TRUE, call. = FALSE)

    # only issue warning if these did not come from added feature names
    if (length(intersect(colnames(x), colnames(y))))
        warning("cbinding dfms with overlapping features",
                call. = FALSE)

    attrs <- attributes(x)
    result <- build_dfm(
        Matrix::cbind2(x, y),
        features = c(colnames(x), colnames(y)),
        docvars = attrs[["docvars"]],
        meta = attrs[["meta"]]
    )

    if (length(args) > 2) {
        for (i in seq(3, length(args))) {
            result <- cbind(result, args[[i]])
        }
    }
    return(result)
}

#' @rdname cbind.dfm
#' @details  `rbind(x, y, ...)` combines dfm objects by rows, returning a
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

    if (!is.dfm(x) || !is.dfm(y)) stop("all arguments must be dfm objects")

    attrs <- attributes(x)
    featname <- union(featnames(x), featnames(y))
    result <- build_dfm(
        Matrix::rbind2(pad_dfm(x, featname), pad_dfm(y, featname)),
        features = featname,
        docvars = make_docvars(nrow(x) + nrow(y), c(docnames(x), docnames(y)), unique = FALSE),
        meta = attrs[["meta"]]
    )

    if (length(args) > 2) {
        for (i in seq(3, length(args))) {
            result <- rbind(result, args[[i]])
        }
    }
    return(result)
}
