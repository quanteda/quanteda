subset_dem <- function(x, i, j, ..., drop) {

    if (missing(i) && missing(j)) return(x)
    x <- as.dem(x)
    attrs <- attributes(x)
    if (nargs() == 2)
        stop("Subscript out of bounds")
    if (!missing(i)) {
        index_row <- seq_len(nrow(x))
        names(index_row) <- rownames(x)
        index_row <- index_row[i]
        if (any(is.na(index_row)))
            stop("Subscript out of bounds")
    }
    if (!missing(j)) {
        stop("Cannot subset columns of dem objects")
    }
    # if (!missing(j)) {
    #     index_col <- seq_len(ncol(x))
    #     names(index_col) <- colnames(x)
    #     index_col <- index_col[j]
    #     if (any(is.na(index_col)))
    #         stop("Subscript out of bounds")
    # }

    if (!missing(i) && missing(j)) {
        x <- "["(as(x, "Matrix"), i, , drop = FALSE)
    }
    # } else if (missing(i) && !missing(j)) {
    #     x <- "["(as(x, "Matrix"), , j, drop = FALSE)
    # } else {
    #     x <- "["(as(x, "Matrix"), i, j, drop = FALSE)
    # }

    if (!missing(i))
        attrs[["docvars"]] <- reshape_docvars(attrs[["docvars"]], index_row, unique = FALSE, ...)

    build_dem(
        x, colnames(x),
        docvars = attrs[["docvars"]],
        meta = attrs[["meta"]]
    )
}

#' @param i document names or indices for documents to extract.
#' @param j feature names or indices for documents to extract.
#' @param ... additional arguments not used here
#' @rdname dem-class
#' @export
#' @examples
#' # dem subsetting
#' demat <- dem(tokens(c("this contains lots of stopwords",
#'                   "no if, and, or but about it: lots",
#'                   "and a third document is it"),
#'                 remove_punct = TRUE))
#' demat[1:2, ]
#' demat[1:2, 1:5]
setMethod("[", signature = c("dem", i = "index", j = "index", drop = "missing"), subset_dem)

#' @rdname dem-class
#' @export
setMethod("[", signature = c("dem", i = "index", j = "index", drop = "logical"), subset_dem)

#' @rdname dem-class
#' @export
setMethod("[", signature = c("dem", i = "missing", j = "missing", drop = "missing"), subset_dem)

#' @rdname dem-class
#' @export
setMethod("[", signature = c("dem", i = "missing", j = "missing", drop = "logical"), subset_dem)

#' @rdname dem-class
#' @export
setMethod("[", signature = c("dem", i = "index", j = "missing", drop = "missing"), subset_dem)

#' @rdname dem-class
#' @export
setMethod("[", signature = c("dem", i = "index", j = "missing", drop = "logical"), subset_dem)

#' @rdname dem-class
#' @export
setMethod("[", signature = c("dem", i = "missing", j = "index", drop = "missing"), subset_dem)

#' @rdname dem-class
#' @export
setMethod("[", signature = c("dem", i = "missing", j = "index", drop = "logical"), subset_dem)

#' @noRd
#' @method "[[" dem
#' @inheritParams dem-class
#' @export
"[[.dem" <- function(x, i) {
    stop("[[ not defined for dem objects", call. = FALSE)
}

## can be removed later, but necessary in v3 for oldrelease checks

#' Return the first or last part of a dem
#' 
#' For a [dem] object, return the dem with only the first or last `n` documents.
#' @param x a [dem] object
#' @inheritParams utils::head
#' @return A [dem] class object corresponding to the subset of documents
#'   determined by by `n`.
#' @export
#' @name head.dem
#' @method head dem
#' @keywords dem internal
#' @examples
#' head(data_dem_lbgexample, 3)
#' head(data_dem_lbgexample, -4)
#' 
head.dem <- function(x, n = 6L, ...) { 
    x <- as.dem(x)
    check_dots(...)
    n <- check_integer(n)
    i <- seq_len(ndoc(x))
    x[i %in% head(i, n), ]
}

#' @rdname head.dem
#' @method tail dem
#' @export
#' @examples 
#' tail(data_dem_lbgexample)
#' tail(data_dem_lbgexample, n = 3)
tail.dem <- function(x, n = 6L, ...) { 
    x <- as.dem(x)
    check_dots(...)
    n <- check_integer(n)
    i <- seq_len(ndoc(x))
    x[i %in% tail(i, n), ]
}

setMethod("head", signature(x = "dem"), function(x, n = 6L, ...) { 
    UseMethod("head")
})
setMethod("tail", signature(x = "dem"), function(x, n = 6L, ...) { 
    UseMethod("tail")
})
