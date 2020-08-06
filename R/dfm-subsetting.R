subset_dfm <- function(x, i, j, ..., drop) {

    if (missing(i) && missing(j)) return(x)
    x <- as.dfm(x)
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
        index_col <- seq_len(ncol(x))
        names(index_col) <- colnames(x)
        index_col <- index_col[j]
        if (any(is.na(index_col)))
            stop("Subscript out of bounds")
    }

    if (!missing(i) && missing(j)) {
        x <- "["(as(x, "Matrix"), i, , ..., drop = FALSE)
    } else if (missing(i) && !missing(j)) {
        x <- "["(as(x, "Matrix"), , j, ..., drop = FALSE)
    } else {
        x <- "["(as(x, "Matrix"), i, j, ..., drop = FALSE)
    }

    if (!missing(i))
        attrs[["docvars"]] <- reshape_docvars(attrs[["docvars"]], index_row)

    build_dfm(
        x, colnames(x),
        docvars = attrs[["docvars"]],
        meta = attrs[["meta"]]
    )
}

#' @param i index for documents
#' @param j index for features
#' @param drop always set to `FALSE`
#' @param ... additional arguments not used here
#' @rdname dfm-class
#' @export
#' @examples
#' # dfm subsetting
#' dfmat <- dfm(tokens(c("this contains lots of stopwords",
#'                   "no if, and, or but about it: lots",
#'                   "and a third document is it"),
#'                 remove_punct = TRUE))
#' dfmat[1:2, ]
#' dfmat[1:2, 1:5]
setMethod("[", signature = c("dfm", i = "index", j = "index", drop = "missing"), subset_dfm)

#' @rdname dfm-class
#' @export
setMethod("[", signature = c("dfm", i = "index", j = "index", drop = "logical"), subset_dfm)

#' @rdname dfm-class
#' @export
setMethod("[", signature = c("dfm", i = "missing", j = "missing", drop = "missing"), subset_dfm)

#' @rdname dfm-class
#' @export
setMethod("[", signature = c("dfm", i = "missing", j = "missing", drop = "logical"), subset_dfm)

#' @rdname dfm-class
#' @export
setMethod("[", signature = c("dfm", i = "index", j = "missing", drop = "missing"), subset_dfm)

#' @rdname dfm-class
#' @export
setMethod("[", signature = c("dfm", i = "index", j = "missing", drop = "logical"), subset_dfm)

#' @rdname dfm-class
#' @export
setMethod("[", signature = c("dfm", i = "missing", j = "index", drop = "missing"), subset_dfm)

#' @rdname dfm-class
#' @export
setMethod("[", signature = c("dfm", i = "missing", j = "index", drop = "logical"), subset_dfm)

#' @noRd
#' @method "[[" dfm
#' @inheritParams dfm-class
#' @export
"[[.dfm" <- function(x, i) {
    stop("[[ not defined for a dfm/fcm object", call. = FALSE)
}
