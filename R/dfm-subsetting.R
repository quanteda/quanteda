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
    check_dots(...)
    
    n <- check_integer(n)
    nf <- check_integer(nf)
    i <- seq_len(ndoc(x))
    j <- seq_len(nfeat(x))
    x[i %in% head(i, n), j %in% head(j, nf)]
}


#' @rdname head.dfm
#' @method tail dfm
#' @export
#' @examples 
#' tail(data_dfm_lbgexample)
#' tail(data_dfm_lbgexample, n = 3, nf = 4)
tail.dfm <- function(x, n = 6L, nf = nfeat(x), ...) { 
    
    x <- as.dfm(x)
    check_dots(...)
    
    n <- check_integer(n)
    nf <- check_integer(nf)
    i <- seq_len(ndoc(x))
    j <- seq_len(nfeat(x))
    x[i %in% tail(i, n), j %in% tail(j, nf)]
}

setMethod("head", signature(x = "dfm"), function(x, n = 6L, nf = nfeat(x), ...) { 
    UseMethod("head")
})
setMethod("tail", signature(x = "dfm"), function(x, n = 6L, nf = nfeat(x), ...) { 
    UseMethod("tail")
})
