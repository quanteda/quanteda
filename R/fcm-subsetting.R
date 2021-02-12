
subset_fcm <- function(x, i, j, ..., drop) {
    
    if (missing(i) && missing(j)) return(x)
    x <- as.fcm(x)
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
    
    build_fcm(
        x, rownames(x), colnames(x),
        meta = attrs[["meta"]]
    )
}

#' @param i index for features
#' @param j index for features
#' @param drop always set to `FALSE`
#' @param ... additional arguments not used here
#' @rdname fcm-class
#' @export
#' @examples 
#' # fcm subsetting
#' fcmat <- fcm(tokens(c("this contains lots of stopwords",
#'                   "no if, and, or but about it: lots"),
#'                 remove_punct = TRUE))
#' fcmat[1:3, ]
#' fcmat[4:5, 1:5]
#' 
#' 
setMethod("[", signature = c("fcm", i = "index", j = "index", drop = "missing"), subset_fcm)

#' @rdname fcm-class
#' @export
setMethod("[", signature = c("fcm", i = "index", j = "index", drop = "logical"), subset_fcm)

#' @rdname fcm-class
#' @export
setMethod("[", signature = c("fcm", i = "missing", j = "missing", drop = "missing"), subset_fcm)

#' @rdname fcm-class
#' @export
setMethod("[", signature = c("fcm", i = "missing", j = "missing", drop = "logical"), subset_fcm)

#' @rdname fcm-class
#' @export
setMethod("[", signature = c("fcm", i = "index", j = "missing", drop = "missing"), subset_fcm)

#' @rdname fcm-class
#' @export
setMethod("[", signature = c("fcm", i = "index", j = "missing", drop = "logical"), subset_fcm)

#' @rdname fcm-class
#' @export
setMethod("[", signature = c("fcm", i = "missing", j = "index", drop = "missing"), subset_fcm)

#' @rdname fcm-class
#' @export
setMethod("[", signature = c("fcm", i = "missing", j = "index", drop = "logical"), subset_fcm)
