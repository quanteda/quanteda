subset_dfm <- function(x, i, j, ..., drop) {
    
    attrs <- attributes(x)
    error <- FALSE
    if (!missing(i)) {
        if (is.character(i))
            i <- match(i, rownames(x))
        if (is.numeric(i) && (any(is.na(i)) || any(i < nrow(x) * -1L) || any(nrow(x) < i)))
            error <- TRUE
        #if (is.logical(i) && length(i) != nrow(x))
        #    error <- TRUE
    }
    if (!missing(j)) {
        if (is.character(j))
            j <- match(j, colnames(x))
        if (is.numeric(j) && (any(is.na(j)) || any(j < ncol(x) * -1L) || any(ncol(x) < j)))
            error <- TRUE
        #if (is.logical(j) && length(j) != ncol(x))
        #    error <- TRUE
    }
    if (error) stop("Subscript out of bounds")
    
    if (missing(i) && missing(j)) {
        return(x)
    } else if (!missing(i) && missing(j)) {
        x <- "["(as(x, "Matrix"), i, , ..., drop = FALSE)
    } else if (missing(i) && !missing(j)) {
        x <- "["(as(x, "Matrix"), , j, ..., drop = FALSE)
    } else {
        x <- "["(as(x, "Matrix"), i, j, ..., drop = FALSE)    
    }
    
    if (!missing(i))
        attrs$docvars <- subset_docvars(attrs$docvars, i)
    matrix2dfm(x, attrs)
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

