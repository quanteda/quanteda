subset_dfm <- function(x, i, j, ..., drop) {
    
    attrs <- attributes(x)
    error <- FALSE
    if (!missing(i)) {
        if (is.character(i) && any(!i %in% rownames(x))) error <- TRUE
        if (is.numeric(i) && any(i > nrow(x))) error <- TRUE
    }
    if (!missing(j)) {
        if (is.character(j) && any(!j %in% colnames(x))) error <- TRUE
        if (is.numeric(j) && any(j > ncol(x))) error <- TRUE
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
        attrs$docvars <- attrs$docvars[i, , drop = FALSE]
    matrix2dfm(x, attrs)
}

#' @param i index for documents
#' @param j index for features
#' @param drop always set to \code{FALSE}
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

