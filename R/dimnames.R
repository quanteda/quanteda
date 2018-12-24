#' Internal functions to set dimnames
#'
#' Default dimnames() converts a zero-length character vector to NULL, leading
#' to disfunctioning of subsetting functions. These are safer methods to set
#' dimnames of a dfm or fcm.
#' @param x a dfm or fcm
#' @param value character a vector for docnames or featmanes or a list of them
#'   for dimnames
#' @keywords internal
#' @rdname set_dfm_dimnames
"set_dfm_dimnames<-" <- function(x, value) {
    if (is.null(value[[1]])) value[[1]] <- character()
    if (is.null(value[[2]])) value[[2]] <- character()
    stopifnot(nrow(x) == length(value[[1]]))
    stopifnot(ncol(x) == length(value[[2]]))
    x@Dimnames <- list("docs" = value[[1]], "features" = value[[2]])
    return(x)
}

#' @rdname set_dfm_dimnames
"set_dfm_docnames<-" <- function(x, value) {
    stopifnot(nrow(x) == length(value))
    x@Dimnames[[1]] <- value
    return(x)
}

#' @rdname set_dfm_dimnames
"set_dfm_featnames<-" <- function(x, value) {
    if (is.null(value)) value <- character()
    stopifnot(ncol(x) == length(value))
    x@Dimnames[[2]] <- value
    return(x)
}

#' @rdname set_dfm_dimnames
"set_fcm_dimnames<-" <- function(x, value) {
    if (is.null(value[[1]])) value[[1]] <- character()
    if (is.null(value[[2]])) value[[2]] <- character()
    stopifnot(nrow(x) == length(value[[1]]))
    stopifnot(ncol(x) == length(value[[2]]))
    x@Dimnames <- list("features" = value[[1]], "features" = value[[2]])
    return(x)
}

#' @rdname set_dfm_dimnames
"set_fcm_featnames<-" <- function(x, value) {
    if (is.null(value)) value <- character()
    stopifnot(nrow(x) == length(value))
    stopifnot(ncol(x) == length(value))
    x@Dimnames[[1]] <- x@Dimnames[[2]] <- value
    return(x)
}


