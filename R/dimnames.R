#' Internal functions to set dimnames
#'
#' Default `dimnames()` converts a zero-length character vector to NULL,
#' leading to the improper functioning of subsetting functions. These are safer
#' methods to set the dimnames of a dfm or fcm object.
#' @param x [dfm] or [fcm]
#' @param value character a vector for docnames or featnames or a list of them
#'   for dimnames
#' @keywords internal
#' @examples 
#' dfmat <- dfm(c("a a b b c", "b b b c"))
#' quanteda:::set_dfm_featnames(dfmat) <- paste0("feature", 1:3)
#' quanteda:::set_dfm_docnames(dfmat) <- paste0("DOC", 1:2)
#' quanteda:::set_dfm_dimnames(dfmat) <- list(c("docA", "docB"), LETTERS[1:3])
#' @rdname set_dfm_dimnames
"set_dfm_dimnames<-" <- function(x, value) {
    if (is.null(value[[1]])) value[[1]] <- character()
    if (is.null(value[[2]])) value[[2]] <- character()
    stopifnot(nrow(x) == length(value[[1]]))
    stopifnot(ncol(x) == length(value[[2]]))
    x@Dimnames <- list("docs" = as.character(value[[1]]), 
                       "features" = as.character(value[[2]]))
    return(x)
}

#' @rdname set_dfm_dimnames
"set_dfm_docnames<-" <- function(x, value) {
    stopifnot(nrow(x) == length(value))
    x@Dimnames[[1]] <- as.character(value)
    return(x)
}

#' @rdname set_dfm_dimnames
"set_dfm_featnames<-" <- function(x, value) {
    if (is.null(value)) value <- character()
    stopifnot(ncol(x) == length(value))
    x@Dimnames[[2]] <- as.character(value)
    return(x)
}

#' @rdname set_dfm_dimnames
"set_fcm_dimnames<-" <- function(x, value) {
    if (is.null(value[[1]])) value[[1]] <- character()
    if (is.null(value[[2]])) value[[2]] <- character()
    stopifnot(nrow(x) == length(value[[1]]))
    stopifnot(ncol(x) == length(value[[2]]))
    x@Dimnames <- list("features" = as.character(value[[1]]), 
                       "features" = as.character(value[[2]]))
    return(x)
}

#' @rdname set_dfm_dimnames
"set_fcm_featnames<-" <- function(x, value) {
    if (is.null(value)) value <- character()
    stopifnot(nrow(x) == length(value))
    stopifnot(ncol(x) == length(value))
    x@Dimnames[[1]] <- x@Dimnames[[2]] <- as.character(value)
    return(x)
}
