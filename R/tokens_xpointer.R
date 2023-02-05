#' @export
as.externalptr <- function(x) {
    UseMethod("as.externalptr")
}

#' @method as.externalptr tokens
#' @export
as.externalptr.tokens <- function(x) {
    attrs <- attributes(x)
    result <- qatd_cpp_as_xptr(x, attrs$types)
    rebuild_tokens(result, attrs)
}

#' @method as.externalptr externalptr
#' @export
as.externalptr.externalptr <- function(x) {
    attrs <- attributes(x)
    result <- qatd_cpp_copy_xptr(x)
    rebuild_tokens(result, attrs)
}

#' @export
ndoc.externalptr <- function(x) {
    qatd_cpp_ndoc(x)
}

#' @export
types.externalptr <- function(x) {
    qatd_cpp_types(x)
}

#' @method as.tokens externalptr
#' @export
as.tokens.externalptr <- function(x) {
    attrs <- attributes(x)
    result <- qatd_cpp_as_list(x)
    rebuild_tokens(result, attrs)
}

#' @method as.list externalptr
#' @export
as.list.externalptr <- function(x) {
    as.list(as.tokens(x))
}
