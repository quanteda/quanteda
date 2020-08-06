
#' Get or set document-level meta-data
#' 
#' @param x a [corpus] object
#' @param field character, the name of the metadata field(s) to be queried or 
#' @export
#' @keywords corpus
metadoc <- function(x, field = NULL) 
    UseMethod("metadoc")

#' @export
metadoc.default <- function(x, field = NULL) {
    stop(friendly_class_undefined_message(class(x), "metadoc"))
}

#' @noRd
#' @export
metadoc.corpus <- function(x, field = NULL) {
    warning("metadoc is deprecated", call. = FALSE)
    docvar <- attr(x, "docvars")
    l <- stri_startswith_fixed(names(docvar), "_")
    select_docvars(docvar[, l, drop = FALSE], field)
}

#' @noRd
#' @export
metadoc.tokens <- function(x, field = NULL) {
    warning("metadoc is deprecated", call. = FALSE)
    docvar <- attr(x, "docvars")
    l <- stri_startswith_fixed(names(docvar), "_")
    select_docvars(docvar[, l, drop = FALSE], field)
}

#' @noRd
#' @export
metadoc.dfm <- function(x, field = NULL) {
    warning("metadoc is deprecated", call. = FALSE)
    docvar <- x@docvars
    l <- stri_startswith_fixed(names(docvar), "_")
    select_docvars(docvar[, l, drop = FALSE], field)
}

#' @rdname metadoc
#' @param value the new value of the new meta-data field
#' @export
"metadoc<-" <- function(x, field = NULL, value) 
    UseMethod("metadoc<-")

#' @export
"metadoc<-.default" <- function(x, field = NULL, value) {
    stop(friendly_class_undefined_message(class(x), "metadoc<-"))
}

#' @noRd
#' @export
"metadoc<-.corpus" <- function(x, field = NULL, value) {
    warning("metadoc is deprecated", call. = FALSE)
    docvars(x, paste0("_", field)) <- value
    return(x)
}

#' @noRd
#' @export
"metadoc<-.tokens" <- function(x, field = NULL, value) {
    warning("metadoc is deprecated", call. = FALSE)
    docvars(x, paste0("_", field)) <- value
    return(x)
}

#' @noRd
#' @export
"metadoc<-.dfm" <- function(x, field = NULL, value) {
    warning("metadoc is deprecated", call. = FALSE)
    docvars(x, paste0("_", field)) <- value
    return(x)
}
