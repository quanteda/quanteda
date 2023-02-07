#' @export
as.tokens_xptr <- function(x) {
    UseMethod("as.tokens_xptr")
}

#' @method as.tokens_xptr tokens
#' @export
as.tokens_xptr.tokens <- function(x) {
    attrs <- attributes(x)
    result <- qatd_cpp_as_xptr(x, attrs$types)
    build_tokens(result, 
                 types = NULL, 
                 padding = TRUE, 
                 docvars = attrs[["docvars"]], 
                 meta = attrs[["meta"]], 
                 class = "tokens_xptr")
}

#' @method as.tokens_xptr tokens_xptr
#' @export
as.tokens_xptr.tokens_xptr <- function(x) {
    attrs <- attributes(x)
    result <- qatd_cpp_copy_xptr(x)
    rebuild_tokens(result, attrs)
}

#' @export
ndoc.tokens_xptr <- function(x) {
    qatd_cpp_ndoc(x)
}

#' @export
types.tokens_xptr <- function(x) {
    qatd_cpp_types(x)
}

#' @export
ntoken.tokens_xptr <- function(x) {
    structure(qatd_cpp_ntoken(x), names = docnames(x))
}

#' @export
docnames.tokens_xptr <- function(x) {
    get_docvars(x, "docname_", FALSE, TRUE, TRUE)
}

#' @method as.tokens tokens_xptr
#' @export
as.tokens.tokens_xptr <- function(x) {
    attrs <- attributes(x)
    result <- qatd_cpp_as_list(x)
    build_tokens(result, 
                 types = attr(result, "types"), 
                 padding = attr(result, "padding"), 
                 docvars = attrs[["docvars"]], 
                 meta = attrs[["meta"]])
}

#' @method as.list tokens_xptr
#' @export
as.list.tokens_xptr <- function(x) {
    as.list(as.tokens(x))
}

#' @method [ tokens_xptr
#' @export
"[.tokens_xptr" <- function(x, i, drop_docid = TRUE) {
    if (missing(i)) return(x)
    attrs <- attributes(x)
    
    index <- seq_along(docnames(x))
    names(index) <- docnames(x)
    index <- index[i]
    
    if (any(is.na(index)))
        stop("Subscript out of bounds")
    
    result <- build_tokens(
        qatd_cpp_subset(x, index),
        attrs[["types"]],
        docvars = reshape_docvars(attrs[["docvars"]], index, drop_docid = drop_docid),
        meta = attrs[["meta"]],
        class = attrs[["class"]]
    )
    
}

# internal functions ----------------------------------------

#' @method get_docvars tokens_xptr
get_docvars.tokens_xptr <- function(x, field = NULL, user = TRUE, system = FALSE, drop = FALSE) {
    select_docvars(attr(x, "docvars"), field, user, system, drop)
}

