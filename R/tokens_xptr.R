#' @rdname as.tokens_xptr
#' @export
is.tokens_xptr <- function(x) {
    identical(typeof(x), "externalptr") && "tokens_xptr" %in% class(x)
}

#' Convert a tokens object to a tokens_xptr object
#'
#' tokens_xptr is a external pointer object to process a large number of
#' documents efficiently.
#' @param x a [tokens] object to convert or a tokens_xptr object to
#'   to deep-copy.
#' @rdname as.tokens_xptr
#' @export
as.tokens_xptr <- function(x) {
    UseMethod("as.tokens_xptr")
}

#' @rdname as.tokens_xptr
#' @method as.tokens_xptr tokens
#' @export
as.tokens_xptr.tokens <- function(x) {
    attrs <- attributes(x)
    result <- cpp_as_xptr(x, attrs$types)
    build_tokens(result, 
                 types = NULL, 
                 padding = TRUE, 
                 docvars = attrs[["docvars"]], 
                 meta = attrs[["meta"]], 
                 class = attrs[["class"]])
}

#' @rdname as.tokens_xptr
#' @method as.tokens_xptr tokens_xptr
#' @export
as.tokens_xptr.tokens_xptr <- function(x) {
    attrs <- attributes(x)
    result <- cpp_copy_xptr(x)
    rebuild_tokens(result, attrs)
}

#' @export
ndoc.tokens_xptr <- function(x) {
    cpp_ndoc(x)
}

#' @export
types.tokens_xptr <- function(x) {
    cpp_get_types(x, TRUE)
}

#' @export
ntoken.tokens_xptr <- function(x, ...) {
    structure(cpp_ntoken(x), names = docnames(x))
}

# #' @export
# docnames.tokens_xptr <- function(x) {
#     get_docvars(x, "docname_", FALSE, TRUE, TRUE)
# }
# 
# #' @export
# docid.tokens_xptr <- function(x) {
#     get_docvars(x, "docid_", FALSE, TRUE, TRUE)
# }
# 
# #' @export
# segid.tokens_xptr <- function(x) {
#     get_docvars(x, "segid_", FALSE, TRUE, TRUE)
# }

#' @method as.tokens tokens_xptr
#' @export
as.tokens.tokens_xptr <- function(x, ...) {
    attrs <- attributes(x)
    result <- cpp_as_list(x)
    build_tokens(result, 
                 types = attr(result, "types"), 
                 padding = TRUE, 
                 docvars = attrs[["docvars"]], 
                 meta = attrs[["meta"]],
                 class = attrs[["class"]])
}

#' @method as.list tokens_xptr
#' @export
as.list.tokens_xptr <- function(x, ...) {
    as.list(as.tokens(x), ...)
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
    
    build_tokens(
        cpp_subset(x, index),
        attrs[["types"]],
        docvars = reshape_docvars(attrs[["docvars"]], index, drop_docid = drop_docid),
        meta = attrs[["meta"]],
        class = attrs[["class"]]
    )
}

#' @method head tokens_xptr
#' @export
head.tokens_xptr <- function(x, n = 6L, ...) {
    x[head(seq_len(ndoc(x), ...), n)]
}

#' @method tail tokens_xptr
#' @export
tail.tokens_xptr <- function(x, n = 6L, ...) {
    x[tail(seq_len(ndoc(x), ...), n)]
}


#' @export
tokens_subset.tokens_xptr <- function(x, subset, drop_docid = TRUE, ...) {
    
    check_dots(...)
    
    attrs <- attributes(x)
    docvar <- get_docvars(x, user = TRUE, system = TRUE)
    r <- if (missing(subset)) {
        rep_len(TRUE, ndoc(x))
    } else {
        e <- substitute(subset)
        r <- eval(e, docvar, parent.frame())
        r & !is.na(r)
    }
    return(x[r, drop_docid = drop_docid])
}

#' @method dfm tokens_xptr
#' @export
dfm.tokens_xptr <- function(x, tolower = TRUE, remove_padding = FALSE, ...) {
    x <- as.tokens_xptr(x) # avoid modifying the original tokens
    if (tolower)
        x <- tokens_tolower(x)
    if (remove_padding)
        x <- tokens_remove(x, "", valuetype = "fixed")
    attrs <- attributes(x)
    temp <- t(cpp_dfm(x, attrs$meta$object$what != "dictionary"))
    build_dfm(temp, colnames(temp),
              docvars = get_docvars(x, user = TRUE, system = TRUE),
              meta = attrs[["meta"]])
}

#' @export
tokens_tolower.tokens_xptr <- function(x, keep_acronyms = FALSE) {
    keep_acronyms <- check_logical(keep_acronyms)
    # NOTE: consider removing keep_acronyms
    set_types(x) <- lowercase_types(get_types(x), keep_acronyms) 
    return(x)
}

#' @noRd
#' @export
tokens_toupper.tokens_xptr <- function(x) {
    set_types(x) <- char_toupper(types(x))
    return(x)
}

# internal functions ----------------------------------------

# #' @method get_docvars tokens_xptr
# get_docvars.tokens_xptr <- function(x, field = NULL, user = TRUE, system = FALSE, drop = FALSE) {
#    select_docvars(attr(x, "docvars"), field, user, system, drop)
#}

get_types <- function(x) {
    UseMethod("get_types")
}

#' @method get_types tokens
get_types.tokens <- function(x) {
    attr(x, "types")
}

#' @method get_types tokens_xptr
get_types.tokens_xptr <- function(x) {
    cpp_get_types(x)
}

"set_types<-" <- function(x, value) {
    if (!is.character(value))
        stop("replacement value must be character")
    UseMethod("set_types<-")
}

#' @method set_types tokens
"set_types<-.tokens" <- function(x, value) { # TODO: remove types<-.tokens
    attr(x, "types") <- value
    return(x)
}

#' @method set_types tokens_xptr
"set_types<-.tokens_xptr" <- function(x, value) {
    cpp_set_types(x, value)
}