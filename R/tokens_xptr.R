#' Methods for tokens_xptr objects
#'
#' Methods for creating and testing for `tokens_xptr` objects, which are
#' [tokens] objects containing pointers to memory locations that can be passed
#' by reference for efficient processing in `tokens_*()` functions that modify
#' them, or for constructing a document-feature matrix without requiring a deep
#' copy to be passed to [dfm()].
#' @name tokens_xptr
#' @keywords tokens
#' @param x a [tokens] object to convert or a `tokens_xptr` class object to deep
#'   copy.
NULL

#' @rdname tokens_xptr
#' @description `is.tokens_xptr()` tests whether an object is of class
#'   `tokens_xtpr`.
#' @returns `is.tokens_xptr()` returns `TRUE` if the object is a external
#'   pointer-based tokens object, `FALSE` otherwise.
#' @export
is.tokens_xptr <- function(x) {
    identical(typeof(x), "externalptr") && "tokens_xptr" %in% class(x)
}

#' @rdname tokens_xptr
#' @description `as.tokens_xptr()` coerces a `tokens` object to an external
#'   pointer-based tokens object, or returns a deep copy of a `tokens_xtpr` when
#'   `x` is already a `tokens_xtpr` object.
#' @returns `as.tokens_xptr()` returns a (deep copy of a) `tokens_xtpr` class
#'   object.
#' @export
as.tokens_xptr <- function(x) {
    UseMethod("as.tokens_xptr")
}

#' @export
as.tokens_xptr.default <- function(x) {
    check_class(class(x), "as.tokens_xptr")
}

#' @rdname tokens_xptr
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

#' @rdname tokens_xptr
#' @method as.tokens_xptr tokens_xptr
#' @export
as.tokens_xptr.tokens_xptr <- function(x) {
    attrs <- attributes(x)
    result <- cpp_copy_xptr(x)
    rebuild_tokens(result, attrs)
}

#' @method lengths tokens_xptr
#' @noRd
#' @export
lengths.tokens_xptr <- function(x, use.names = TRUE) {
    structure(cpp_ntoken(x), 
              names = if (use.names) docnames(x) else NULL)
}

#' @export
ndoc.tokens_xptr <- function(x) {
    cpp_ndoc(x)
}

# -------------------------------------------------------------------------


#' @export
types.tokens_xptr <- function(x) {
    cpp_get_types(x, TRUE)
}

#' @export
concatenator.tokens_xptr <- function(x) {
    get_concatenator(x)
}

#' @export
concat.tokens_xptr <- function(x) {
    get_concatenator(x)
}

#' @export
ntype.tokens_xptr <- function(x, remove_padding = FALSE, ...) {
    remove_padding <- check_logical(remove_padding)
    if (length(list(...)))
        x <- tokens(as.tokens_xptr(x), ...) 
    structure(cpp_ntype(x, remove_padding), names = docnames(x))
}

#' @export
ntoken.tokens_xptr <- function(x, remove_padding = FALSE, ...) {
    remove_padding <- check_logical(remove_padding)
    if (length(list(...)))
        x <- tokens(as.tokens_xptr(x), ...) 
    structure(cpp_ntoken(x, remove_padding), names = docnames(x))
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
    result <- cpp_as_tokens(x)
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
    if (missing(i)) return(as.tokens_xptr(x))
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

#' @export
"[[.tokens_xptr" <- function(x, i) {
    unlist_character(as.list(as.tokens(x[head(i, 1)])), use.names = FALSE)
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
tokens_subset.tokens_xptr <- function(x, subset, min_ntoken = NULL, max_ntoken = NULL, 
                                      drop_docid = TRUE, ...) {
    
    min_ntoken <- check_integer(min_ntoken, min = 0, allow_null = TRUE)
    max_ntoken <- check_integer(max_ntoken, min = 0, allow_null = TRUE)
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
    
    l <- if (is.null(min_ntoken) && is.null(max_ntoken)) {
        rep_len(TRUE, ndoc(x))
    } else {
        n <- ntoken(x)
        if (is.null(min_ntoken)) min_ntoken <- 0L
        if (is.null(max_ntoken)) max_ntoken <- max(n)
        min_ntoken <= n & n <= max_ntoken
    }
    
    return(x[r & l, drop_docid = drop_docid])
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
