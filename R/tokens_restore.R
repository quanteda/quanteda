#' Restore special tokens
#'
#' Compounds a sequence of tokens marked by special markers. The beginning and
#' the end of the sequence should be marked by U+E001 and U+E002 respectively.
#' @param x tokens object
#' @export
#' @keywords internal
tokens_restore <- function(x) {
    UseMethod("tokens_restore")
}

#' @export
tokens_restore.default <- function(x) {
    check_class(class(x), "tokens_restore")
}

#' @export
tokens_restore.tokens_xptr <- function(x) {
    type <- get_types(x)
    attrs <- attributes(x)
    result <- cpp_tokens_restore(x, 
                                 list(match("\uE001", type)), 
                                 list(match("\uE002", type)), 
                                 "")
    rebuild_tokens(result, attrs)
}

#' @export
tokens_restore.tokens <- function(x) {
    as.tokens(tokens_restore(as.tokens_xptr(x)))
}
