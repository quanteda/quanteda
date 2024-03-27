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
    
    left <- pattern2id("\uE001", type, valuetype = "fixed")
    right <- pattern2id("\uE002", type, valuetype = "fixed")
    if (!length(left) || !length(right)) 
        return(x)
    
    result <- cpp_tokens_restore(x, left, right, "", get_threads())
    rebuild_tokens(result, attrs)
}

#' @export
tokens_restore.tokens <- function(x) {
    as.tokens(tokens_restore(as.tokens_xptr(x)))
}
