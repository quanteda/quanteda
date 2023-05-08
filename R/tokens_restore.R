#' Restore special tokens
#'
#' Compounds segments of tokens marked by special markers. The beginning and
#' the end of the segments should be marked by U+E001 and U+E002 respectively.
#' @param x tokens object
#' @returns a modified tokens object
#' @keywords internal tokens
tokens_restore <- function(x) {
    type <- types(x)
    attrs <- attributes(x)
    result <- qatd_cpp_tokens_restore(x, 
                                      list(match("\uE001", type)), 
                                      list(match("\uE002", type)), 
                                      type, "")
    rebuild_tokens(result, attrs)
}
