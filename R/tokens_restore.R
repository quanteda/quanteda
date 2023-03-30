#' Restore special tokens
#'
#' Compounds segments of tokens marked by special markers. The beginning and
#' the end of the segments should be marked by U+E001 and U+E002 respectively.
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
                                 delim = "")
    rebuild_tokens(result, attrs)
}

#' @export
#' @examples
#' require(stringi)
#' txt <- c(d1 = "オリンピック延期決定！ #politics @abe #政治# #政治 #安倍政権 @安倍政権 ！")
#' txt <- stri_replace_all_regex(txt, "@[a-zA-Z0-9_]+|#[\\p{L}\\p{N}]+#?", "\uE001$0\uE002")
#' toks <- as.tokens(stri_split_boundaries(txt, type = "word"))
#' tokens_restore(toks)
tokens_restore.tokens <- function(x) {
    as.tokens(tokens_restore(as.tokens_xptr(x)))
}
