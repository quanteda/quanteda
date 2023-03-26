#' Restore special tokens
#'
#' Compounds segments of tokens marked by special markers. The beginning and
#' the end of the segments should be marked by U+E001 and U+E002 respectively.
#' @param x tokens object
#' @examples
#' txt <- c(d1 = "オリンピック延期決定！ #politics @abe #政治# #政治 #安部政権 @安部政権 ！")
#' txt <- stri_replace_all_regex(txt, "@[a-zA-Z0-9_]+|#[\\p{L}\\p{N}]+#?", "\uE001$0\uE002")
#' toks <- as.tokens(stri_split_boundaries(txt, type = "word"))
#' quanteda:::tokens_restore(toks)
tokens_restore <- function(x) {
    type <- types(x)
    attrs <- attributes(x)
    result <- qatd_cpp_tokens_restore(x, 
                                      list(match("\uE001", type)), 
                                      list(match("\uE002", type)), 
                                      type, "")
    rebuild_tokens(result, attrs)
}
